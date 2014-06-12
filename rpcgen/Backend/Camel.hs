--
-- Copyright (c) 2010 Citrix Systems, Inc.
-- 
-- This program is free software; you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation; either version 2 of the License, or
-- (at your option) any later version.
-- 
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
-- 
-- You should have received a copy of the GNU General Public License
-- along with this program; if not, write to the Free Software
-- Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
--

{-# LANGUAGE OverloadedStrings #-}
module Backend.Camel where

import Data.List
import qualified Data.Text.Lazy as T
import qualified Data.Map as M
import qualified DBus.Types as D
import qualified DBus.Introspection as I
import Text.Printf
import Backend
import Tools
import Template

backend :: Backend
backend = Backend { genServer = genServer_
                  , genClient = genClient_ }

outputServerFile object = object ++ "_server.ml"
serverMLFileTemplate = "template_server.ml"
outputClientFile object = object ++ "_client.ml"
clientMLFileTemplate = "template_client.ml"

lookupTemplate :: String -> Input -> String
lookupTemplate name input =
    case M.lookup name (templates input) of
      Nothing -> error $ "No such template: " ++ name
      Just c  -> c

paramType :: I.Parameter -> D.Type
paramType (I.Parameter _ sig) = head $ D.signatureTypes sig

paramTypes :: [I.Parameter] -> [D.Type]
paramTypes = map paramType

paramName :: I.Parameter -> String
paramName (I.Parameter n _) = T.unpack n

typeSig :: D.Type -> String
typeSig D.DBusBoolean = "DBus.SigBool"
typeSig D.DBusByte = "DBus.SigByte"
typeSig D.DBusInt16 = "DBus.SigInt16"
typeSig D.DBusInt32 = "DBus.SigInt32"
typeSig D.DBusInt64 = "DBus.SigInt64"
typeSig D.DBusWord16 = "DBus.SigUInt16"
typeSig D.DBusWord32 = "DBus.SigUInt32"
typeSig D.DBusWord64 = "DBus.SigUInt64"
typeSig D.DBusDouble = "DBus.SigDouble"
typeSig D.DBusString = "DBus.SigString"
typeSig D.DBusObjectPath = "DBus.SigObjectPath"
typeSig D.DBusSignature = "DBus.SigString"
typeSig D.DBusVariant = "DBus.SigVariant"
typeSig (D.DBusArray      elemT)      = "DBus.SigArray ("  ++ typeSig elemT ++ ")"
typeSig (D.DBusStructure  elemTs)     = "DBus.SigStruct [" ++ (concat . intersperse ";" $ map typeSig elemTs) ++ "]"
typeSig (D.DBusDictionary keyT elemT) = "DBus.SigDict (("  ++ typeSig keyT ++ "),(" ++ typeSig elemT ++ "))"

arrayConstructor :: D.Type -> String -> String
arrayConstructor elemT var_name =
    cons elemT
  where
    n = var_name
    cons D.DBusBoolean = "DBus.Bools " ++ n
    cons D.DBusByte    = "DBus.Bytes " ++ n
    cons D.DBusInt16   = "DBus.Int16s " ++ n
    cons D.DBusInt32   = "DBus.Int32s " ++ n
    cons D.DBusInt64   = "DBus.Int64s " ++ n
    cons D.DBusWord16  = "DBus.UInt16s " ++ n
    cons D.DBusWord32  = "DBus.UInt32s " ++ n
    cons D.DBusWord64  = "DBus.UInt64s " ++ n
    cons D.DBusDouble  = "DBus.Doubles " ++ n
    cons D.DBusString  = "DBus.Strings " ++ n
    cons D.DBusObjectPath = "DBus.Strings " ++ n
    cons D.DBusSignature  = "DBus.Strings " ++ n
    cons D.DBusVariant    = "DBus.Variants " ++ n
    cons (D.DBusStructure types) = printf "DBus.Structs (%s,%s)" siglist n
                                   where siglist = concat . intersperse ";" . map typeSig $ types
    cons (D.DBusArray elemT) = printf "DBus.Arrays (%s,%s)" (typeSig elemT) subarrays
                               where subarrays = n
    cons (D.DBusDictionary keyT elemT) = printf "DBus.Dicts ((%s,%s),%s)" (typeSig keyT) (typeSig elemT) n

typeConstructor :: D.Type -> String ->  String
typeConstructor typ var_name =
    cons typ
  where
    n = var_name

    cons D.DBusBoolean = "DBus.Bool " ++ n
    cons D.DBusByte    = "DBus.Byte " ++ n
    cons D.DBusInt16   = "DBus.Int16 " ++ n
    cons D.DBusInt32   = "DBus.Int32 " ++ n
    cons D.DBusInt64   = "DBus.Int64 " ++ n
    cons D.DBusWord16  = "DBus.UInt16 " ++ n
    cons D.DBusWord32  = "DBus.UInt32 " ++ n
    cons D.DBusWord64  = "DBus.UInt64 " ++ n
    cons D.DBusDouble  = "DBus.Double " ++ n
    cons D.DBusString  = "DBus.String " ++ n
    cons D.DBusObjectPath = "DBus.ObjectPath " ++ n
    cons D.DBusSignature  = "DBus.String "  ++ n
    cons D.DBusVariant    = "DBus.Variant " ++ n
    cons (D.DBusStructure types)
        = "DBus.Struct " ++ n
    cons (D.DBusArray elemT)
        = "DBus.Array " ++ "(" ++ arrayConstructor elemT n ++ ")"
    cons t@(D.DBusDictionary keyT elemT)
        = "DBus.Array " ++ "(" ++ arrayConstructor t n ++ ")"

typeConstructor' :: I.Parameter -> String
typeConstructor' p =  typeConstructor (paramType p) (paramName p)

handlerStubs :: String -> I.Interface -> [String]
handlerStubs object (I.Interface iname methods _ _) =
    map stub methods
  where
    stub (I.Method name inparams outparams) =
        let mname       = T.unpack $ D.strMemberName name
            call_module = capitalise . decamelise $ object ++ "Methods"
            call_name   = replace "." "_" (interface ++ "_" ++ mname)
            call_args   = concat . intersperse " " . ("msg" :) . map get_pname $ inparams
        in

        unlines $ [ printf "\t\t\t| \"%s\", \"%s\", args -> (try " interface mname
                  , printf "\t\t\t\tlet [%s] = args in" (concat . intersperse "; " . map typeConstructor' $ inparams)
                  , printf "\t\t\t\tlet (%s) = %s.%s %s in" (outvars outparams) call_module call_name call_args
                  , "\t\t\t\tlet reply = DBus.Message.new_method_return msg in"
                  ]
                    ++ reply_appends (nameSequence outparams)
                    ++ [ "\t\t\t\treply"
                       , "\t\t\t\t with Match_failure _ -> DBus.Message.new_error msg DBus.ERR_INVALID_SIGNATURE \"invalid arguments\""
                       , "\t\t\t\t    | Failure s -> DBus.Message.new_error msg DBus.ERR_FAILED s"
                       , "\t\t\t\t    | _ -> DBus.Message.new_error msg DBus.ERR_FAILED \"exception occured\")"
                       ]

    outvars pms = concat . intersperse "," $ map get_pname (nameSequence pms)

    nameSequence pms = aux 1 pms
        where 
          aux _ []      = []
          aux id (p:ps) =
              let (I.Parameter _ sig) = p in
              I.Parameter (T.pack $ "out_" ++ show id) sig : aux (id+1) ps
                               
    reply_appends params = map append params
    append param  = "\t\t\t\tDBus.Message.append reply [" ++ typeConstructor' param ++ "];"

    interface = T.unpack $ D.strInterfaceName iname
    get_pname (I.Parameter pname _) = T.unpack pname

callStubs :: String -> I.Interface -> [String]
callStubs object (I.Interface iname methods _ _) =
    map stub methods
  where
    stub (I.Method name inparams outparams) =
        let mname       = T.unpack $ D.strMemberName name
            stub_name   = replace "." "_" (interface ++ "_" ++ mname)
            stub_args   = concat . intersperse " " . map paramName $ inparams
            call_args   = concat . intersperse "; " . map typeConstructor' $ inparams
            out_cons    = concat . intersperse "; " $ outConstructors outparams
            out_values  = concat . intersperse ", " $ take (length outparams) outnames
        in
        unlines $ [ printf "let %s ?(timeout=(-1)) service_ obj_path_ %s = " stub_name stub_args
                  , printf "\tlet reply = call_dbus ~timeout ~service:service_ ~obj:obj_path_ ~interface:\"%s\" ~member:\"%s\" ~args:[ %s ] in" interface mname call_args
                  , printf "\tmatch Message.get reply with"
                  , printf "\t| [ %s ] -> (%s)" out_cons out_values
                  , printf "\t| _ -> failwith \"unexpected reply\""
                  ]

    indices  = [1..]
    outnames = map name indices
               where name i = "out_" ++ show i

    outConstructors pms = map constructor (zip (paramTypes pms) outnames)
                          where constructor (typ,name) = typeConstructor typ name

    interface = T.unpack $ D.strInterfaceName iname
        
genServer_ :: Input -> IO Output
genServer_ input =
    return [ (outputServerFile object, server_contents) ]
  where
    object = objectname input
    server_contents =
        substRules [ ("@QUOTED_INTROSPECT_XML@", quoted_introspect_xml)
                   , ("@INTERFACE@", interface_name interface)
                   , ("@METHOD_HANDLER_STUBS@", handlers)
                   , ("@object@", object)
                   ]
                   $ lookupTemplate serverMLFileTemplate input

    quoted_introspect_xml = replace "\"" "\\\"" (xml input)
    interface_name (I.Interface n _ _ _) = T.unpack $ D.strInterfaceName n
    interface = head interfaces
    interfaces = let Just intro_obj = I.fromXML "/" (T.pack (xml input))
                     (I.Object _ ifs _) = intro_obj
                 in
                   ifs
    handlers = concat . intersperse "\n" $ handlerStubs object interface

genClient_ :: Input -> IO Output
genClient_ input =
    return [ (outputClientFile object, client_contents) ]
  where
    object = objectname input
    client_contents =
        substRules [ ("@STUBS@", stubs) ]
        $ lookupTemplate clientMLFileTemplate input
    stubs = concat . intersperse "\n" . concat . map (callStubs object) $ interfaces
    interfaces = let Just intro_obj = I.fromXML "/" (T.pack (xml input))
                     (I.Object _ ifs _) = intro_obj
                 in
                   ifs
