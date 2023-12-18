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
import qualified Data.Text as T
import qualified Data.Map as M
import qualified DBus.Internal.Types as D
import qualified DBus.Introspection.Types as I
import qualified DBus.Introspection.Parse as P
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

paramType :: I.MethodArg -> D.Type
paramType (I.MethodArg _ typ _) = typ

paramTypes :: [I.MethodArg] -> [D.Type]
paramTypes = map paramType

paramName :: I.MethodArg -> String
paramName (I.MethodArg n _ _) = n

filterInParams :: [I.MethodArg] -> [I.MethodArg]
filterInParams params = filter (\p -> I.methodArgDirection p == I.In) params

filterOutParams :: [I.MethodArg] -> [I.MethodArg]
filterOutParams params = filter (\p -> I.methodArgDirection p == I.Out) params

typeSig :: D.Type -> String
typeSig D.TypeBoolean = "DBus.SigBool"
typeSig D.TypeWord8 = "DBus.SigByte"
typeSig D.TypeInt16 = "DBus.SigInt16"
typeSig D.TypeInt32 = "DBus.SigInt32"
typeSig D.TypeInt64 = "DBus.SigInt64"
typeSig D.TypeWord16 = "DBus.SigUInt16"
typeSig D.TypeWord32 = "DBus.SigUInt32"
typeSig D.TypeWord64 = "DBus.SigUInt64"
typeSig D.TypeDouble = "DBus.SigDouble"
typeSig D.TypeString = "DBus.SigString"
typeSig D.TypeObjectPath = "DBus.SigObjectPath"
typeSig D.TypeSignature = "DBus.SigString"
typeSig D.TypeVariant = "DBus.SigVariant"
typeSig (D.TypeArray      elemT)      = "DBus.SigArray ("  ++ typeSig elemT ++ ")"
typeSig (D.TypeStructure  elemTs)     = "DBus.SigStruct [" ++ (concat . intersperse ";" $ map typeSig elemTs) ++ "]"
typeSig (D.TypeDictionary keyT elemT) = "DBus.SigDict (("  ++ typeSig keyT ++ "),(" ++ typeSig elemT ++ "))"

arrayConstructor :: D.Type -> String -> String
arrayConstructor elemT var_name =
    cons elemT
  where
    n = var_name
    cons D.TypeBoolean = "DBus.Bools " ++ n
    cons D.TypeWord8    = "DBus.Bytes " ++ n
    cons D.TypeInt16   = "DBus.Int16s " ++ n
    cons D.TypeInt32   = "DBus.Int32s " ++ n
    cons D.TypeInt64   = "DBus.Int64s " ++ n
    cons D.TypeWord16  = "DBus.UInt16s " ++ n
    cons D.TypeWord32  = "DBus.UInt32s " ++ n
    cons D.TypeWord64  = "DBus.UInt64s " ++ n
    cons D.TypeDouble  = "DBus.Doubles " ++ n
    cons D.TypeString  = "DBus.Strings " ++ n
    cons D.TypeObjectPath = "DBus.Strings " ++ n
    cons D.TypeSignature  = "DBus.Strings " ++ n
    cons D.TypeVariant    = "DBus.Variants " ++ n
    cons (D.TypeStructure types) = printf "DBus.Structs (%s,%s)" siglist n
                                   where siglist = concat . intersperse ";" . map typeSig $ types
    cons (D.TypeArray elemT) = printf "DBus.Arrays (%s,%s)" (typeSig elemT) subarrays
                               where subarrays = n
    cons (D.TypeDictionary keyT elemT) = printf "DBus.Dicts ((%s,%s),%s)" (typeSig keyT) (typeSig elemT) n

typeConstructor :: D.Type -> String ->  String
typeConstructor typ var_name =
    cons typ
  where
    n = var_name

    cons D.TypeBoolean = "DBus.Bool " ++ n
    cons D.TypeWord8    = "DBus.Byte " ++ n
    cons D.TypeInt16   = "DBus.Int16 " ++ n
    cons D.TypeInt32   = "DBus.Int32 " ++ n
    cons D.TypeInt64   = "DBus.Int64 " ++ n
    cons D.TypeWord16  = "DBus.UInt16 " ++ n
    cons D.TypeWord32  = "DBus.UInt32 " ++ n
    cons D.TypeWord64  = "DBus.UInt64 " ++ n
    cons D.TypeDouble  = "DBus.Double " ++ n
    cons D.TypeString  = "DBus.String " ++ n
    cons D.TypeObjectPath = "DBus.ObjectPath " ++ n
    cons D.TypeSignature  = "DBus.String "  ++ n
    cons D.TypeVariant    = "DBus.Variant " ++ n
    cons (D.TypeStructure types)
        = "DBus.Struct " ++ n
    cons (D.TypeArray elemT)
        = "DBus.Array " ++ "(" ++ arrayConstructor elemT n ++ ")"
    cons t@(D.TypeDictionary keyT elemT)
        = "DBus.Array " ++ "(" ++ arrayConstructor t n ++ ")"

typeConstructor' :: I.MethodArg -> String
typeConstructor' p =  typeConstructor (paramType p) (paramName p)

handlerStubs :: String -> I.Interface -> [String]
handlerStubs object (I.Interface iname methods _ _) =
    map stub methods
  where
    stub (I.Method name params) =
        let mname       = D.formatMemberName name
            call_module = capitalise . decamelise $ object ++ "Methods"
            call_name   = replace "." "_" (interface ++ "_" ++ mname)
            call_args   = concat . intersperse " " . ("msg" :) . map get_pname $ (filterInParams params)
        in

        unlines $ [ printf "\t\t\t| \"%s\", \"%s\", args -> (try " interface mname
                  , printf "\t\t\t\tlet [%s] = args in" (concat . intersperse "; " . map typeConstructor' $ (filterInParams params))
                  , printf "\t\t\t\tlet (%s) = %s.%s %s in" (outvars (filterOutParams params)) call_module call_name call_args
                  , "\t\t\t\tlet reply = DBus.Message.new_method_return msg in"
                  ]
                    ++ reply_appends (nameSequence (filterOutParams params))
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
              let (I.MethodArg _ typ _) = p in
              I.MethodArg ("out_" ++ show id) typ I.Out : aux (id+1) ps
                               
    reply_appends params = map append params
    append param  = "\t\t\t\tDBus.Message.append reply [" ++ typeConstructor' param ++ "];"

    interface = D.formatInterfaceName iname
    get_pname (I.MethodArg pname _ _) = pname

callStubs :: String -> I.Interface -> [String]
callStubs object (I.Interface iname methods _ _) =
    map stub methods
  where
    stub (I.Method name params) =
        let mname       = D.formatMemberName name
            stub_name   = replace "." "_" (interface ++ "_" ++ mname)
            stub_args   = concat . intersperse " " . map paramName $ (filterInParams params)
            call_args   = concat . intersperse "; " . map typeConstructor' $ (filterInParams params)
            out_cons    = concat . intersperse "; " $ outConstructors (filterOutParams params)
            out_values  = concat . intersperse ", " $ take (length (filterOutParams params)) outnames
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

    interface = D.formatInterfaceName iname
        
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
    interface_name (I.Interface n _ _ _) = D.formatInterfaceName n
    interface = head interfaces
    interfaces = let Just intro_obj = P.parseXML "/" (T.pack (xml input))
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
    interfaces = let Just intro_obj = P.parseXML "/" (T.pack (xml input))
                     (I.Object _ ifs _) = intro_obj
                 in
                   ifs
