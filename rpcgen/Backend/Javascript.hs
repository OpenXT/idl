--
-- Copyright (c) 2012 Citrix Systems, Inc.
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
module Backend.Javascript where

import Data.List
import Data.Maybe
import qualified Data.Text.Lazy as T
import qualified DBus.Types as D
import Text.Printf
import Backend
import Tools
import Template
import Model

backend :: Backend
backend = Backend { genServer = genServer_
                  , genClient = genClient_ }

safeName :: String -> String
safeName "switch" = "switch_"
safeName "delete" = "delete_"
safeName "class" = "class_"
safeName "function" = "function_"
safeName s = s

clientMethodName :: (Interface, Method) -> String
clientMethodName (interface,method) =
    robbise (interfaceN ++ "_" ++ methodN)
  where
    -- make method name so Rob likes it
    robbise n  = safeName methodN
    interfaceN = replace "." "_" (interfaceName interface)
    methodN    = methodName method

clientStub :: (Interface, Method) -> String
clientStub (interface,method) =
    header ++ body
  where
    header :: String
    header = printf "  %s : function (%s%ssuccess_, failure_) {\n" method_name args_list sep
             where sep :: String
                   sep = if null args_list
                           then ""
                           else ", "
    method_name = clientMethodName (interface,method)
    args_list = join ", " . map ((++"_").parameterName) $ (methodInParams method)
    body =    printf "    var args_ = [%s];\n" args_list
           ++ printf "    XUIDBus.sendMessage(service_name_, \"%s\", object_path_, \"%s\", args_, success_, failure_);\n" call_method call_interface
           ++ "  }"
    call_interface = interfaceName interface
    call_method = methodName method

interfaceStubName :: Interface -> String
interfaceStubName intf =
    "this." ++ name
    where
      name = interfaceName intf

interfaceText :: Interface -> String
interfaceText intf = indent 2 $
    "Namespace(\"" ++ interfaceName intf ++ "\", this);\n" ++
    interfaceStubName intf ++ " =\n{\n"
    ++ meta ++ proxies ++ properties_decl ++ rwproperties_decl ++ "\n};\n"
    where
      meta            = indent 4 $ "interface_name : \"" ++ interfaceName intf ++ "\",\n" ++
                                   "get_property : function (name_, success_, failure_) {\n" ++
                                   "  return self.org.freedesktop.DBus.Properties.Get(\"" ++ interfaceName intf ++ "\", name_, success_, failure_);\n"  ++
                                   "},\n" ++
                                   "get_all_properties : function (success_, failure_) {\n" ++
                                   "  return self.org.freedesktop.DBus.Properties.GetAll(\"" ++ interfaceName intf ++ "\", success_, failure_);\n"  ++
                                   "},\n" ++
                                   "set_property : function (name_, value_, success_, failure_) {\n" ++
                                   "  return self.org.freedesktop.DBus.Properties.Set(\"" ++ interfaceName intf ++ "\", name_, value_, success_, failure_);\n"  ++
                                   "},\n"
      proxies         = indent 4 $ join ",\n" $ all_stubs
      all_stubs       = method_stubs
      rwproperties_decl = indent 4 $ "\n, properties_rw : [" ++ rwproperty_names ++ "]"
                        where comma | null all_stubs = ""
                                    | otherwise = ","
      properties_decl = indent 4 $ comma ++ "\nproperties : [" ++ property_names ++ "]"
                        where comma | null all_stubs = ""
                                    | otherwise = ","
      property_names  = join "," . map quote . map property_name $ properties
      rwproperty_names= join "," . map quote . map property_name $ rwproperties
      properties      = propertiesFromInterfaceSet [intf]
      rwproperties    = filter (\(_,p) -> propertyAccess p == ReadWrite) properties
      
      property_name (_,p) = propertyName p
      method_stubs   = map (\m -> clientStub (intf,m)) (interfaceMethods intf)

propertyAccessMethods :: Interface -> [(Interface,Method)]
propertyAccessMethods intf =
    concatMap getaccessors $ propertiesFromInterfaceSet [intf]
    where
      getaccessors (intf,prop) = map (\a -> (intf,a)) $ propertyAccessors prop

groupInterfaceText :: Input -> String
groupInterfaceText input =
    shortcuts_texts
  where {-
    intfs_texts = join "\n" $ map intf_text intfs
    intf_name i = replace "." "_" (interfaceName i)
    intf_text i = indent 2 $ "this." ++ intf_name i ++ " = " ++ interfaceStubName i ++ ";" -}
    intfs       = nub $ interfaces model ++ [orgFreedesktopDBusProperties]
    model       = fromJust . fromXML $ T.pack (xml input)

    intf_methods intf = map (\m -> (intf,m)) (interfaceMethods intf)

    methods = concat . map intf_methods $ intfs
    shortcuts_texts =
        aux methods []
        where
          aux [] _ = "\n"
          aux ((intf,method):ms) done
              | method `elem` done = aux ms done
              | otherwise =
                  shortcut_text (intf,method) ++ "\n" ++
                  aux ms (method : done)
          shortcut_text (i,m) =
              "  this." ++ methname ++ " = this." ++ iname i  ++ "." ++ methname ++ ";"
              where methname = safeName $ methodName m
          iname i = interfaceName i
              
interfaceNamesArr :: [Interface] -> String
interfaceNamesArr intfs = join ", " . map ref $ intfs where
    ref intf = quote $ interfaceName intf

interfaceRefs :: [Interface] -> String
interfaceRefs intfs = join ", " . map ref $ intfs where
    ref intf = ("this."++) . replace "." "_" $ interfaceName intf

enumValueStr :: D.Type -> String -> String
enumValueStr D.DBusBoolean "true"  = "true"
enumValueStr D.DBusBoolean "false" = "false"
enumValueStr D.DBusBoolean s  = error $ "bad boolean enum string: " ++ show s
enumValueStr D.DBusByte    s  = s
enumValueStr D.DBusInt16   s  = s
enumValueStr D.DBusInt32   s  = s
enumValueStr D.DBusInt64   s  = s
enumValueStr D.DBusWord16  s  = s
enumValueStr D.DBusWord32  s  = s
enumValueStr D.DBusWord64  s  = s
enumValueStr D.DBusDouble  s  = s
enumValueStr D.DBusString  s  = (quote s)
enumValueStr t _ = error $ "unsupported enum type: " ++ show t

declareEnum :: String -> Enumeration -> String
declareEnum oname e =
    unlines
    [ printf "XenClient.Resource.%s.%s = {" oname (enumName e)
    , indent 4 values
    ,        "};"
    ]
    where
      values = join ",\n" . map value $ enumValues e
      value v = printf "%s : %s" (enumSuffix v) (enumValueStr (enumType e) (enumValue v))
 
-- name of file where we are outputting client staff
outputClientFile :: Input -> String
outputClientFile input =
    decapitalise $ decamelise (objectname input) ++ "_client.js"

genObjectName :: Input -> String
genObjectName input =
    camelise ( objectname input ++ "Client" )

genClient_ :: Input -> IO Output
genClient_ input =
    return [ (outputClientFile input, contents) ]
  where
    contents    = substRules rules (getTemplate "template_client.js" input)
    rules       = [ ("@OBJECT_NAME@", genObjectName input)
                  , ("@SHORT_OBJECT_NAME@", soname)
                  , ("@CONSTANTS@", join "\n" . map (declareEnum soname) $ enums model)
                  , ("@INTERFACE_PROXIES@", proxies )
                  , ("@INTERFACE_STUBS@", "") --groupInterfaceText input)
                  , ("@INTERFACE_NAMES@", interfaceNamesArr intfs)
                  , ("@INTERFACE_REFS@", interfaceRefs intfs)
                  ]
    soname      = capitalise $ objectname input
    proxies     = join "\n" $ map interfaceText intfs
    intfs       = nub $ interfaces model ++ [orgFreedesktopDBusProperties]
    model       = fromJust . fromXML $ T.pack (xml input)

genServer_ :: Input -> IO Output
genServer_ input = error "server mode not supported by javascript backend"
