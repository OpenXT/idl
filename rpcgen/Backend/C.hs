--
-- Copyright (c) 2013 Citrix Systems, Inc.
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
module Backend.C (backend) where

import Data.List
import Data.Char
import Data.Maybe
import qualified Data.Text.Lazy as T
import qualified Data.Map as M

import Control.Monad hiding (join)
import System.IO
import System.Exit
import qualified DBus.Types as D
import Text.Printf
import Backend
import Template
import Tools
import Model

backend :: Backend
backend = Backend { genServer = genServer_
                  , genClient = genClient_  }
  

-- template & generated file names
clientHFileTemplate            = "template_client.h"
objHFileTemplate               = "template_obj_header.h"
objCFileTemplate               = "template_obj_imp.c"
outputObjHFile      object     = object ++ "_server_obj.h"
outputObjCFile      object     = object ++ "_server_obj.c"
outputMarshallHFile object     = object ++ "_server_marshall.h"
outputClientHFile   object     = object ++ "_client.h"

-- template substitution rules for given object name
basicRules :: String -> [Rule]
basicRules object =
    [ ("@OBJECT@"            , map toUpper object)
    , ("@Object@"            , capitalise object)
    , ("@object@"            , object)
    , ("@GLIB_OBJ@"          , camelise object ++ "Object")
    , ("@GLIB_CLASS@"        , camelise object ++ "ObjectClass")
    , ("@OBJECT_HEADER_FILE@", outputObjHFile object)
    , ("@MARSHALL_FILE@"     , outputMarshallHFile object)
    ]

lookupTemplate :: String -> Input -> String
lookupTemplate name input =
    case M.lookup name (templates input) of
      Nothing -> error $ "No such template: " ++ name
      Just c  -> c

fixedType :: D.Type -> Bool
fixedType D.DBusBoolean = True
fixedType D.DBusByte    = True
fixedType D.DBusInt16   = True
fixedType D.DBusInt32   = True
fixedType D.DBusInt64   = True
fixedType D.DBusWord16  = True
fixedType D.DBusWord32  = True
fixedType D.DBusWord64  = True
fixedType D.DBusDouble  = True
fixedType _             = False

gtypeFromDBusType :: D.Type -> String
gtypeFromDBusType D.DBusBoolean    = "gboolean"
gtypeFromDBusType D.DBusByte       = "guchar"
gtypeFromDBusType D.DBusInt16      = "gint"
gtypeFromDBusType D.DBusInt32      = "gint"
gtypeFromDBusType D.DBusInt64      = "gint64"
gtypeFromDBusType D.DBusWord16     = "guint"
gtypeFromDBusType D.DBusWord32     = "guint"
gtypeFromDBusType D.DBusWord64     = "guint64"
gtypeFromDBusType D.DBusDouble     = "gdouble"
gtypeFromDBusType D.DBusString     = "char*"
gtypeFromDBusType D.DBusSignature  = "char*"
gtypeFromDBusType D.DBusObjectPath = "char*"
gtypeFromDBusType D.DBusVariant    = "GValue*"
gtypeFromDBusType (D.DBusArray D.DBusString) = "char**"
gtypeFromDBusType (D.DBusArray t)
                  | fixedType t = "GArray*"
                  | otherwise   = "GPtrArray*"
gtypeFromDBusType (D.DBusStructure _ )   = "GValueArray*"
gtypeFromDBusType (D.DBusDictionary _ _) = "GHashTable*"

dbusTypeTag :: D.Type -> String
dbusTypeTag D.DBusBoolean = "DBUS_TYPE_BOOLEAN"
dbusTypeTag D.DBusString = "DBUS_TYPE_STRING"
dbusTypeTag D.DBusInt32  = "DBUS_TYPE_INT32"
dbusTypeTag D.DBusInt64  = "DBUS_TYPE_INT64"
dbusTypeTag D.DBusWord32 = "DBUS_TYPE_UINT32"
dbusTypeTag D.DBusWord64 = "DBUS_TYPE_UINT64"
dbusTypeTag D.DBusDouble = "DBUS_TYPE_DOUBLE"
dbusTypeTag D.DBusByte   = "DBUS_TYPE_BYTE"
dbusTypeTag t            = error $ "not supporting dbus type " ++ show t

cargsFromDBusMethod :: Method -> String
cargsFromDBusMethod (Method name inargs outargs) =
    (concat . intersperse ", ") $ map (makeConst . format "IN_") inargs 
                               ++ map (format "*OUT_") outargs
  where
    format prefix (Parameter name typ) =
        gtypeFromDBusType typ ++ " " ++ prefix ++ name

    makeConst str
        | "char*" `elem` inits str  = "const " ++ str
        | otherwise                 = str

-- properties
propertyIDName :: Interface -> String -> String
propertyIDName i = (("PROP_" ++ capIname ++ "_") ++) .  map toUpper . replace "-" "_"
  where
    capIname = map toUpper . replace "." "_" $ interfaceName i

propertyIDName' i p = propertyIDName i (propertyName p)

enumPropertiesBody :: [Interface] -> String
enumPropertiesBody = unlines . map (++ ",") . concat . map enumI where
  enumI face = map (enumP face) $ interfaceProperties face
  enumP face p = propertyIDName' face p
  
data GParamSpecFlag = FreeRequired deriving (Eq,Show)

data GParamSpec = GPS { gpsFunType :: String
                      , gpsSpec :: String
                      , gpsFlags :: [GParamSpecFlag] }

gParamSpec :: D.Type -> GParamSpec
gParamSpec = g where
  g D.DBusBoolean = GPS "boolean" "FALSE" []
  g D.DBusInt32 = GPS "int" "INT_MIN, INT_MAX, 0" []
  g D.DBusWord32 = GPS "uint" "0, UINT_MAX, 0" []
  g D.DBusString = GPS "string" "\"\"" [FreeRequired]
  g _ = error "unsupported property type"

gParamAccess :: Access -> String
gParamAccess Read = "G_PARAM_READ"
gParamAccess Write = "G_PARAM_WRITE"
gParamAccess ReadWrite = "G_PARAM_READWRITE"

initPropertiesBody :: [Interface] -> String
initPropertiesBody = unlines . concat . map initI where
  initI face = map (initP face) $ interfaceProperties face
  initP face p = let GPS typ spec _ = gParamSpec (propertyType p) in
    printf "obj_properties[%s] = g_param_spec_%s (\"%s\", \"\", \"\", %s, %s);"
      (propertyIDName' face p)
      typ
      (propertyName p)
      spec
      (gParamAccess (propertyAccess p))

readable p = propertyAccess p `elem` [Read,ReadWrite]
writable p = propertyAccess p `elem` [Write,ReadWrite]

valueFunSuffix :: Property -> String
valueFunSuffix = gpsFunType . gParamSpec . propertyType

freeReq = (FreeRequired `elem`) . gpsFlags . gParamSpec . propertyType

setPropertyBody :: String -> [Interface] -> String
setPropertyBody objname = unlines . concat . map bodyI where
  bodyI face = map (bodyP face) $ interfaceProperties face
  bodyP face p | writable p =
    printf "case %s: { GError *err = NULL; %s v = (%s) g_value_get_%s(value); %s (self, v, &err); break; }"
      (propertyIDName' face p)
      (gtypeFromDBusType (propertyType p))
      (gtypeFromDBusType (propertyType p))
      (valueFunSuffix p)
      (setterName objname face p)
  bodyP _ _ = ""

setterName objname face p = objname ++ "_property_set_" ++ (replace "-" "_" name)
  where name = propertyName p

getPropertyBody :: String -> [Interface] -> String
getPropertyBody objname = unlines . concat . map bodyI where
  bodyI face = map (bodyP face) $ interfaceProperties face
  bodyP face p | readable p =
    printf "case %s: { GError *err = NULL; %s v; %s (self, &v, &err); g_value_set_%s(value, v); %s; break; }"
      (propertyIDName' face p)
      (gtypeFromDBusType (propertyType p))
      (getterName objname face p)
      (valueFunSuffix p)
      ((if freeReq p then "free(v)" else "") :: String)
  bodyP _ _ = ""

getterName objname face p = objname ++ "_property_get_" ++ (replace "-" "_" name)
  where name = propertyName p

--FIXME: add parameters support
signalStub :: String -> (Interface, Signal) -> String
signalStub objname (intf,signal) =
    unlines [ "static inline gboolean " ++ function_name ++ "(xcdbus_conn_t *_conn, const char *_service, const char *_obj_path " ++ arg_decls ++") {"
            , "  DBusConnection *c = xcdbus_get_dbus_connection(xcdbus_of_conn(_conn)); "
            , "  DBusMessage *msg = dbus_message_new_signal (_obj_path, " ++ quote iname ++ ", " ++ quote signame ++ ");"
            , "  if (!msg) { return FALSE; } "
            , (if length args >0
                 then "  dbus_message_append_args(msg, " ++ call_args ++ ", DBUS_TYPE_INVALID);"
                 else "")
            , "  dbus_connection_send(c, msg, NULL); dbus_connection_flush(c);\n"
            , "  dbus_message_unref(msg);"
            , "  return TRUE;"
            , "}"
            ]
    where
      function_name = replace "." "_" $ "notify_" ++ (interfaceName intf) ++ "_" ++ (signalName signal)
      iname         = interfaceName intf
      signame       = signalName signal

      args = map make_arg (signalParams signal)

      arg_decls = prefix (concat . intersperse ", " $ map arg_decl args)
                  where prefix "" = ""
                        prefix s  = ", " ++ s

      call_args = concat . intersperse ", " $ map arg_pass_to_call args

      constchar "char*" = "const char*"
      constchar x       = x
      arg_decl (name, _, gtype)       = constchar gtype ++ " " ++ name
      arg_pass_to_call (name, tag, _) = tag ++ ",&" ++ name

      make_arg (Parameter name t) =
          let name' = name ++ "_" in
          ( name'
          , dbusTypeTag t
          , gtypeFromDBusType t )

-- generate server bindings from given xml
genServer_ :: Input -> IO Output
genServer_ input =
    withTempFile "xml-intf" $ \(name,handle) ->
        do hPutStr handle (xml input)
           hFlush handle
           putStrLn $ "invoking: " ++ cmdBT name
           bt_out <- spawnShell' (cmdBT name)
           when (bt_out == Nothing) $
                do hPutStrLn stderr "dbus-binding-tool FAILED"
                   exitWith (ExitFailure 1)
           let Just marshall_contents = bt_out

           return [
             -- output marshalling file got from binding tool
               (outputMarshallHFile object, marshall_contents)
             -- output a header with convenience object declarations as well + required prototypes
             , (outputObjHFile object, objh_contents)
             -- and corresponding implementation with some initialisation stuff
             , (outputObjCFile object, objc_contents) ]

  where
    object  = objectname input -- this corresponds to glib OBJECT name
    cmdBT n = "dbus-binding-tool --mode=glib-server --prefix=" ++ object ++ " " ++ n

    objh_contents =
        substRules rules (lookupTemplate objHFileTemplate input)
        where rules = ("@PROTOTYPES@", prototypes)
                      : ("@SIGNALS@",  signalStubs)
                      : ("@CONSTANTS@", join "\n" . map declareEnum . enums $ model)
                      : basicRules object

    objc_contents =
        substRules rules (lookupTemplate objCFileTemplate input)
        where rules = ("@enum_properties@", enumPropertiesBody (interfaces model))
                      : ("@init_properties@", initPropertiesBody (interfaces model))
                      : ("@get_property_body@", getPropertyBody object (interfaces model))
                      : ("@set_property_body@", setPropertyBody object (interfaces model))
                      : basicRules object

    model = fromJust $ fromXML (T.pack $ xml input)

    prototypes :: String
    prototypes =
        concat . intersperse "\n" $ map describeI (interfaces model)

    
    describeI :: Interface -> String
    describeI intf =
        concat . intersperse "\n" $    map describeM (interfaceMethods intf) 
                                    ++ filter (not . null) (map (describeP intf) (interfaceProperties intf))

    describeM :: Method -> String
    describeM m =
        let name = methodName m
            args =
                concat . intersperse ", " $ [this] 
                                         ++ ( case cargsFromDBusMethod m of
                                                     []  -> []
                                                     str -> [str]  )
                                         ++ ["GError**"]
        in
        "extern gboolean " ++ object ++ "_" ++ (decamelise name) ++ "(" ++ args ++ ");"

    describeP :: Interface -> Property -> String
    describeP face p =
       unlines $ filter (not . null ) $ concat [ getter, setter ]
       where
         getter | readable p = [ getter_ ]
                | otherwise  = []
         setter | writable p = [ setter_ ]
                | otherwise  = []
         getter_ = "extern gboolean " ++ getterName object face p ++ "(" ++ this ++ ", " ++ typ ++ ", GError **);"
           where typ = gtypeFromDBusType (propertyType p) ++ "*"
         setter_ = "extern gboolean " ++ setterName object face p ++ "(" ++ this ++ ", " ++ typ ++ ", GError **);"
           where typ = gtypeFromDBusType (propertyType p)
    
    signals :: [ (Interface,Signal) ]
    signals = [ (intf,s) | intf <- interfaces model, s <- interfaceSignals intf ]

    signalStubs :: String
    signalStubs =
        concat . intersperse "\n" $ map (signalStub object) signals

    this = camelise object ++ "Object *this"

enumValueStr :: D.Type -> String -> String
enumValueStr D.DBusBoolean "true"  = "1"
enumValueStr D.DBusBoolean "false" = "0"
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

declareEnum :: Enumeration -> String
declareEnum e = values where
      values = join "\n" . map value $ enumValues e
      value v = printf "#define ENUM_%s_%s %s" (enumName e) (enumSuffix v) (enumValueStr (enumType e) (enumValue v))

getter_method D.DBusBoolean = Just "xcdbus_get_property_bool"
getter_method D.DBusByte    = Just "xcdbus_get_property_byte"
getter_method D.DBusString  = Just "xcdbus_get_property_string"
getter_method D.DBusInt32   = Just "xcdbus_get_property_int"
getter_method D.DBusWord32  = Just "xcdbus_get_property_uint"
getter_method D.DBusInt64   = Just "xcdbus_get_property_int64"
getter_method D.DBusWord64  = Just "xcdbus_get_property_uint64"
getter_method D.DBusDouble  = Just "xcdbus_get_property_double"
getter_method _ = Nothing -- other types unsupported

setter_method D.DBusBoolean = Just "xcdbus_set_property_bool"
setter_method D.DBusByte    = Just "xcdbus_set_property_byte"
setter_method D.DBusString  = Just "xcdbus_set_property_string"
setter_method D.DBusInt32   = Just "xcdbus_set_property_int"
setter_method D.DBusWord32  = Just "xcdbus_set_property_uint"
setter_method D.DBusInt64   = Just "xcdbus_set_property_int64"
setter_method D.DBusWord64  = Just "xcdbus_set_property_uint64"
setter_method D.DBusDouble  = Just "xcdbus_set_property_double"
setter_method _ = Nothing -- other types unsupported

glib_getter_type D.DBusBoolean = Just "gboolean"
glib_getter_type D.DBusByte    = Just "unsigned char"
glib_getter_type D.DBusString  = Just "char*"
glib_getter_type D.DBusInt32   = Just "gint"
glib_getter_type D.DBusWord32  = Just "guint"
glib_getter_type D.DBusInt64   = Just "gint64"
glib_getter_type D.DBusWord64  = Just "guint64"
glib_getter_type D.DBusDouble  = Just "double"
glib_getter_type _ = Nothing -- other types unsupported

glib_setter_type D.DBusBoolean = Just "gboolean"
glib_setter_type D.DBusByte    = Just "unsigned char"
glib_setter_type D.DBusString  = Just "const char*"
glib_setter_type D.DBusInt32   = Just "gint"
glib_setter_type D.DBusWord32  = Just "guint"
glib_setter_type D.DBusInt64   = Just "gint64"
glib_setter_type D.DBusWord64  = Just "guint64"
glib_setter_type D.DBusDouble  = Just "double"
glib_setter_type _ = Nothing -- other types unsupported

-- generate client bindings from given xml
genClient_ :: Input -> IO Output
genClient_ input =
    withTempFile "xml-intf" $ \(name,handle) ->
        do hPutStr handle (xml input)
           hFlush handle
           putStrLn $ "invoking: " ++ cmdBT name
           bt_out <- spawnShell' (cmdBT name)
           when (bt_out == Nothing) $
                do hPutStrLn stderr "dbus-binding-tool FAILED"
                   exitWith (ExitFailure 1)
           let Just bt_contents = bt_out
           return [ (outputClientHFile object, clienth_contents bt_contents) ]
  where
    object   = objectname input -- this corresponds to glib OBJECT name
    cmdBT n  = "dbus-binding-tool --mode=glib-client " ++ n

    clienth_contents bt_contents =
        substRules rules (lookupTemplate clientHFileTemplate input)
        where rules = basicRules object ++
                      [ ("@BINDING_TOOL_OUTPUT@", bt_contents)
                      , ("@PROXY_PROTOTYPES@"   , protos)
                      , ("@CONSTANTS@"          , join "\n" . map declareEnum . enums $ model)
                      ]

    model = fromJust $ fromXML (T.pack $ xml input)
    protos = concat $ map describeI (interfaces model)

    describeI :: Interface -> String
    describeI intf =
        let iname = interfaceName intf in
        concat . intersperse "\n" $ 
             map (describeM iname) (interfaceMethods intf)
          ++ map (describeP iname) (interfaceProperties intf)

    describeP :: String -> Property -> String
    describeP intf p
      = concat . intersperse "\n" $ catMaybes [ getter, setter ]
      where
        acc = propertyAccess p
        getter | acc `elem` [Read, ReadWrite] = describePGetter intf p
               | otherwise = Nothing
        setter | acc `elem` [Write,ReadWrite] = describePSetter intf p
               | otherwise = Nothing

    describePGetter :: String -> Property -> Maybe String
    describePGetter intf p
      = do getter <- getter_method (propertyType p)
           typ <- glib_getter_type (propertyType p)
           let
              intf_ = replace "." "_" intf             
              r = [ ("@INTERFACE@", intf)
                  , ("@INTERFACE_@", intf_)
                  , ("@PROPERTY@", propertyName p)
                  , ("@PROPERTY_@", replace "-" "_" (propertyName p))
                  , ("@T@", typ)
                  , ("@GETTER@", getter)
                  ]
           return $ substRules r (lookupTemplate "template_client_pgetter.c" input)
              
    describePSetter :: String -> Property -> Maybe String
    describePSetter intf p
      = do setter <- setter_method (propertyType p)
           typ <- glib_setter_type (propertyType p)
           let
              intf_ = replace "." "_" intf             
              r = [ ("@INTERFACE@", intf)
                  , ("@INTERFACE_@", intf_)
                  , ("@PROPERTY@", propertyName p)
                  , ("@PROPERTY_@", replace "-" "_" (propertyName p))
                  , ("@T@", typ)
                  , ("@SETTER@", setter)
                  ]
           return $ substRules r (lookupTemplate "template_client_psetter.c" input)

    describeM :: String -> Method -> String
    describeM iname m@(Method name inargs outargs) =
        let args =
                concat.intersperse ", " $
                            "xcdbus_conn_t *_conn"
                          : "const char *_service"
                          : "const char *_obj_path"
                          : case cargsFromDBusMethod m of
                              []  -> []
                              str -> [str]
            mname  = decamelise name
            iname' =
                map (\c -> case c of 
                             '.' -> '_'
                             _   -> c) iname

            call                       = iname' ++ "_" ++ mname
            call_args                  = concat . intersperse ", " $ ( map (arg_name "IN_") inargs
                                                                    ++ map (arg_name "OUT_") outargs )
            call_args'                 = if null call_args
                                            then ""
                                            else call_args ++ ", "


            arg_name prefix (Parameter n _) = prefix ++ n
        in
        unlines [ "\nstatic inline gboolean" 
                , iname' ++ "_" ++ mname ++ "_(" ++ args ++ ") {"
                , "  DBusGProxy *proxy = xcdbus_get_proxy(_conn, _service, _obj_path, \"" ++ iname ++ "\");"
                , "  if (!" ++ call ++ "(proxy, " ++ call_args' ++ "NULL)) {"
                , "    xcdbus_dispatch(_conn);"
                , "    return FALSE;"
                , "  }"
                , "  xcdbus_dispatch(_conn);"
                , "  return TRUE;"
                , "}"
                ]
