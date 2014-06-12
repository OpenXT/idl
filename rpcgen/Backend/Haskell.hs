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

{-# LANGUAGE PatternGuards #-}
module Backend.Haskell where

import Data.List
import Data.Maybe
import qualified Data.Text.Lazy as T
import qualified DBus.Types as D
import Text.Printf
import Backend
import Tools
import Template
import Model

-- | property accessors use values boxed in variant types, but the type of
--   boxed value is constant for a given property.
data DBusType = DBusType D.Type
              | VariantBoxedDBusType D.Type

-- | names have powers
data InterfaceName = InterfaceName { interfaceNameStr :: String }
data MethodName = MethodName { methodNameStr :: String }
data FunctionName = FunctionName { functionNameStr :: String }
data VariableName = VariableName { variableNameStr :: String }

interfaceName' = InterfaceName . interfaceName
methodName'    = MethodName . methodName

-- | haskell types
data HaskellType =
     HaskellType { haskellTypeStr :: String }

unitType               = HaskellType "()"
stringType             = HaskellType "String"
functionType arg_types = HaskellType . join " -> " . map haskellTypeStr $ arg_types

-- | haskell type classes
data TypeClass =
     TypeClass { typeClassStr :: String }

monadFreezeableRpcClass = TypeClass "(FreezeIOM ctx i m, MonadRpc e m)"
monadRpcClass = TypeClass "(MonadRpc e m)"

-- | haskell expression (or just a block of code)
data Expression = Expression { expressionStr :: String }

indentExp :: Int -> Expression -> Expression
indentExp n (Expression s) = Expression (indent n s)

applyExp :: Expression -> Expression -> Expression
applyExp (Expression a) (Expression b) = Expression (printf "(%s $ %s)" a b)

-- | field of haskell's record
data RecordField = RecordField FunctionName HaskellType

-- | haskell function declarations
data Function = Function {
      functionHdr :: FunctionHeader
    , functionBody :: Expression
    }

data FunctionHeader = FunctionHeader {
      functionName :: String
    , functionClasses :: [TypeClass]
    , functionArgs :: [FunctionArg]
    , functionReturnType :: HaskellType
    }

data FunctionArg = FunctionArg {
      argName :: String
    , argType :: HaskellType
    }

-- | pretty printing
prettyPrintFunSig :: FunctionHeader -> String
prettyPrintFunSig f =
    printf "%s :: %s %s" (functionName f) classes (join " -> " types)
    where
      ret_t = functionReturnType f
      types = map haskellTypeStr $ map argType (functionArgs f) ++ [ret_t]
      classes | null (functionClasses f) = ""
              | cs <- functionClasses f  = printf "(%s) => " (intercalate "," $ map typeClassStr cs)
      
prettyPrintFunHeader :: FunctionHeader -> String
prettyPrintFunHeader f =
    printf "%s %s" (functionName f) (join " " argnames)
    where
      args     = functionArgs f
      argnames = map argName args
                       
prettyPrintFun :: Function -> String
prettyPrintFun (Function decl body) =
    unlines [ prettyPrintFunSig decl
            , prettyPrintFunHeader decl ++ " ="
            , expressionStr (indentExp 4 body) ]

prettyPrintRF :: RecordField -> String
prettyPrintRF (RecordField n t) = printf "%s :: %s" (functionNameStr n) (haskellTypeStr t)

-- | Invocation of remote method with specific arguments
data RemoteMethodInvocation =
    Rmi { rmiInterface :: InterfaceName
        , rmiMethod :: MethodName
        , rmiArgs :: [RmiArg]
        , rmiArgTypes :: [DBusType]
        , rmiReturnTypes :: [DBusType]
        }

data RmiArg = ArgVariable VariableName
            | ArgExp Expression

-- | dbus type mapping
haskellType :: D.Type -> HaskellType
haskellType D.DBusBoolean    = HaskellType "Bool"
haskellType D.DBusByte       = HaskellType "Word8"
haskellType D.DBusInt16      = HaskellType "Int16"
haskellType D.DBusInt32      = HaskellType "Int32"
haskellType D.DBusInt64      = HaskellType "Int64"
haskellType D.DBusWord16     = HaskellType "Word16"
haskellType D.DBusWord32     = HaskellType "Word32"
haskellType D.DBusWord64     = HaskellType "Word64"
haskellType D.DBusDouble     = HaskellType "Double"
haskellType D.DBusString     = HaskellType "String"
haskellType D.DBusSignature  = HaskellType "String"
haskellType D.DBusObjectPath = HaskellType "ObjectPath"
haskellType D.DBusVariant    = HaskellType "Variant"
haskellType (D.DBusArray D.DBusByte) = HaskellType "B.ByteString"
haskellType (D.DBusArray t)  = HaskellType $ printf "[%s]" (haskellTypeStr $ haskellType t)
haskellType (D.DBusDictionary k v) =
    HaskellType $ printf "(Map.Map %s %s)" (haskellTypeStr tk) (haskellTypeStr tv)
    where tk = haskellType k
          tv = haskellType v
haskellType (D.DBusStructure ts) =
    HaskellType $ printf "(%s)" (join ", " $ map (haskellTypeStr . haskellType) ts)

-- to avoid clashes we suffix param names
suffixParamName :: Parameter -> Parameter
suffixParamName (Parameter name t) = Parameter (name ++ "_") t
suffixParamNames :: [Parameter] -> [Parameter]
suffixParamNames = map suffixParamName

-- | expressions to convert between variants and haskell types (assumes library support for proper Variable instances)
haskellToVariantExp :: Expression
haskellToVariantExp = Expression "toVariant"

variantToHaskellExp :: Expression
variantToHaskellExp = Expression "((\\mv_ -> let Just v_ = mv_ in v_) . fromVariant)"

normaliseFunctionName :: String -> String
normaliseFunctionName = decapitalise . camelise . replace "." "_" . replace "-" "_"

stubReturnType :: RemoteMethodInvocation -> HaskellType
stubReturnType rmi =
    HaskellType $ printf "m (%s)" (join "," . map haskellTypeStr $ out_types)
    where
      out_types = map ( haskellType . unpacked_dbus_type ) (rmiReturnTypes rmi)
      unpacked_dbus_type (DBusType t) = t
      unpacked_dbus_type (VariantBoxedDBusType t) = t

stubReturnType' :: [D.Type] -> HaskellType
stubReturnType' out_types =
    HaskellType $ printf "m (%s)" (join "," . map (haskellTypeStr . haskellType) $ out_types)

-- method invocation experssion which returns tuple of variants
remoteMethodInvocationExp :: RemoteMethodInvocation -> Expression
remoteMethodInvocationExp rmi =
    Expression . unlines $
    [ printf "(do variants <- rpcCall (mkcall service_ objPath_ %s %s" (quote interface_str) (quote method_str)
    , printf "                 [ %s ] )" call_list_str
    ,        "    case variants of"
    , printf "      [ %s ] -> return (%s)" out_names_str out_values_str
    , printf "      _      -> error \"RPC call returned unexpected number of arguments! %s.%s\")" interface_str method_str
    ]
    where
      interface_str = interfaceNameStr (rmiInterface rmi)
      method_str    = methodNameStr (rmiMethod rmi)
      args          = zip (rmiArgs rmi) (rmiArgTypes rmi)
      call_list_str = join ", " $ map (expressionStr . arg_expr) args

      arg_expr (ArgVariable name, DBusType _)             = applyExp haskellToVariantExp $ Expression (variableNameStr name)
      arg_expr (ArgVariable name, VariantBoxedDBusType _) = applyExp haskellToVariantExp . applyExp haskellToVariantExp $ Expression (variableNameStr name)
      arg_expr (ArgExp exp, _)                            = exp

      out_names_str  = join ", " $ out_names
      out_names      = take (length . rmiReturnTypes $ rmi) namepool
                       where namepool = map (\i -> "out_" ++ show i) [1..]
      out_values_str = join ", " $ map expressionStr out_values
      out_values     = map out_value (zip out_names (rmiReturnTypes rmi))
      out_value (name, DBusType _)             = applyExp variantToHaskellExp $ Expression name
      out_value (name, VariantBoxedDBusType _) = applyExp variantToHaskellExp . applyExp variantToHaskellExp $ Expression name

remoteMethodInvocation :: InterfaceName -> Method -> RemoteMethodInvocation
remoteMethodInvocation interface_name remote_method =
    Rmi { rmiInterface = interface_name
        , rmiMethod = methodName' remote_method
        , rmiArgs = map arg (methodInParams remote_method)
        , rmiArgTypes = map arg_type (methodInParams remote_method)
        , rmiReturnTypes = map arg_type (methodOutParams remote_method)
        }
    where
      arg_type p = DBusType (parameterType p)
      arg p = ArgVariable . VariableName . parameterName $ p

getterInvocation :: Interface -> Property -> RemoteMethodInvocation
getterInvocation i p =
    Rmi { rmiInterface = interfaceName' orgFreedesktopDBusProperties
        , rmiMethod = methodName' (remotePropertyGetter p)
        , rmiArgs =
            [ ArgExp . Expression $ printf "toVariant %s" (quote (interfaceNameStr interface_name))
            , ArgExp . Expression $ printf "toVariant %s" (quote $ propertyName p) ]
        , rmiArgTypes =
            [ DBusType D.DBusString
            , DBusType D.DBusString ]
        , rmiReturnTypes =
            [ VariantBoxedDBusType . propertyType $ p ]
        }
    where interface_name = interfaceName' i

setterInvocation :: Interface -> Property -> RemoteMethodInvocation
setterInvocation i p =
    Rmi { rmiInterface = interfaceName' orgFreedesktopDBusProperties
        , rmiMethod = methodName' (remotePropertySetter p)
        , rmiArgs =
            [ ArgExp . Expression $ printf "toVariant %s" (quote (interfaceNameStr interface_name))
            , ArgExp . Expression $ printf "toVariant %s" (quote $ propertyName p)
            , ArgVariable . VariableName $ "value" ]
        , rmiArgTypes =
            [ DBusType D.DBusString
            , DBusType D.DBusString
            , VariantBoxedDBusType (propertyType p) ]
        , rmiReturnTypes = [ ]
        }
    where interface_name = interfaceName' i

-- | Stub function to invoke bit of remote code / implement bit of server side code
data Stub = GetterStub Interface Property
          | SetterStub Interface Property
          | MethodStub Interface Method

remoteStubName :: Stub -> String
remoteStubName (GetterStub i p) = normaliseFunctionName $ interfaceName i ++ "_get_" ++ propertyName p
remoteStubName (SetterStub i p) = normaliseFunctionName $ interfaceName i ++ "_set_" ++ propertyName p
remoteStubName (MethodStub i m) = normaliseFunctionName $ interfaceName i ++ "_" ++ methodName m

remoteInvocation :: Stub -> RemoteMethodInvocation
remoteInvocation (GetterStub i p) = getterInvocation i p
remoteInvocation (SetterStub i p) = setterInvocation i p
remoteInvocation (MethodStub i m) = remoteMethodInvocation (interfaceName' i) m

hfunctionArgOfParam :: Parameter -> FunctionArg
hfunctionArgOfParam p = FunctionArg (parameterName p) (haskellType . parameterType $ p)
hfunctionArgs = map hfunctionArgOfParam

remoteStubFun :: Stub -> Function
remoteStubFun stub = Function hdr body where
    hdr = FunctionHeader {
             functionName       = remoteStubName stub
           , functionClasses    = [monadRpcClass]
           , functionArgs       = fixed_args ++ in_args stub
           , functionReturnType = stubReturnType invocation
           }

    invocation = remoteInvocation stub
    invocation_exp = remoteMethodInvocationExp invocation
    body = invocation_exp

    fixed_args =
        [ FunctionArg "service_" stringType
        , FunctionArg "objPath_" stringType ]
    in_args (GetterStub i p) = []
    in_args (SetterStub i p) = [ FunctionArg "value" (haskellType . propertyType $ p) ]
    in_args (MethodStub i m) = hfunctionArgs (methodInParams m)


interfaceStubs :: Interface -> [Stub]
interfaceStubs i = pstubs ++ mstubs
    where
      properties = interfaceProperties i
      methods    = interfaceMethods i
      pstubs     = concatMap pstub properties
      mstubs     = concatMap mstub methods
      pstub p    = case propertyAccess p of
                     Read      -> [GetterStub i p]
                     Write     -> [SetterStub i p]
                     ReadWrite -> [GetterStub i p, SetterStub i p]
      mstub m    = [MethodStub i m]

-- name of file where we are outputting client staff
outputClientFile :: Input -> String
outputClientFile input =
    camelise (objectname input) ++ "Client.hs"

-- name of module containing client proxies
clientModuleName :: Input -> String
clientModuleName input =
    better_pr ++ camelise (objectname input ++ "Client")
  where
    pr = prefix input
    better_pr | pr == ""   = ""
              | otherwise  = pr ++ "."

enumValueFunName :: Enumeration -> EnumValue -> String
enumValueFunName e ev = "e" ++ enumName e ++ "_" ++ enumSuffix ev

enumValueFun :: Enumeration -> EnumValue -> Function
enumValueFun e ev = Function hdr (enumValueExp (enumType e) (enumValue ev)) where
    hdr = FunctionHeader {
            functionName = enumValueFunName e ev
          , functionClasses = []
          , functionArgs = []
          , functionReturnType = haskellType (enumType e)
          }

enumValueExp :: D.Type -> String -> Expression
enumValueExp D.DBusBoolean "true"  = Expression "True"
enumValueExp D.DBusBoolean "false" = Expression "False"
enumValueExp D.DBusBoolean s  = error $ "bad boolean enum string: " ++ show s
enumValueExp D.DBusByte    s  = Expression s
enumValueExp D.DBusInt16   s  = Expression s
enumValueExp D.DBusInt32   s  = Expression s
enumValueExp D.DBusInt64   s  = Expression s
enumValueExp D.DBusWord16  s  = Expression s
enumValueExp D.DBusWord32  s  = Expression s
enumValueExp D.DBusWord64  s  = Expression s
enumValueExp D.DBusDouble  s  = Expression s
enumValueExp D.DBusString  s  = Expression (quote s)
enumValueExp t _ = error $ "unsupported enum type: " ++ show t

enumValueFuns :: [Enumeration] -> [Function]
enumValueFuns es =
    concatMap valueFs es
        where valueFs e = map (enumValueFun e) (enumValues e)


genClient_ :: Input -> IO Output
genClient_ input =
    return [ (outputClientFile input, contents) ]
  where
    contents    = substRules rules (getTemplate "template_client.hs" input)
    rules       = [ ("@METHODS@", enums_text ++ stubs_text)
                  , ("@MODULE@", clientModuleName input)
                  , ("@MODULE_EXPORTS@", join ", \n" exports) ]
    model       = modelFromInput input
    intfs       = interfaces model
    exports     =    map (functionName . functionHdr) enumfs
                  ++ map remoteStubName stubs
    -- list of functions generated in the client module
    stubs       = concatMap interfaceStubs intfs
    enumfs      = enumValueFuns (enums model)
    stubs_text  = join "\n" . map prettyPrintFun . map remoteStubFun $ stubs
    enums_text  = join "\n" . map prettyPrintFun $ enumfs


-----------------
-- SERVER STUFF!
--
-----------------

-- | stub function to emits a dbus notification
emitSignalStub :: Interface -> Signal -> Function
emitSignalStub interface signal = Function declaration body where
    declaration = FunctionHeader {
                    functionName        = normaliseFunctionName $ printf "notify_%s_%s" (interfaceName interface) (signalName signal)
                  , functionClasses     = [monadRpcClass]
                  , functionArgs        = objpath_arg : hfunctionArgs (signalParams signal)
                  , functionReturnType  = HaskellType "m ()"
                  }
    objpath_arg = FunctionArg "objPath_" (HaskellType "ObjectPath")
    body =
        Expression $ printf
          "rpcEmitSignal RpcSignal { signalPath = objPath_, signalInterfaceT = %s, signalMemberT = %s, signalArgs = %s }"
          intf_elem memb_elem body_elem
            where
              intf_elem = "fromString " ++ quote (interfaceName interface)
              memb_elem = "fromString " ++ quote (signalName signal)
              body_elem = "[" ++ join ", " variants ++ "]"
              variants  = map variant (signalParams signal)
              variant p = expressionStr . applyExp haskellToVariantExp . Expression $ parameterName p

interfaceEmitSignalStubs :: Interface -> [Function]
interfaceEmitSignalStubs i = map (emitSignalStub i) (interfaceSignals i)

notifyModuleName :: Input -> String
notifyModuleName input = better_pr ++ camelise (objectname input ++ "Notify") where
    pr = prefix input
    better_pr | pr == ""   = ""
              | otherwise  = pr ++ "."

notifyFileContents :: Input -> [Interface] -> Maybe String
notifyFileContents input interfaces = contents where
    contents
        | null stubs = Nothing
        | otherwise  = Just $ substRules rules (getTemplate "template_notify.hs" input)

    stubs = concatMap interfaceEmitSignalStubs interfaces
    rules = [ ("@MODULE@", notifyModuleName input)
            , ("@SIGNAL_STUBS@", join "\n" . map prettyPrintFun $ stubs)
            , ("@SIGNAL_EXPORTS@", join ",\n" . map (functionName . functionHdr) $ stubs) ]

serverRecordTypeName :: Input -> String
serverRecordTypeName input = printf "%sServer" (camelise . objectname $ input)

serverStubName :: Interface -> Method -> String
serverStubName i m =
    normaliseFunctionName $ printf "stub_%s_%s" (interfaceName i) (methodName m)

interfaceServerRecordFields :: Interface -> [RecordField]
interfaceServerRecordFields = map field . interfaceStubs where
    field_sig ins outs = functionType $ map haskellType ins ++ [stubReturnType' outs]
    field s@(GetterStub i p) = RecordField (FunctionName $ remoteStubName s) (field_sig [] [propertyType p])
    field s@(SetterStub i p) = RecordField (FunctionName $ remoteStubName s) (field_sig [propertyType p] [])
    field s@(MethodStub i m) = RecordField (FunctionName $ remoteStubName s) (field_sig (map parameterType (methodInParams  m))
                                                                                       (map parameterType (methodOutParams m)))

serverMethodStubF :: Input -> Interface -> Method -> Function
serverMethodStubF input interface method = Function hdr body where
    hdr = FunctionHeader {
            functionName = serverStubName interface method
          , functionClasses = [monadFreezeableRpcClass]
          , functionArgs =
              [ FunctionArg "server_" sobjtype
              , FunctionArg "args_" argstype ]
          , functionReturnType = HaskellType "m [Variant]"
          }
    sobjtype = HaskellType $ printf "%s m" (serverRecordTypeName input)
    argstype = HaskellType "[Variant]"
    body = Expression . unlines $
           [        "do case args_ of"
           , printf "     [ %s ] ->" argnamesstr
           , printf "       do (%s) <- %s server_ %s" outtuplestr callname callargs
           , printf "          return [ %s ]" variantsstr
           , printf "     _      -> error \"invalid arguments\""
           ]
    inps = methodInParams method
    outs = methodOutParams method
    outnames = take (length outs) namepool
               where namepool = map (\i -> "out_" ++ show i) [1..]
    argnamesstr = join ", " . map parameterName $ inps
    outtuplestr = join ", " outnames
    callname = remoteStubName (MethodStub interface method)
    callargs =
        join " "
        . map expressionStr
        . map (applyExp variantToHaskellExp . Expression . parameterName)
        $ inps
    variantsstr =
        join ", "
       . map expressionStr
       . map (applyExp haskellToVariantExp . Expression)
       $ outnames

-- list of methods which have to be implemented, for one dbus interface
interfaceMethodsToImplement :: Interface -> [Method]
interfaceMethodsToImplement intf = interfaceMethods intf

-- list of methods which have to be implemented, for whole dbus object
objectMethodsToImplement :: [Interface] -> [ (Interface,Method) ]
objectMethodsToImplement intfs =
    concat . map intf_methods $ intfs
    where
      intf_methods intf = zip (repeat intf) (interfaceMethodsToImplement intf)

-- implementation of org.freedesktop.DBus.Properties
orgFreedesktopDBusPropertiesImplem :: Input -> [Interface] -> [Function]
orgFreedesktopDBusPropertiesImplem input intfs =
    [ orgFreedesktopDBusPropertiesGet input intfs
    , orgFreedesktopDBusPropertiesSet input intfs
    , orgFreedesktopDBusPropertiesGetAll input intfs
    ]

-- implementation of org.freedesktop.DBus.Properties.Get
orgFreedesktopDBusPropertiesGet :: Input -> [Interface] -> Function
orgFreedesktopDBusPropertiesGet input intfs = Function hdr body where
      hdr = FunctionHeader {
              functionName = "orgFreedesktopDBusPropertiesGet"
            , functionClasses = [monadRpcClass]
            , functionArgs =
                [ FunctionArg "obj" objtype
                , FunctionArg "intf" stringType
                , FunctionArg "property" stringType ]
            , functionReturnType = HaskellType "m Variant"
            }
      objtype = HaskellType $ serverRecordTypeName input ++ " m"
      props = filter readable $ propertiesFromInterfaceSet intfs
              where readable (_,p) = propertyAccess p == Read || propertyAccess p == ReadWrite
      prop_name_as_str prop = quote $ propertyName prop
      body  = Expression $ "case (intf,property) of\n"
              ++ properties_call_stubs
              ++ indent 4 "_ -> error $ \"property \" ++ intf ++ \".\" ++ property ++ \" is not readable, or does not exist\""
      properties_call_stubs = indent 4 . unlines $ map property_call_stub props
      property_call_stub (intf,prop) =
          "(" ++ quote (interfaceName intf) ++ "," ++ prop_name_as_str prop ++ ") -> " ++ call_str (intf,prop)
      call_str (intf,prop) = remoteStubName (MethodStub intf (propertyGetter prop)) ++ " obj >>= return . toVariant"

-- implementation of org.freedesktop.DBus.Properties.Set
orgFreedesktopDBusPropertiesSet :: Input -> [Interface] -> Function
orgFreedesktopDBusPropertiesSet input intfs = Function hdr body where
      hdr = FunctionHeader {
              functionName = "orgFreedesktopDBusPropertiesSet"
            , functionClasses = [monadRpcClass]
            , functionArgs =
                [ FunctionArg "obj" objtype
                , FunctionArg "intf" stringType
                , FunctionArg "property" stringType
                , FunctionArg "value" (HaskellType "Variant")
                ]
            , functionReturnType = HaskellType "m ()"
            }
      objtype = HaskellType $ serverRecordTypeName input ++ " m"
      props = filter writable $ propertiesFromInterfaceSet intfs
              where writable (_,p) = propertyAccess p == Write || propertyAccess p == ReadWrite
      prop_name_as_str prop = quote $ propertyName prop
      body  = Expression $ "case (intf,property) of\n"
              ++ properties_call_stubs
              ++ indent 4 "_ -> error $ \"property \" ++ intf ++ \".\" ++ property ++ \" is not writable, or does not exist\""
      properties_call_stubs = indent 4 . unlines $ map property_call_stub props
      property_call_stub (intf,prop) =
          "(" ++ quote (interfaceName intf) ++ "," ++ prop_name_as_str prop ++ ") -> " ++ call_str (intf,prop)
      call_str (intf,prop) = "let Just v = fromVariant value in " ++ remoteStubName (MethodStub intf (propertySetter prop)) ++ " obj v"

-- implementation of org.freedesktop.DBus.Properties.GetAll
orgFreedesktopDBusPropertiesGetAll :: Input -> [Interface] -> Function
orgFreedesktopDBusPropertiesGetAll input intfs = Function hdr body where
      hdr = FunctionHeader {
              functionName = "orgFreedesktopDBusPropertiesGetAll"
            , functionClasses = [monadFreezeableRpcClass]
            , functionArgs =
                [ FunctionArg "obj" objtype
                , FunctionArg "intf" stringType
                ]
            , functionReturnType = HaskellType "m (Map.Map String Variant)"
            }
      objtype = HaskellType $ serverRecordTypeName input ++ " m"
      prop_name_as_str prop = quote $ propertyName prop
      body  = Expression $ "case intf of\n"
              ++ indent 4 interface_handlers
              ++ indent 4 "_ -> error $ \"unknown interface \" ++ intf"
      interface_handlers = unlines $ map interface_handler intfs
      interface_handler intf =
          let props   = filter readable $ interfaceProperties intf
                        where readable p = propertyAccess p == Read || propertyAccess p == ReadWrite
              getters = indent 16 . join "\n, " . map (getter intf) $ props in
          quote (interfaceName intf) ++ " -> rpcRunParallel [\n" ++ getters ++ "\n          ] >>= return . Map.fromList"
      getter intf p = remoteStubName (MethodStub intf (propertyGetter p)) ++ " obj >>= \\v -> return (" ++ quote (propertyName p) ++ ", toVariant v)"


declExportMethod :: (Interface,Method) -> Expression
declExportMethod m@(interface,method) =
    Expression $ printf "rpcMethod %s %s %s (%s server_)"
                   (quote methodname) (quote inargs) (quote outargs) (serverStubName interface method)
  where
    methodname = methodName method
    inargs     = join "," . map param $ methodInParams method
    outargs    = join "," . map param $ methodOutParams method
    param p | null (parameterName p)   = typecode p
            | otherwise                = parameterName p ++ ":" ++ typecode p
    typecode p                         = T.unpack $ D.typeCode (parameterType p)

declExportProperty :: (Interface,Property) -> Expression
declExportProperty p@(interface,property) =
    Expression $ printf "rpcProperty %s %s %s" (quote pname) (quote typename) access
  where
    pname         = propertyName property
    typename      = T.unpack $ D.typeCode (propertyType property)
    access        = case propertyAccess property of
                      Read -> "Read"
                      Write -> "Write"
                      ReadWrite -> "ReadWrite"

declExportInterface :: Interface -> Expression
declExportInterface i =
    Expression . unlines $
               [ printf "RpcInterface %s [" (quote iname)
               , indent 4 . join "\n, " . map (expressionStr . declExportMethod) $ methods
               , "] ["
               , indent 4 . join "\n, " . map (expressionStr . declExportProperty) $ properties
               , "]"
               ]
    where
      iname = interfaceName i
      properties = [ (i, p) | p <- interfaceProperties i ]
      methods = [ (i, m) | m <- interfaceMethods i ]

exportedInterfacesFunction :: Input -> [Interface] -> Function
exportedInterfacesFunction input interfaces = Function hdr body where
    hdr = FunctionHeader {
            functionName = "interfaces"
          , functionClasses = [monadFreezeableRpcClass]
          , functionArgs = args
          , functionReturnType = HaskellType "[RpcInterface m]"
          }
    args = [FunctionArg "server_" sobjtype]
    sobjtype = HaskellType $ printf "%s m" (serverRecordTypeName input)
    body = Expression . indent 2 . unlines $
           [ "["
           , indent 8 . join "\n," . map (expressionStr . declExportInterface) $ interfaces
           , "]"
           ]

serverObjectModuleName :: Input -> String
serverObjectModuleName input = better_pr ++ camelise (objectname input ++ "Server") where
    pr = prefix input
    better_pr | pr == ""   = ""
              | otherwise  = pr ++ "."

serverObjectFileContents :: Input -> [Interface] -> String
serverObjectFileContents input interfaces =
    substRules rules (getTemplate "template_server.hs" input)
  where
    rules = [ ("@SERVER_TYPE@", serverRecordTypeName input)
            , ("@MODULE@", serverObjectModuleName input)
            , ("@SERVER_TYPE_SIGNATURES@", record_fields)
            , ("@SERVER_STUBS@", stubstxt)
            , ("@SERVER_INTERFACES@", prettyPrintFun interfacesfun) ]
    record_fields = indent 2 . join "\n, " . map prettyPrintRF . concatMap interfaceServerRecordFields $ interfaces
    interfacesfun = exportedInterfacesFunction input expinterfaces
    expinterfaces | null (propertiesFromInterfaceSet interfaces) = interfaces
                  | otherwise = interfaces ++ [orgFreedesktopDBusProperties]
    stubstxt =
        join "\n" $ [ prettyPrintFun (serverMethodStubF input i m) | (i,m) <- objectMethodsToImplement expinterfaces ]
                 ++ [ prettyPrintFun m                             | m <- orgFreedesktopDBusPropertiesImplem input interfaces ]

constModuleName :: Input -> String
constModuleName input = better_pr ++ camelise (objectname input ++ "Const") where
    pr = prefix input
    better_pr | pr == ""   = ""
              | otherwise  = pr ++ "."

constFileContents :: Input -> [Enumeration] -> Maybe String
constFileContents input []    = Nothing
constFileContents input enums = Just contents where
    contents = substRules rules (getTemplate "template_const.hs" input)
    rules = [ ("@MODULE@", constModuleName input)
            , ("@MODULE_EXPORTS@", enumexports)
            , ("@CONSTS@", consts)
            ]
    consts      = join "\n" . map prettyPrintFun $ efs
    enumexports = join "\n, " xs where xs = map (functionName . functionHdr) efs
    efs         = enumValueFuns enums

genServer_ :: Input -> IO Output
genServer_ input = return files where
    files = catMaybes [ serverObjectFile, notifyFile, constFile ]
    serverObjectFile = Just (serverObjectFileName, serverObjectFileContents input intfs)
    serverObjectFileName = printf "%sServer.hs" (camelise . objectname $ input)
    notifyFile = fmap mk (notifyFileContents input intfs) where mk contents = (notifyFileName, contents)
    notifyFileName = printf "%sNotify.hs" (camelise . objectname $ input)
    constFile = fmap mk (constFileContents input enums') where mk contents = (constFileName, contents)
    constFileName = printf "%sConst.hs" (camelise . objectname $ input)
    model  = modelFromInput input
    intfs  = interfaces model
    enums' = enums model

modelFromInput :: Input -> Model
modelFromInput input = fromJust . fromXML $ T.pack (xml input)

backend :: Backend
backend = Backend { genServer = genServer_
                  , genClient = genClient_ }

