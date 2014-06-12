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

{-# LANGUAGE OverloadedStrings,PatternGuards #-}
module Model ( fromXML
             , propertiesFromInterfaceSet
             , propertyAccessors
             , propertyGetter
             , propertySetter
             , remotePropertyGetter
             , remotePropertySetter
             , orgFreedesktopDBusProperties

             , Model (..)
             , Interface (..)
             , Method (..)
             , Signal (..)
             , Parameter (..)
             , Property (..)
             , Access (..)
             , Enumeration (..)
             , EnumValue (..)
             ) where

import Data.Maybe
import Control.Applicative
import DBus.Types as D
import qualified Data.Text.Lazy as TL
import qualified Text.XML.HaXml as X
import Text.XML.HaXml (o)

import Tools

data Model = Model { interfaces :: [Interface]
                   , enums :: [Enumeration] }
               deriving (Eq)

data Interface = Interface { interfaceName :: String
                           , interfaceMethods :: [Method]
                           , interfaceSignals :: [Signal]
                           , interfaceProperties :: [Property]
                           }
               deriving (Eq)

data Signal = Signal { signalName :: String
                     , signalParams :: [Parameter] }
               deriving (Eq)

data Method = Method { methodName :: String
                     , methodInParams :: [Parameter]
                     , methodOutParams :: [Parameter] }
               deriving (Eq)

data Parameter = Parameter { parameterName :: String
                           , parameterType :: D.Type }
               deriving (Eq)

data Property = Property { propertyName :: String
                         , propertyType :: D.Type
                         , propertyAccess :: Access }
              deriving (Eq)

data Enumeration = Enumeration { enumName :: String
                               , enumType :: D.Type
                               , enumValues :: [EnumValue] }
                 deriving (Eq)

data EnumValue = EnumValue { enumSuffix :: String
                           , enumValue :: String }
               deriving (Eq)
               
data Access = Read | Write | ReadWrite
            deriving (Eq)

-- properties implemented by given set of interfaces (tupled with relevant interface)
propertiesFromInterfaceSet :: [Interface] -> [(Interface,Property)]
propertiesFromInterfaceSet intfs =
    concat $ map property_pairs intfs
    where
      property_pairs intf = zip (repeat intf) (interfaceProperties intf)

-- make getter/setter methods for a property
propertyAccessors :: Property -> [Method]
propertyAccessors prop = methods
    where
      methods | propertyAccess prop == ReadWrite = [getter,setter]
              | propertyAccess prop == Read      = [getter]
              | propertyAccess prop == Write     = [setter]
              | otherwise = error ("Unexpected or missing property access type in property " ++ propertyName prop ++ ".")
      getter = propertyGetter prop
      setter = propertySetter prop

propertyGetter :: Property -> Method
propertyGetter prop =
    Method { methodName = ("get_" ++ method_suffix)
           , methodInParams = []
           , methodOutParams = [Parameter "value" (propertyType prop)] }
      where
        method_suffix = dashesToUnderscores $ replace "." "_" (propertyName prop)

propertySetter :: Property -> Method
propertySetter prop =
      Method { methodName = ("set_" ++ method_suffix)
             , methodInParams = [Parameter "value" (propertyType prop)]
             , methodOutParams = [] }
      where
        method_suffix = dashesToUnderscores $ replace "." "_"  (propertyName prop)

remotePropertyGetter :: Property -> Method
remotePropertyGetter prop =
    Method "Get" [Parameter "interface" D.DBusString, Parameter "property" D.DBusString] [Parameter "value" D.DBusVariant]

remotePropertySetter :: Property -> Method
remotePropertySetter prop = 
    Method "Set" [Parameter "interface" D.DBusString, Parameter "property" D.DBusString, Parameter "value" D.DBusVariant] []


orgFreedesktopDBusProperties :: Interface
orgFreedesktopDBusProperties =
    Interface { interfaceName = "org.freedesktop.DBus.Properties"
              , interfaceSignals = []
              , interfaceProperties = []
              , interfaceMethods =
                  [ Method "Get"    [Parameter "interface" D.DBusString, Parameter "property" D.DBusString] [Parameter "value" D.DBusVariant]
                  , Method "Set"    [Parameter "interface" D.DBusString, Parameter "property" D.DBusString, Parameter "value" D.DBusVariant] []
                  , Method "GetAll" [Parameter "interface" D.DBusString] [Parameter "properties" (D.DBusDictionary D.DBusString D.DBusVariant)]
                  ]
              }

fromXML :: TL.Text -> Maybe Model
fromXML text = Model <$> is <*> es
    where
      is = parseInterfaces root
      es = sequence $ map parseEnum enumNodes
      X.Document _ _ root _ = X.xmlParse "" (TL.unpack text)
      childEnums = childElems "tp:enum"
      enumNodes = childEnums root
               ++ concatMap childEnums (childElems "interface" root)

parseInterfaces :: X.Element i -> Maybe [Interface]
parseInterfaces root = sequence $ map parseInterface ielems
    where
      ielems = childElems "interface" root

parseInterface :: X.Element i -> Maybe Interface
parseInterface e = do
  name <- attr "name" e
  methods <- sequence $ map parseMethod melems
  properties <- sequence $ map parseProperty pelems
  signals <- sequence $ map parseSignal selems
  return $ Interface name methods signals properties
  where
    melems = childElems "method" e
    pelems = childElems "property" e
    selems = childElems "signal" e

parseMethod :: X.Element i -> Maybe Method
parseMethod e = do
  name <- attr "name" e
  inps <- sequence $ map parseParameter inpelems
  outs <- sequence $ map parseParameter outelems
  return $ Method name inps outs
  where
    argelems = childElemsWith (X.tag "arg") e
    outelems = childElemsWith (X.tag "arg" `o` X.attrval ("direction", X.AttValue [Left "out"])) e
    inpelems = filter p argelems where
                p e = case attr "direction" e of
                        Nothing   -> True
                        Just "in" -> True
                        _         -> False

parseProperty :: X.Element i -> Maybe Property
parseProperty e = do
  name <- attr "name" e
  typ <- parseType =<< attr "type" e
  access <- parseAccess =<< attr "access" e
  return $ Property name typ access

parseSignal :: X.Element i -> Maybe Signal
parseSignal e =
  Signal <$> attr "name" e <*>
    (sequence . map parseParameter $ childElems "arg" e)

parseEnum :: X.Element i -> Maybe Enumeration
parseEnum e =
  Enumeration <$> attr "name" e <*> (parseType =<< attr "type" e) <*>
    (sequence $ map parseEnumValue (childElems "tp:enumvalue" e))

parseEnumValue :: X.Element i -> Maybe EnumValue
parseEnumValue e =
  EnumValue <$> attr "suffix" e <*> attr "value" e

parseAccess :: String -> Maybe Access
parseAccess "read" = Just Read
parseAccess "write" = Just Write
parseAccess "readwrite" = Just ReadWrite
parseAccess _ = Nothing

parseType :: String -> Maybe D.Type
parseType s = do
  typsig <- D.mkSignature (TL.pack s)
  case D.signatureTypes typsig of
    [t] -> Just t
    _   -> Nothing

parseParameter :: X.Element i -> Maybe Parameter
parseParameter e = Parameter (fromMaybe "" $ attr "name" e) <$>
                   (parseType =<< attr "type" e)

childElems :: String -> X.Element i -> [X.Element i]
childElems name = childElemsWith (X.tag name)

childElemsWith :: X.CFilter i -> X.Element i -> [X.Element i]
childElemsWith fil (X.Elem _ _ contents) =
    catMaybes . map select . concatMap fil $ contents
    where
      select (X.CElem e _) = Just e
      select _ = Nothing
    
attr :: String -> X.Element i -> Maybe String
attr name (X.Elem n attrs _) = show <$> lookup name attrs
