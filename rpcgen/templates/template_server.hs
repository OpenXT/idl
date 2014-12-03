-- THIS FILE IS AUTOGENERATED -> DO NOT EDIT OR YOUR EDITS WILL BE LOST!
{-# LANGUAGE OverloadedStrings #-}
module @MODULE@ (
  @SERVER_TYPE@ (..)

  -- use this to get list of interfaces for export purposes, from a server object definition
, interfaces

) where

import Data.String
import Data.Word
import Data.Int
import Data.Maybe
import qualified Data.Map as Map
import qualified Data.Text.Lazy as TL
import Rpc.Core
import qualified Data.ByteString as B
import Tools.FreezeIOM

-- Server data type, is a list of methods which have to be implemented manually and elsewhere
data @SERVER_TYPE@ m = @SERVER_TYPE@ {
@SERVER_TYPE_SIGNATURES@
}

-- Stubs handle marshalling datatypes from/into variants when doing calls
@SERVER_STUBS@

-- produce list of interfaces for exporting, given the server data type !
@SERVER_INTERFACES@