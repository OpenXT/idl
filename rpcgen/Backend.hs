--
-- Copyright (c) 2011 Citrix Systems, Inc.
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

module Backend ( Input (..)
               , Output (..)
               , Backend (..)
               , getTemplate
               , getTemplateFile
               ) where

import qualified Data.Map as M
import Data.Map (Map)
import System.FilePath
import Template

type Output       = [File]
type File         = (FName,FContents)
type FName        = String
type FContents    = String

data Input = Input { xml        :: String
                   , objectname :: String
                   , prefix     :: String
                   , templates  :: Map TName TContents
                   , templatesdir :: FilePath
                   , outputdir  :: FilePath
                   }
type TName     = String
type TContents = String

data Backend = Backend { genServer :: Input -> IO Output
                       , genClient :: Input -> IO Output }

getTemplate :: String -> Input -> String
getTemplate name input =
    case M.lookup name (templates input) of
      Nothing -> error $ "No such template: " ++ name
      Just c  -> c

getTemplateFile :: String -> Input -> FilePath
getTemplateFile name input =
    templatesdir input </> name
