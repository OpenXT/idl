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

module Template ( Pattern, Replacement, Rule
                , substRules
                , untemplatise
                ) where

import Control.Applicative
import Data.List
import qualified Data.Text.Lazy as T

type Pattern = String
type Replacement = String
type Rule = (Pattern, Replacement)

substRules :: [Rule] -> String -> String
substRules rules input =
    T.unpack $ foldl' subst inputT rules
  where
    inputT = T.pack input
    subst text (pattern,replacement) = T.replace (T.pack pattern) (T.pack replacement) text

untemplatise :: [Rule] -> FilePath -> IO String
untemplatise rules file =
    substRules rules <$> readFile file
