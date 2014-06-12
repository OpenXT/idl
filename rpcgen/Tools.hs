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

module Tools (
               spawnShell
             , spawnShell'
             , withTempFile
             , capitalise
             , decapitalise
             , camelise
             , decamelise
             , replace
             , dashesToUnderscores
             , quote
             , indent
             , join
             ) where

import Data.Char
import Data.List
import qualified Data.Text as T
import Control.Exception
import System.Process
import System.IO
import System.Exit
import System.Directory

join :: String -> [String] -> String
join sep = concat . intersperse sep

indent :: Int -> String -> String
indent level text =
    unlines . map shift $ lines text
  where
    shift s = replicate level ' ' ++ s

quote :: String -> String
quote s = "\"" ++ s ++ "\""

capitalise :: String -> String
capitalise []     = []
capitalise (c:cs) = toUpper c : cs

decapitalise :: String -> String
decapitalise []     = []
decapitalise (c:cs) = toLower c : cs

-- convert a string foo_bar_zonk into FooBarZonk
camelise :: String -> String
camelise = capitalise . aux
    where aux ('_' : c : cs) = toUpper c : aux cs
          aux (c : cs)       = c : aux cs
          aux []             = []

-- convert a string FooBarZonk into foo_bar_zonk
decamelise :: String -> String
decamelise (a : b : xs) | isLower a && isUpper b = a : '_' : toLower b : decamelise xs
decamelise (a : xs    ) | isUpper a              = toLower a : decamelise xs
decamelise (a : xs    )                          = a : decamelise xs
decamelise []                                    = []

replace :: String -> String -> String -> String
replace pat repl txt =
    T.unpack $ T.replace (T.pack pat) (T.pack repl) (T.pack txt)

dashesToUnderscores = replace "-" "_"

-- Execute shell command and wait for its output, return empty string in case of exit failure
spawnShell :: String -> IO String
spawnShell cmd =
    spawnShell' cmd >>= f where f Nothing  = return ""
                                f (Just s) = return s

-- Execute shell command and wait for its output, return Nothing on failure exit code
spawnShell' :: String -> IO (Maybe String)
spawnShell' cmd =
    runInteractiveCommand cmd >>= \ (_, stdout, _, h) ->
        do contents <- hGetContents stdout
           -- force evaluation of contents
           exitCode <- length contents `seq` waitForProcess h
           case exitCode of
             ExitSuccess -> return $ Just contents
             _           -> return   Nothing

-- open temporary file & exec an action with it
withTempFile :: String -> ((FilePath, Handle) -> IO a) -> IO a
withTempFile name_templ action =
    do dir <- getTemporaryDirectory
       bracket 
         (openTempFile dir name_templ)
         release
         action
    where
      release (p,h) = hClose h >> removeFile p
