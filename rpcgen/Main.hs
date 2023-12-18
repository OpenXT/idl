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
module Main where

import Data.List
import Data.Char
import qualified Data.Text as T
import qualified Data.Map as M

import Control.Applicative
import System.IO
import System.FilePath
import System.Exit
import System.Console.GetOpt
import System.Environment
import System.Directory
import qualified DBus.Introspection as I
import qualified DBus.Introspection.Parse as P

import qualified Backend
import qualified Backend.C
import qualified Backend.C2
import qualified Backend.Camel
import qualified Backend.Haskell
import qualified Backend.Javascript
import qualified Backend.Ruby

import Paths_xc_rpcgen

-- Command line options passed to utility
data OptFlag = Output String
             | TemplatesDir String
             | ModulePrefix String
             | Name String
             | Server
             | Client
             | Camel
             | Haskell
             | Javascript
             | Ruby
             | C2
               deriving (Eq, Show)

-- Configuartion
data Config = Config { templatesDir :: FilePath
                     , outputDir    :: FilePath
                     , inputXml     :: FilePath
                     , objectName   :: String
                     , prefix       :: String
                     , serverMode   :: Bool }

type Problem = String

checkXML ::String -> Maybe Problem
checkXML xml =
    case P.parseXML "/" (T.pack xml) of
      Nothing  -> Just "failed to parse interface specification XML"
      Just obj -> Nothing

mkBackendInput :: Config -> IO Backend.Input
mkBackendInput c =
    do template_files <- filter (\x -> not $ x `elem` [".",".."]) <$> getDirectoryContents (templatesDir c)
       let template_paths = map joinPath $ map (\f -> templatesDir c : [f]) template_files
       template_contents <- mapM readFile template_paths
       xml <- readFile (inputXml c)
       return Backend.Input { Backend.xml        = xml
                            , Backend.templates  = M.fromList $ zip template_files template_contents
                            , Backend.objectname = objectName c
                            , Backend.prefix     = prefix c
                            , Backend.templatesdir= templatesDir c
                            , Backend.outputdir  = outputDir c }

mkConfig :: FilePath -> [OptFlag] -> [String] -> Config
mkConfig datadir opts nonOpts =
    let cfg = Config { inputXml     = head nonOpts
                     , outputDir    = foldl' extractOutput "." opts
                     , templatesDir = foldl' extractTemplatesDir (datadir </> "templates") opts
                     , objectName   = foldl' extractName defaultName opts
                     , prefix       = foldl' extractModPrefix "" opts
                     , serverMode   = (not . null) $ filter (== Server) opts
                     }
        defaultName = map toLower $ takeBaseName (head nonOpts)
    in cfg

--
--
--

options = [ Option "o" ["output"] (ReqArg Output "DIR") "Output directory"
          , Option "c" ["client"] (NoArg Client) "Generate client bindings"
          , Option "s" ["server"] (NoArg Server) "Generate server bindings"
          , Option "n" ["name"] (ReqArg Name "NAME") "Specify generated object name/prefix"
          , Option ""  ["camel"] (NoArg Camel) "OCAML mode"
          , Option ""  ["haskell"] (NoArg Haskell) "HASKELL mode"
          , Option ""  ["c2"] (NoArg C2) "C alternative mode"
          , Option ""  ["javascript"] (NoArg Javascript) "JAVASCRIPT mode"
          , Option ""  ["ruby"] (NoArg Ruby) "RUBY mode"
          , Option ""  ["module-prefix"] (ReqArg ModulePrefix "PREFIX") "Prefix before autogenerated module name"
          , Option ""  ["templates-dir"] (ReqArg TemplatesDir "DIR") "Directory with file templates"
          ]

usage = do
    prog <- getProgName
    hPutStrLn stderr $ usageInfo ("Usage: " ++ prog ++ " [options] xml-file") options

selectBackend :: [OptFlag] -> Backend.Backend
selectBackend opts
  | Haskell `elem` opts    = Backend.Haskell.backend
  | Camel `elem` opts      = Backend.Camel.backend
  | Javascript `elem` opts = Backend.Javascript.backend
  | Ruby `elem` opts       = Backend.Ruby.backend
  | C2 `elem` opts         = Backend.C2.backend
  | otherwise              = Backend.C.backend

main = do
  args <- getArgs
  case getOpt Permute options args of

    -- no options given
    (_   , []     , _   ) -> usage

    -- failed to parse cmdline                                                
    (_   , _      , errs) | errs /= [] -> usage

    (opts, nonOpts, _   ) -> getDataDir >>= \datadir ->
        let cfg = mkConfig datadir opts nonOpts in
          -- sanity check for the XML file
          do contents <- readFile (inputXml cfg)
             case checkXML contents of
               Just problem -> hPutStrLn stderr problem >> exitWith (ExitFailure 1)
               Nothing      ->
                   do let backend = selectBackend opts
                      input <- mkBackendInput cfg
                      -- generate output files
                      files <- case (serverMode cfg) of True -> (Backend.genServer backend) input
                                                        _    -> (Backend.genClient backend) input
                      -- and write them
                      mapM_ (outputFile cfg) files


outputFile :: Config -> (String,String) -> IO ()
outputFile config (name,contents) =
    writeFile path contents
    where path = joinPath [ (outputDir config), name ]

extractOutput x (Output dir) = dir
extractOutput x _            = x

extractTemplatesDir x (TemplatesDir dir) = dir
extractTemplatesDir x _                  = x

extractName x (Name n) = n
extractName x _        = x

extractModPrefix x (ModulePrefix p) = p
extractModPrefix x _ = x
