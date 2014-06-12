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
module Backend.Ruby where

import Data.List
import Control.Monad
import Backend
import Tools
import System.IO
import System.Process
import System.Exit

backend :: Backend
backend = Backend { genServer = genServer_
                  , genClient = genClient_ }


genClient_ :: Input -> IO Output
genClient_ = error "client mode not supported by ruby backend"

genServer_ :: Input -> IO Output
genServer_ input =
    withTempFile "xml-intf" $ \(name,handle) ->
        do hPutStr handle (xml input)
           hFlush handle
           putStrLn $ "invoking " ++ exe ++ " " ++ concat (intersperse " " (args name))
           (status, _, serr) <- readProcessWithExitCode exe (args name) ""
           when (status /= ExitSuccess) $
                do hPutStrLn stderr "xsltproc FAILED"
                   hPutStrLn stderr serr
                   exitWith (ExitFailure 1)
           return []
    where
      exe = "xsltproc"
      args fname = ["--output", outputdir input,
            "--stringparam", "signal_file", signal_file,
            "--stringparam", "method_file", method_file,
            "--stringparam", "signal_class", signal_class,
            "--stringparam", "method_class", method_class,
            "--stringparam", "method_impl_class", method_impl_class,
            "--stringparam", "constants_file", constants_file,
            getTemplateFile "ruby.xsl" input, fname]

      oname = objectname input
      signal_file  = oname ++ "_notify_dbus_iface.rb"
      method_file  = oname ++ "_dbus_iface.rb"
      constants_file = oname ++ "_dbus_constants.rb"
      signal_class = (capitalise . camelise $ oname) ++ "NotifyDBusInterface"
      method_class = (capitalise . camelise $ oname) ++ "DBusInterface"
      method_impl_class = (capitalise . camelise $ oname) ++ "DBus"
