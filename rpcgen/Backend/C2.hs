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

module Backend.C2 (backend) where

import Data.List
import Data.Maybe
import qualified Data.Text.Lazy as T
import qualified DBus.Types as D
import Text.Printf
import Backend
import Tools
import Model

outputClientHFile   object     = object ++ "_client2.h"

backend :: Backend
backend = Backend genserver genclient

genserver = undefined

genclient input = return [ (outputClientHFile object, contents) ]
	where
		object   = objectname input -- this corresponds to glib OBJECT name
		contents = (unlines
			[ "#include \"udbus.h\""
			, "#include \"udbusloop.h\""
			, "#include <stdint.h>"
			, "#include <stdlib.h>"
			]) ++ methodsGen
		methodsGen = join "\n" $ map methodGenerate $ methods
		methods = [ (intf, m) | intf <- intfs, m <- interfaceMethods intf ] ++ prop_methods
		intfs   = interfaces model
		model   = fromJust . fromXML $ T.pack (xml input)
		prop_methods = concat . map accessors $ props where
			props = propertiesFromInterfaceSet intfs
			accessors (intf,prop) = map (accessor intf) $ propertyAccessors prop
				where accessor intf p = (intf,p)

methodGenerate z@(intf, m) = unlines (
	[ fl
	, "{"
	, printf "\tdbus_sig sig;%s%s%s" (if hasInArray || hasOutArray then " int i;" else "")
	                                     (if hasInArray then " dbus_array_writer arraywriter;" else "")
	                                     (if hasOutArray then " dbus_array_reader arrayreader; int n;" else "")
	, printf "\tdbus_msg *msg%s" (if hasOut then ", *rmsg;" else ";")
	, printf "\tmsg = dbus_msg_new_method_call(-1, _dservice, _dobjpath, \"%s\", \"%s\");" (interfaceName intf) (methodName m)
	] ++ (if hasIn
		then (dbusInSigStr ++ [ printf "\tdbus_msg_body_add(msg, %d);" (8192 :: Int) ] ++ dbusInBodyStr)
		else []) ++ 
	[ printf "\tdbus_loop_send_rpc(_dconn, msg, %s);" (if hasOut then "&rmsg" else "NULL")
	, "\tdbus_msg_free(msg);"
	] ++ dbusOutBodyStr ++
	(if hasOut then [ "\tdbus_msg_free(rmsg);" ] else []) ++
	
	[ "\treturn 1;", "}" ])
	where
		hasIn  = (length $ methodInParams m) > 0
		hasOut = (length $ methodOutParams m) > 0

		hasInArray  = or $ map (isArrayType . parameterType) $ methodInParams m
		hasOutArray = or $ map (isArrayType . parameterType) $ methodOutParams m

		fl      = printf "static int %s(%s)" (cMethodName z) (typeStr)
		typeStr = intercalate ", " $
			[ "dbus_loop_handle *_dconn", "const char *_dservice", "const char *_dobjpath" ] ++
			(map (mkCType "IN" "") $ methodInParams m) ++
			(map (mkCType "OUT" "*") $ methodOutParams m)
		mkCType "IN" _ (Parameter name ty@(D.DBusArray _)) = printf "%s *%s" (cTypeOfDbus ty) (mkCParameterName "IN" name)
		mkCType pre afterty (Parameter name ty) = printf "%s%s %s" (cTypeOfDbus ty) afterty (mkCParameterName pre name)

		mkCParameterName :: String -> String -> String
		mkCParameterName pre name = printf "%s_%s" pre name

		-- SIGNATURE
		dbusInSigStr = map (\(i, t) -> printf "\tsig.a[%d] = %s;" i t) $
		               zip [(0::Int)..] $ ((concatMap mkCSignature $ methodInParams m) ++ ["-1"])
		mkCSignature (Parameter _ ty) = sigTypeOfDBus ty

		-- BODY
		dbusInBodyStr = concatMap mkInBodyFill $ methodInParams m

		mkInBodyFill (Parameter name (D.DBusArray ty)) =
			let cname = mkCParameterName "IN" name in
			let element = head $ sigTypeOfDBus ty in
			[ printf "\tdbus_msg_body_add_array_begin(msg, %s, &arraywriter);" element
			, printf "\tfor (i = 0; i < %s->size; i++) {" cname
			, "\t" ++ mkInBodyAddFct ty (printf "((%s *) %s->ptr)[i]" (cTypeOfDbus ty) cname)
			, "\t}"
			, printf "\tdbus_msg_body_add_array_end(msg, &arraywriter);" ]
		mkInBodyFill (Parameter name ty) =
			let cname = mkCParameterName "IN" name in
			[ mkInBodyAddFct ty cname ]

		mkInBodyAddFct :: D.Type -> String -> String
		mkInBodyAddFct ty name = printf "\tdbus_msg_body_add_%s(msg, %s);" (fctTypeOfDBus ty) name

		-- BODY OUT
		dbusOutBodyStr = concatMap mkOutBodyUnFill $ methodOutParams m

		mkOutBodyUnFill (Parameter name (D.DBusArray ty)) =
			let cname = mkCParameterName "OUT" name in
			let element = head $ sigTypeOfDBus ty in
			[ printf "\tdbus_msg_body_get_array(rmsg, %s, &arrayreader);" element
			, printf "\tn = dbus_msg_body_get_array_left(rmsg, &arrayreader) / sizeof(%s);" (cTypeOfDbus ty)
			, printf "\t%s->size = n;" (cname)
			, printf "\t%s->ptr = malloc((n+1) * sizeof(%s));" (cname) (cTypeOfDbus ty)
			, printf "\twhile (dbus_msg_body_get_array_left(rmsg, &arrayreader) > 0) {"
			--, printf "\tfor (i = 0; i < %s.size; i++) {" cname
			, "\t" ++ mkOutBodyAddFct ty (printf "&((%s *) %s->ptr)[i]" (cTypeOfDbus ty) cname)
			, "\t}"
			, printf "\tdbus_msg_body_get_array_left(rmsg, &arrayreader);" ]
		mkOutBodyUnFill (Parameter name ty) =
			let cname = mkCParameterName "OUT" name in
			[ mkOutBodyAddFct ty cname ]

		mkOutBodyAddFct :: D.Type -> String -> String
		mkOutBodyAddFct ty name = printf "\tdbus_msg_body_get_%s(rmsg, %s);" (fctTypeOfDBus ty) name
		

cMethodName (intf, m) = replace "." "_" (interfaceName intf) ++ "_" ++ methodName m

cTypeOfDbus (D.DBusInt32)   = "int32_t"
cTypeOfDbus (D.DBusInt64)   = "int64_t"
cTypeOfDbus (D.DBusWord32)  = "uint32_t"
cTypeOfDbus (D.DBusWord64)  = "uint64_t"
cTypeOfDbus (D.DBusBoolean) = "bool"
cTypeOfDbus (D.DBusString)  = "char *"
cTypeOfDbus (D.DBusArray _) = "dbus_array"
cTypeOfDbus o               = show o

sigTypeOfDBus (D.DBusArray a) = "DBUS_ARRAY" : sigTypeOfDBus a
sigTypeOfDBus (D.DBusBoolean) = ["DBUS_BOOLEAN"]
sigTypeOfDBus (D.DBusByte) = ["DBUS_BYTE"]
sigTypeOfDBus (D.DBusString) = ["DBUS_STRING"]
sigTypeOfDBus (D.DBusInt16) = ["DBUS_INT16"]
sigTypeOfDBus (D.DBusInt32) = ["DBUS_INT32"]
sigTypeOfDBus (D.DBusInt64) = ["DBUS_INT64"]
sigTypeOfDBus (D.DBusWord16) = ["DBUS_UINT16"]
sigTypeOfDBus (D.DBusWord32) = ["DBUS_UINT32"]
sigTypeOfDBus (D.DBusWord64) = ["DBUS_UINT64"]

--sigTypeOfDBus (D.DBusArray a) = "DBUS_ARRAY" : sigTypeOfDBus a
fctTypeOfDBus (D.DBusBoolean) = "boolean"
fctTypeOfDBus (D.DBusByte) = "byte"
fctTypeOfDBus (D.DBusString) = "string"
fctTypeOfDBus (D.DBusInt16) = "int16"
fctTypeOfDBus (D.DBusInt32) = "int32"
fctTypeOfDBus (D.DBusInt64) = "int64"
fctTypeOfDBus (D.DBusWord16) = "uint16"
fctTypeOfDBus (D.DBusWord32) = "uint32"
fctTypeOfDBus (D.DBusWord64) = "uint64"

isArrayType (D.DBusArray _) = True
isArrayType _               = False
