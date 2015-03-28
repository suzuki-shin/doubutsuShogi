module ListProc where

import Native.ListProc

log : String -> String
log = Native.ListProc.log

takeWhileString : (Char -> Bool) -> String -> String
takeWhileString = Native.ListProc.takeWhileString
