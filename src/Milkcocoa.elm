module Milkcocoa where

import Signal
import Native.Milkcocoa

-- milkcocoa : String -> a
-- milkcocoa = Native.Milkcocoa.milkcocoa

-- dataStore : String -> String -> String ->
-- dataStore = Native.Milkcocoa.dataStore

push : a -> Signal ()
push = Native.Milkcocoa.push


-- child : b -> String -> b
-- child  = Native.Milkcocoa.child

-- on : b -> String -> 
-- on 


-- log : String -> String
-- log = Native.Milkcocoa.log

-- takeWhileString : (Char -> Bool) -> String -> String
-- takeWhileString = Native.Milkcocoa.takeWhileString
