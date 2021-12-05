module Utils where

import Debug.Trace

display :: Show a => a -> a
display x = (trace $ show x) x