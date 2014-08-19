module Debug
( tShow
, tThis
) where

import Debug.Trace

-- Debug helpers
tShow :: Show a => a -> b -> b
tShow a = traceShow a

tThis :: Show b => b -> b
tThis a = tShow a a

