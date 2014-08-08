module Debug
( tShow
, tThis
) where

import Debug.Trace

-- Debug helpers
tShow a = traceShow a
tThis a = tShow a a

