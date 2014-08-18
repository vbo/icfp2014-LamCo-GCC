module Debug
( tShow
, tThis
) where

import Debug.Trace

-- Debug helpers
tShow :: forall a b. Show a => a -> b -> b
tShow a = traceShow a

tThis :: forall b. Show b => b -> b
tThis a = tShow a a

