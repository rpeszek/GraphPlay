--
-- SHARED HELPERS
-- various helpers (typically exist defined somewhere else but wanted to limit library dependencies)
--

module GraphPlay.Helpers where

second' :: (a,b) -> b
second' (_,x) = x

first' :: (a,b) -> a
first' (x,_) = x
