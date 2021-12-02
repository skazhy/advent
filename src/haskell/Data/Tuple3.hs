module Data.Tuple3 (butlast) where

butlast :: (a, b, c) -> (a, b)
butlast (x,y,_) = (x,y)
