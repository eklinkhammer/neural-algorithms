module Util.Tuples
  (
    applyFst
  , applySnd
  ) where


applyFst :: (a -> c) -> (a,b) -> (c,b)
applyFst f (x,y) = (f x, y)

applySnd :: (b -> c) -> (a,b) -> (a,c)
applySnd f (x,y) = (x, f y)
