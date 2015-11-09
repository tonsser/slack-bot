module Misc
    ( mapFst
    )
  where

mapFst :: (a -> c) ->  (a, b) -> (c, b)
mapFst f (a, b) = (f a, b)
