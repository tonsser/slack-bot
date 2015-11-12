module Misc
    ( mapFst
    , groupBy
    )
  where

import Import hiding (groupBy)
import qualified Data.Map as M
import Data.Map()

mapFst :: (a -> c) ->  (a, b) -> (c, b)
mapFst f (a, b) = (f a, b)

groupBy :: (Ord b) => (a -> b) -> [a] -> [(b, [a])]
groupBy f = M.assocs . foldr (\a -> M.insertWith (++) (f a) [a]) M.empty
