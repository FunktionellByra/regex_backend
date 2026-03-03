module DMap
    ( empty
    , add
    , insert
    , lookup
    , toList
    , create
    , keys
    , union
    , toString
    , DefaultMap
    ) where

import Data.Map (Map)
import Prelude hiding (lookup) -- clashes with export @DMap.lookup@
import qualified Data.Map as Map

-- @DefaultMap@ is a wrapper around @Data.Map@ which always comes with a default
-- value; i.e. @DefaultMap.lookup@ always return a value, not `Maybe v`.
type DefaultMap k v = (Map k v, v)

empty :: v -> DefaultMap k v
empty d = (Map.empty, d)

create :: Map k v -> v -> DefaultMap k v
create m v = (m, v)

add :: Ord k => k -> v -> DefaultMap k v -> DefaultMap k v
add k v = insert k (const v)

-- @insert@ is a more general case of @add@ that allows to to modify the value with
-- a function of type @v -> v@ when inserting
insert :: Ord k => k -> (v -> v) -> DefaultMap k v -> DefaultMap k v
insert k f (map, d) = case Map.lookup k map of
    Nothing    -> (Map.insert k (f d) map, d)
    Just found -> (Map.insert k (f found) map, d)

delete :: Ord k => k -> DefaultMap k v -> DefaultMap k v
delete k (map, d) = (Map.delete k map, d)

lookup :: Ord k => k -> DefaultMap k v -> v
lookup k (m, d) = case Map.lookup k m of
    Nothing    -> d
    Just found -> found

toList :: Ord k => DefaultMap k v -> [(k, v)]
toList = Map.toList . fst

keys :: Ord k => DefaultMap k v -> [k]
keys = Map.keys . fst

toString :: (Show k, Show v) => DefaultMap k v -> String 
toString (m, _) = show m

map :: (a -> b) -> b -> DefaultMap k a -> DefaultMap k b
map f newDefault (map, _) = (Map.map f map, newDefault)

union :: Ord k => DefaultMap k v -> DefaultMap k v -> DefaultMap k v
union (map1, d) (map2, _) = (Map.union map1 map2, d) 