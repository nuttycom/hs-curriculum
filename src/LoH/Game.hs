module LoH.Game where

import           Introlude
import qualified Data.Map.Strict  as M

newtype Location where
  ToLoc :: { locName :: Text } -> Location
  deriving (Eq, Ord)

livingRoom :: Location
livingRoom = undefined

garden :: Location
garden = undefined

attic :: Location
attic = undefined

-- newtype Description where
--   ToDesc :: { descText :: Text } -> Description
--
-- type Locations = M.Map Location Description
--
-- locations :: Locations
-- locations = undefined
--
-- describeLocation :: Locations -> Location -> Maybe Description
-- describeLocation = undefined
--
-- data Path where
--   ToPath :: { dest :: Location, dir :: Text, via :: Text } -> Path
--
-- type Paths = M.Map Location [Path]
--
-- paths :: Paths
-- paths = undefined
--
-- describePath :: Path -> Text
-- describePath = undefined
--
-- describePathsFrom :: Paths -> Location -> [Text]
-- describePathsFrom = undefined
--
-- newtype Item where
--   ToItem :: { itemName :: Text } -> Item
--   deriving (Eq, Ord)
--
-- type Items = M.Map Location [Item]
--
-- items :: Items
-- items = undefined
--
-- data Environment where
--   Env :: { currentLocation :: Location
--          , envLocations :: Locations
--          , envPaths :: Paths
--          , envItems :: Items
--          } -> Environment


