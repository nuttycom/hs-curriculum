module LoH.Game where

import           Introlude
import qualified Data.Map.Strict  as M

newtype Location where
  ToLoc :: { locName :: Text } -> Location
  deriving (Eq, Ord)

livingRoom :: Location
livingRoom = ToLoc { locName = "living-room" }

garden = ToLoc { locName = "garden" }
garden = undefined

attic = ToLoc { locName = "attic" }
attic = undefined

newtype Description where
  ToDesc :: { descText :: Text } -> Description

type Locations = M.Map Location Description

locations :: Locations
locations =
  M.fromList [ (livingRoom, ToDesc "You are in the living-room. A wizard is snoring loudly on the couch.")
             , (garden, ToDesc "You are in a beautiful garden. There is a well in front of you.")
             , (attic, ToDesc "You are in the attic. There is a giant welding torch in the corner.")
             ]

describeLocation :: Locations -> Location -> Maybe Description
describeLocation locs loc = M.lookup loc locs

data Path where
  ToPath :: { dest :: Location, dir :: Text, via :: Text } -> Path

type Paths = M.Map Location [Path]

paths :: Paths
paths =
  M.fromList [ (livingRoom, [ ToPath { dest = garden, dir = "west", via = "door" }
                            , ToPath { dest = attic,  dir = "upstairs", via = "ladder" }])
             , (garden,     [ ToPath { dest = livingRoom, dir = "east", via = "door" }])
             , (attic,      [ ToPath { dest = livingRoom, dir = "downstairs", via = "ladder" }])
             ]

describePath :: Path -> Text
describePath p =
  concat ["There is a ", via p," going ", dir p, " from here"]

describePathsFrom :: Paths -> Location -> [Text]
describePathsFrom ps loc =
  maybe
    ["I can't find that location!"]
    (\locPaths -> mapList describePath locPaths)
    (M.lookup loc ps)

newtype Item where
  ToItem :: { itemName :: Text } -> Item
  deriving (Eq, Ord)

type Items = M.Map Location [Item]

items :: Items
items =
  M.fromList [ (garden, mapList ToItem ["frog", "chain"])
             , (livingRoom, mapList ToItem ["whiskey", "bucket"])
             ]

data Environment where
  Env :: { currentLocation :: Location
         , envLocations :: Locations
         , envPaths :: Paths
         , envItems :: Items
         } -> Environment
