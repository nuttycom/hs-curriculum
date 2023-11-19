{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Critter where

import Prelude 
  ( (.), (+), (++)
  , replicate, sum, fmap, pure, show, putStrLn, uncurry, zip
  , Show, Int, IO)
import Data.Foldable (traverse_)
import Data.Text (Text)

data CritterType = Dog | Cat | Human | Fish | Chicken | Duck deriving Show

legs :: CritterType -> Int
legs Dog = 4
legs Cat = 4
legs Human = 2
legs Fish = 0
legs Chicken = 2
legs Duck = 2

pets :: [CritterType]
pets = [Dog, Cat, Cat, Fish]

fowl :: [CritterType]
fowl = replicate 11 Chicken ++ replicate 2 Duck

human :: Text -> Critter
human n = Critter n Human

flipC :: CritterType -> Text -> Critter
flipC t n = Critter n t

people :: [Critter] 
people = fmap (flipC Human) ["Samantha", "Kristopher", "Sophia", "Erasmus", "Dada", "Nonna"]


-- family :: [CritterType]
-- family  = people ++ pets ++ fowl

allLegs :: [CritterType] -> Int
allLegs xs = sum (fmap legs xs)

data Critter = Critter 
  { name :: Text
  , critterType :: CritterType
  } deriving Show

printCritters :: [Critter] -> IO ()
printCritters cs = traverse_ (putStrLn . show) cs


