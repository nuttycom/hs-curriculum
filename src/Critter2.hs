{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Critter where

import Prelude 
  ( (.), (+), (++)
  , replicate, sum, fmap, pure, show, putStrLn, uncurry, zip
  , Show, Int, IO)
import Data.Foldable (traverse_)
import Data.Text (Text)

data Species = Dog | Cat | Human | Fish | Chicken | Duck deriving Show

legs :: Species -> Int
legs Dog = 4
legs Cat = 4
legs Human = 2
legs Fish = 0
legs Chicken = 2
legs Duck = 2

pets :: [Species]
pets = [Dog, Cat, Cat, Fish]

fowl :: [Species]
fowl = replicate 11 Chicken ++ replicate 2 Duck

people :: [Species] 
people = replicate 4 Human

family :: [Species]
family  = people ++ pets ++ fowl

allLegs :: [Species] -> Int
allLegs xs = sum (fmap legs xs)

data Critter = Critter 
  { species :: Species
  , name :: Text
  } deriving Show

names :: [Text]
names = ["Erasmus", "Sophia", "Edrik", "Iluvatar"]

species' :: [Species]
species' = [Human, Human, Dog, Cat]

funny :: [Critter]
funny = do
  n <- names
  s <- species'
  pure (Critter s n)

correct :: [Critter]
correct = fmap (uncurry Critter) (zip species' names)

printCritters :: [Critter] -> IO ()
printCritters = traverse_ (putStrLn . show)
