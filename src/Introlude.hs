{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Introlude
  ( -- IO
    Action
  , putTextAction
  , getTextAction
  , toAction
  , mapAction
  , composeActions
  , withActionResult
  , andThen
  , runAction
  -- comparisons
  , P.Eq
  , P.Ord
  -- Ints
  , Int
  , addInt
  , divInt
  , renderInt
  -- Text
  , Text
  , T.concat
  , T.intercalate
  -- List
  , toList
  , mapList
  , withListElements
  -- Maybe
  , P.Maybe(..)
  , toMaybe
  , P.fromMaybe
  , P.maybe
  , P.undefined
  ) where

import qualified Prelude as P
import           Prelude (IO, Int, (.))

import qualified Control.Monad as P

import qualified Data.Maybe as P
import qualified Data.Text as T
import           Data.Text (Text)

newtype Action a = Action { runAction :: IO a }
  deriving (P.Functor, P.Applicative, P.Monad)

toAction :: a -> Action a
toAction = P.pure

putTextAction :: Text -> Action ()
putTextAction t = Action (P.putStrLn (T.unpack t))

getTextAction :: Action Text
getTextAction = Action (P.fmap T.pack P.getLine)

andThen :: Action a -> Action b -> Action b
andThen = (P.>>)

mapAction :: (a -> b) -> Action a -> Action b
mapAction = P.fmap

composeActions :: (a -> Action b) -> (b -> Action c) -> (a -> Action c)
composeActions = (P.>=>)

withActionResult :: Action a -> (a -> Action b) -> Action b
withActionResult = (P.>>=)

-------

addInt :: Int -> Int -> Int
addInt = (P.+)

divInt :: Int -> Int -> Int
divInt = P.div

renderInt :: Int -> Text
renderInt = T.pack . P.show

toList :: a -> [a]
toList = P.pure

mapList :: (a -> b) -> [a] -> [b]
mapList = P.fmap

withListElements :: (a -> [b]) -> [a] -> [b]
withListElements = (P.=<<)

toMaybe :: a -> P.Maybe a
toMaybe = P.pure


{-# INLINE toAction #-}
{-# INLINE putTextAction #-}
{-# INLINE getTextAction #-}
{-# INLINE andThen #-}
{-# INLINE withActionResult #-}
{-# INLINE mapAction #-}
{-# INLINE addInt #-}
{-# INLINE divInt #-}
{-# INLINE renderInt #-}
{-# INLINE toList #-}
{-# INLINE mapList #-}
{-# INLINE withListElements #-}
{-# INLINE toMaybe #-}
