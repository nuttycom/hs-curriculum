{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | An extremely minimal prelude for children learning Haskell.
--
-- This prelude provides monomorphized versions of a number of
-- commonly used polymorphic functions. This is intended to give
-- the learner a very concrete environment in which to become familiar
-- with the language.
module Introlude
  ( -- IO
    Action
  , putTextAction
  , getTextAction
--  , toAction
  , mapAction
  , composeActionFns
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

-- | An Action represents a restricted I/O operation. For introductory
-- purposes, the only I/O operations supported are reading lines from
-- stdin and writing lines to stdout.
newtype Action a = Action { runAction :: IO a }
  deriving (P.Functor, P.Applicative, P.Monad)

instance P.Show (Action a) where
  show _ = "You constructed an Action value. You have to use the 'runAction' \
           \function to actually run it in the REPL to see what it does."

--toAction :: a -> Action a
--toAction = P.pure

-- | Construct an Action that, when you run it, will print the provided
-- line of text to the screen.
putTextAction :: Text -> Action ()
putTextAction t = Action (P.putStrLn (T.unpack t))

-- | Construct an Action that, when you run it, will read one line of
-- input from the console. The program will pause while you type in text;
-- when you hit \<Enter\> it will continue. You can use 'composeActionFns'
-- or 'withActionResult' to actually use the text that was read in inside
-- your program.
getTextAction :: Action Text
getTextAction = Action (P.fmap T.pack P.getLine)

-- | Create a new action that first performs the first action, then performs
-- the second when run. Most programs need to do more than one thing that
-- requires input or output, so this is a way to join together multiple
-- actions so that they both happen when the resulting /composite/ 'Action' is run.
andThen :: Action () -> Action b -> Action b
andThen = (P.>>)

mapAction :: (a -> b) -> Action a -> Action b
mapAction = P.fmap

composeActionFns :: (a -> Action b) -> (b -> Action c) -> (a -> Action c)
composeActionFns = (P.>=>)

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


--{-# INLINE toAction #-}
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
