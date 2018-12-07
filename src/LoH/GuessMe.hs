module LoH.GuessMe where

import Introlude

guess :: Int -> Int -> Int
guess small big = divInt (addInt small big) 2

printGuess :: Int -> Action ()
printGuess i =
  putTextAction (concat ["My guess is ", renderInt i])

guessMyNumber :: Int -> Int -> Action ()
guessMyNumber small big =
  let latestGuess = guess small big

      doCommand :: Text -> Action ()
      doCommand command = case command of
        "smaller" -> guessMyNumber small latestGuess
        "bigger"  -> guessMyNumber latestGuess big
        "quit"    -> putTextAction "It was fun playing, goodbye!"
        other     ->
          (putTextAction (concat ["I didn't understand \"", other, "\", can you try again?"]))
          `andThen`
          (guessMyNumber small big)

   in (printGuess latestGuess)
      `andThen`
      (withActionResult getTextAction doCommand)

startGame =
  runAction (
    (putTextAction "A number guessing game??? Awesome!")
    `andThen`
    (guessMyNumber 0 100)
  )

