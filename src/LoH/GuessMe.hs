module LoH.GuessMe where

import Introlude

guess :: Int -> Int -> Int
guess = undefined

printGuess :: Int -> Action ()
printGuess = undefined

guessMyNumber :: Int -> Int -> Action ()
guessMyNumber = undefined

startGame =
  runAction (
    (putTextAction "A number guessing game??? Awesome!")
    `andThen`
    (guessMyNumber 0 100)
  )

