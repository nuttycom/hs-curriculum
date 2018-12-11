module LoH.GuessMe where

import Introlude

-- | Implement a function that returns a guess that is half way between the two
-- arguments
guess :: Int -> Int -> Int
guess = undefined

-- | Given a number <n>, return an Action that will print out the text "My
-- guess is <n>." For example, `printGuess 10` will return an Action that when
-- run causes "My guess is 10" to be printed to the screen.
printGuess :: Int -> Action ()
printGuess = undefined

-- | This is the main game. It should be implemented as a recursive function
-- that takes as its arguments the lower and upper bounds of the number to be
-- guessed.
--
-- First, it should compute a guess using the 'guess' function , and
-- display it to the player using `'printGuess'.
--
-- Then, it should accept a line of input. If the input is "smaller" then it
-- should call itself recursively, setting the upper bound to the guess; if the
-- input is "bigger" then it should make the recursive call using the guess for
-- the lower bound. It should also accept the "quit" string, in which case it
-- prints "Goodbye!" and returns without calling itself; if any other input is
-- received, it should print an error and repeat the request for input.
guessMyNumber :: Int -> Int -> Action ()
guessMyNumber = undefined

-- | This is the main function used to start the game. It has already been
-- implemented for you; it demonstrates how to use the `andThen` function to
-- join together two actions.
startGame =
  runAction (
    (putTextAction "A number guessing game??? Awesome!")
    `andThen`
    (guessMyNumber 0 100)
  )

