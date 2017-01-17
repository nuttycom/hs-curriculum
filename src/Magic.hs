{-# LANGUAGE ScopedTypeVariables #-}
module Magic where

{- Create a data type whose constructors represent the different colors
 - of mana. `Color` is the *type* that we are defining, and `White`, 
 - `Blue`, `Black` etc. are each 0-argument functions that construct different
 - values of the type `Color`
 -}
data Color 
  = White
  | Blue
  | Black
  | Red
  | Green
  | Colorless
  deriving (Eq, Show)

{- `main` is the primary entry point for this program. To run the program, first
 - open the Haskell interpreter by typing `stack ghci` at the terminal prompt
 - from the directory containing this file. Then, when you see the `Prelude>`
 - prompt, you can type:
 -
 - Prelude> :l Magic.hs
 -
 - to load this file. Once the file has loaded (which includes compilation; if
 - there are any errors, they will appear here) you can type:
 -
 - *Magic> main
 -
 - to run the program.
 -}
main :: IO ()
main = do
  l <- promptLandCount
  r <- promptSymbolCount Red
  g <- promptSymbolCount Green
  putStrLn (concat ["Use ", show (landFrac <$> r <*> g <*> l), " ", landName Red])
  putStrLn (concat ["Use ", show (landFrac <$> g <*> r <*> l), " ", landName Green])

{- Prompt the user for total number of lands to include in their deck. In
 - a 60-card deck, 24 is a good starting point for this value; in a 40-card
 - deck, try 17.k
 -}
promptLandCount :: IO (Maybe Int)
promptLandCount = do
  prompt "Enter the total number of lands you want to have in your deck: "

{- Prompt the user for the count of symbols for the specified color.
 -}
promptSymbolCount :: Color -> IO (Maybe Int)
promptSymbolCount c = do
  prompt (concat ["Enter the number of ", show c, " color symbols: "])

{- This function performs the central arithmetic logic of the program
 - by determining what percentage of the overall land population needs
 - to be of the 'primary' color, and then multiplying that percentage
 - by the total desired number of lands.
 -}
landFrac :: Int -- the count of color symbols for the color for which you want a land count
         -> Int -- the total count of color symbols which are *not* the primary color
         -> Int -- the total number of lands desired for the final deck
         -> Int -- the result will be the number of lands of the primary color that you should use
landFrac primary other lands = 
  let primary' :: Double -- convert the number of primary-color symbols to a double-precision floating point value, for division
      primary' = fromIntegral primary 
      
      other' :: Double -- convert the number of other mana symbols to a double-precision floating point value, for division
      other' = fromIntegral other

      frac :: Double -- the overall fraction of primary lands is a double-precision floating point value
      frac = primary' / (primary' + other')

  in  round (frac * fromIntegral lands) -- round the result to an integer value

{- A convenience function for displaying the basic land type for a each color.
 - Note that there is one implementation of this function for each color's
 - constructor - this is called "pattern matching"
 -}
landName :: Color -> String
landName White = "plains"
landName Blue = "islands"
landName Black = "swamps"
landName Red = "mountains"
landName Green = "forests"
landName Colorless = "wastes"

{- For future use: this function will "parse" a string to turn it into
 - a color value. Use this if you want to add functionality to prompt
 - the user for what color they want to enter.
 -}
readColor :: String -> Maybe Color
readColor "white" = Just White
readColor "blue"  = Just Blue
readColor "black" = Just Black
readColor "red"   = Just Red
readColor "green" = Just Green
readColor "none"  = Just Colorless
readColor _ = Nothing

{- A utility function for prompting the user for a value. This eliminates
 - the need for us to duplicate code for different prompts.
 -}
prompt :: (Read n) => String -> IO (Maybe n)
prompt p = do
  putStr p
  s <- getLine
  return (safeRead s)

{- This function is a helper to ensure that if the user enters a bad
 - value, the program terminates cleanly rather than failing with an
 - error that may be difficult to understand. We will improve on this
 - function in a future version.
 -}
safeRead :: (Read n) => String -> Maybe n
safeRead s = case reads s of 
  [] -> Nothing
  [(f, _)] -> Just f
