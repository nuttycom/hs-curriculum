module Magic where

data Color 
  = White
  | Blue
  | Black
  | Red
  | Green
  | Colorless
  deriving (Eq, Show)

readColor :: String -> Maybe Color
readColor "white" = Just White
readColor "blue"  = Just Blue
readColor "black" = Just Black
readColor "red"   = Just Red
readColor "green" = Just Green
readColor "none"  = Just Colorless
readColor _ = Nothing

landName :: Color -> String
landName White = "plains"
landName Blue = "islands"
landName Black = "swamps"
landName Red = "mountains"
landName Green = "forests"
landName Colorless = "wastes"

data ColorCount = CC Color Int

main :: IO ()
main = do
  l <- promptLands
  r <- promptColor Red
  g <- promptColor Green
  putStrLn (concat ["Use ", show (landFrac <$> r <*> g <*> l), " ", landName Red])
  putStrLn (concat ["Use ", show (landFrac <$> g <*> r <*> l), " ", landName Green])

landFrac :: Double -> Double -> Double -> Int 
landFrac primary other total = 
  let frac = primary / (primary + other)
  in  round (frac * total)

promptColor :: (Read n) => Color -> IO (Maybe n)
promptColor c = do
  prompt (concat ["Enter the number of ", show c, " color symbols: "])

promptLands :: (Read n) => IO (Maybe n)
promptLands = do
  prompt "Enter the number of lands: "

prompt :: (Read n) => String -> IO (Maybe n)
prompt p = do
  putStr p
  s <- getLine
  return (safeRead s)

safeRead :: (Read n) => String -> Maybe n
safeRead s = case reads s of 
  [] -> Nothing
  [(f, _)] -> Just f
