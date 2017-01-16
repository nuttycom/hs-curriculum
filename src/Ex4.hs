module Ex4 where

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

data ColorCount = CC Color Int

main :: IO ()
main = do
  l <- promptLands
  r <- promptColor Red
  g <- promptColor Green
  print (Red,   landProp <$> r <*> g <*> l)
  print (Green, landProp <$> g <*> r <*> l)

landProp :: (Fractional n) => n -> n -> n -> n
landProp primary other total = 
  let frac = primary / (primary + other)
  in  frac * total

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
