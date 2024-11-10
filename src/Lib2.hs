import qualified Data.Char as C
import qualified Data.List as L

type Parser a = String -> Either String (a, String)

-- Helper parsers
char :: Char -> Parser Char
char c [] = Left ("Cannot find " ++ [c] ++ " in an empty input")
char c (x:xs) = if c == x then Right (c, xs) else Left ([c] ++ " is not found at the start")

string :: String -> Parser String
string "" str = Right ("", str)
string (c:cs) str = do
  (_, rest) <- char c str
  (parsed, rest') <- string cs rest
  return (c : parsed, rest')

whitespace :: Parser String
whitespace input = Right (span C.isSpace input)

parseWord :: Parser String
parseWord input = 
  let (letters, rest) = span C.isLetter input
  in if null letters 
     then Left "Expected a word"
     else Right (letters, rest)

parseNumber :: Parser Int
parseNumber input = 
  let (digits, rest) = span C.isDigit input
  in if null digits 
     then Left "Expected a number"
     else Right (read digits, rest)

-- Specific parsers for car parameters
parseLicensePlate :: Parser String
parseLicensePlate = parseWord  -- Simplified; assumes license plate is a word

parseMake :: Parser String
parseMake = parseWord

parseModel :: Parser String
parseModel = parseWord

parseYear :: Parser Int
parseYear = parseNumber

-- and combinator for 5 parsers
and5' :: (a -> b -> c -> d -> e -> f) -> Parser a -> Parser b -> Parser c -> Parser d -> Parser e -> Parser f
and5' f pa pb pc pd pe = \input -> do
    (a, r1) <- pa input
    (b, r2) <- pb r1
    (c, r3) <- pc r2
    (d, r4) <- pd r3
    (e, r5) <- pe r4
    return (f a b c d e, r5)

-- Command parsers
parseAddCar :: Parser Query
parseAddCar = and5' AddCar (string "add car ") parseLicensePlate (string " ") parseMake (string " ") parseModel (string " ") parseYear

parseRemoveCar :: Parser Query
parseRemoveCar = do
    (_, rest1) <- string "remove car "
    (plate, rest2) <- parseLicensePlate rest1
    return (RemoveCar plate, rest2)

parseListCars :: Parser Query
parseListCars input = fmap (\(_, rest) -> (ListCars, rest)) (string "list cars" input)

parseServiceCar :: Parser Query
parseServiceCar = and5' ServiceCar (string "service car ") parseLicensePlate (string " ") parseWord (string " ") parseWord

parseListServices :: Parser Query
parseListServices = do
    (_, rest1) <- string "list services "
    (plate, rest2) <- parseLicensePlate rest1
    return (ListServices plate, rest2)

-- Combine all parsers in parseQuery
parseQuery :: Parser Query
parseQuery = parseAddCar `or2` parseRemoveCar `or2` parseListCars `or2` parseServiceCar `or2` parseListServices

-- State transition function
stateTransition :: Query -> State -> State
stateTransition (AddCar plate make model year) (State cars services) =
  State (Car plate make model year : cars) services
stateTransition (RemoveCar plate) (State cars services) =
  State (filter (\car -> licensePlate car /= plate) cars) services
stateTransition ListCars state = state
stateTransition (ServiceCar plate serviceType date) (State cars services) =
  State cars (Service plate serviceType date : services)
stateTransition (ListServices plate) state = state

-- Main function for testing
main :: IO ()
main = do
  print (parseQuery "add car ABC123 Toyota Corolla 2020") -- should parse as `AddCar`
  print (parseQuery "remove car ABC123") -- should parse as `RemoveCar`
  print (parseQuery "list cars") -- should parse as `ListCars`
  print (parseQuery "service car ABC123 Oil 12-12-2022") -- should parse as `ServiceCar`
