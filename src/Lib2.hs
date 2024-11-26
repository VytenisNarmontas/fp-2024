module Lib2
    ( parseQuery
    , emptyState
    , stateTransition
    , Command(..)
    , Car(..)
    , Service(..)
    , ServiceType(..)
    , State(..)
    ) where

import Data.List (find, delete)
import Data.Char (isAlpha, isDigit)

-- Define the types for Car and Service
data Car = Car
  { carPlate :: String
  , carMake  :: String
  , carModel :: String
  , carYear  :: Int
  } deriving (Show, Eq)

data Service = Service
  { serviceCarPlate :: String
  , serviceTypes    :: [ServiceType]
  , serviceDate     :: String
  } deriving (Show)

data ServiceType = SimpleService String
                | NestedService String [ServiceType]
                deriving (Eq)

instance Show ServiceType where
    show (SimpleService s) = s
    show (NestedService s services) = 
        s ++ "(" ++ showServices services ++ ")"  -- Correctly formatted nested services

-- Show services as a properly formatted string
showServices :: [ServiceType] -> String
showServices [] = ""
showServices [x] = show x
showServices (x:xs) = show x ++ ", " ++ showServices xs

data State = State
  { cars     :: [Car]
  , services :: [Service]
  } deriving (Show)

data Command
  = AddCar String String String Int
  | RemoveCar String
  | ServiceCar String [ServiceType] String
  | ListCars
  | ListServices String
  deriving (Show)

stateTransition :: State -> Command -> Either String ([String], State)
stateTransition state@(State oldCars oldServices) cmd = case cmd of
    AddCar plate make model year -> 
        case findCar plate oldCars of
            Just _ -> Left "Car with this plate already exists"
            Nothing -> Right (["Car added: " ++ plate], State (newCar : oldCars) oldServices)
                where newCar = Car plate make model year
    
    RemoveCar plate -> 
        case findCar plate oldCars of
            Nothing -> Left "Car not found"
            Just car -> Right (["Car removed: " ++ plate], State (delete car oldCars) oldServices)

    ServiceCar plate serviceTypes date -> 
        case findCar plate oldCars of
            Nothing -> Left "Car not found"
            Just _ -> Right (["Car serviced: " ++ plate], State oldCars (Service plate serviceTypes date : oldServices))

    ListCars -> 
        Right (map show oldCars, state)

    ListServices plate -> 
        case findCar plate oldCars of
            Nothing -> Left "Car not found"
            Just _ -> Right (map show $ filter (\s -> serviceCarPlate s == plate) oldServices, state)

findCar :: String -> [Car] -> Maybe Car
findCar plate = find (\car -> carPlate car == plate)

parseQuery :: String -> Either String Command
parseQuery input = parseAddCar input
    `orElse` parseRemoveCar input
    `orElse` parseServiceCar input
    `orElse` parseListCars input
    `orElse` parseListServices input
  where
    orElse :: Either String Command -> Either String Command -> Either String Command
    orElse (Right res) _ = Right res
    orElse _ (Right res) = Right res
    orElse (Left msg1) (Left msg2) = Left (msg1 ++ " or " ++ msg2)
    orElse _ _ = Left "Invalid command"

parseAddCar :: String -> Either String Command
parseAddCar input =
    case words input of
        ("add" : "car" : plate : make : model : yearStr : _) -> 
            case reads yearStr of
                [(year, "")] -> Right (AddCar plate make model year)
                _ -> Left "Invalid year format"
        _ -> Left "Invalid add car command"

parseRemoveCar :: String -> Either String Command
parseRemoveCar input =
    case words input of
        ("remove" : "car" : plate : _) -> Right (RemoveCar plate)
        _ -> Left "Invalid remove car command"

parseServiceCar :: String -> Either String Command
parseServiceCar input = 
    case words input of
        ("service" : "car" : plate : rest) -> 
            let (serviceStr, dateStr) = splitAtLastSpace (unwords rest)
                services = parseServiceList serviceStr
            in if not (null dateStr)
                then Right (ServiceCar plate services (trim dateStr))
                else Left $ "Invalid service car command: missing date. serviceStr: " ++ serviceStr ++ ", dateStr: " ++ dateStr
        _ -> Left "Invalid service car command"

parseServiceList :: String -> [ServiceType]
parseServiceList input = 
    sanitizeParsedServices $ map (fst . parseServiceType . trim) (splitTopLevelServices input)

sanitizeParsedServices :: [ServiceType] -> [ServiceType]
sanitizeParsedServices = map sanitizeServiceType

sanitizeServiceType :: ServiceType -> ServiceType
sanitizeServiceType (SimpleService s) = SimpleService (trimUnmatched s)
sanitizeServiceType (NestedService s services) = 
    NestedService (trimUnmatched s) (sanitizeParsedServices services)

trimUnmatched :: String -> String
trimUnmatched = reverse . dropWhile (== ')') . reverse . dropWhile (== '(')

splitAtLastSpace :: String -> (String, String)
splitAtLastSpace input =
    case breakAtLastSpace input of
        (before, after) -> (trim before, trim after)

breakAtLastSpace :: String -> (String, String)
breakAtLastSpace str =
    let reversed = reverse str
        (revAfter, revBefore) = break (== ' ') reversed
    in (reverse revBefore, reverse revAfter)

splitTopLevelServices :: String -> [String]
splitTopLevelServices input = split 0 "" [] input
  where
    split _ curr acc [] = if null curr then acc else acc ++ [reverse curr]
    split depth curr acc (c:cs)
        | c == '(' = split (depth + 1) (c:curr) acc cs
        | c == ')' = split (depth - 1) (c:curr) acc cs
        | c == ',' && depth == 0 = split depth "" (acc ++ [reverse curr]) cs
        | otherwise = split depth (c:curr) acc cs

trim :: String -> String
trim = reverse . dropWhile (== ' ') . reverse . dropWhile (== ' ')

-- Parses a service type, handling both simple and nested cases
parseServiceType :: String -> (ServiceType, String)
parseServiceType input = 
    let input' = trim input
    in case break (== '(') input' of
        (name, "") -> (SimpleService (trim name), "")  -- Simple service
        (name, '(':rest) -> 
            let (services, remaining) = parseNestedList rest 1
            in (NestedService (trim name) services, trim remaining)
        _ -> error $ "Unexpected service format: " ++ input


-- Parses a nested list of services, handling nested parentheses correctly
parseNestedList :: String -> Int -> ([ServiceType], String)
parseNestedList input depth = go (trim input) depth []
  where
    go [] _ acc = (reverse acc, [])  -- End of input
    go (')':rest) 1 acc = (reverse acc, trim rest)  -- Close top-level nesting correctly
    go (')':rest) d acc = go rest (d - 1) acc  -- Handle unmatched `)`
    go ('(':rest) d acc = 
        let (service, remaining) = parseServiceType ('(':rest)
        in go remaining (d + 1) (service : acc)  -- Parse nested service
    go (',':rest) d acc = go rest d acc  -- Skip commas at the top level
    go str d acc = 
        let (service, remaining) = parseServiceType str
        in go remaining d (service : acc)  -- Parse the next service


parseListCars :: String -> Either String Command
parseListCars input =
    case words input of
        ("list" : "cars" : _) -> Right ListCars
        _ -> Left "Invalid list cars command"

parseListServices :: String -> Either String Command
parseListServices input =
    case words input of
        ("list" : "services" : plate : _) -> Right (ListServices plate)
        _ -> Left "Invalid list services command"

emptyState :: State
emptyState = State [] []