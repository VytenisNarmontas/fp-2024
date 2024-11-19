module Lib2
    ( parseQuery
    , emptyState
    , stateTransition
    , Command(..)
    , Car(..)
    , Service(..)
    , State(..)
    ) where

import Data.List (find, delete)

-- Define the types for Car and Service
data Car = Car
  { carPlate :: String
  , carMake  :: String
  , carModel :: String
  , carYear  :: Int
  } deriving (Show, Eq)

data Service = Service
  { serviceCarPlate :: String
  , serviceType     :: String
  , serviceDate     :: String
  } deriving (Show)

-- Define the State of the system
data State = State
  { cars     :: [Car]
  , services :: [Service]
  } deriving (Show)

-- Define the commands that can be parsed
data Command
  = AddCar String String String Int        -- Plate, Make, Model, Year
  | RemoveCar String                       -- Plate
  | ServiceCar String String String        -- Plate, Service Type, Date
  | ListCars                               -- List all cars
  | ListServices String                    -- List services for a specific car
  deriving (Show)

-- The state transition function that handles commands
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

    ServiceCar plate sType date ->
        case findCar plate oldCars of
            Nothing -> Left "Car not found"
            Just _ -> Right (["Car serviced: " ++ plate], State oldCars (newService : oldServices))
                where newService = Service plate sType date

    ListCars -> 
        Right (map show oldCars, state)

    ListServices plate ->
        case findCar plate oldCars of
            Nothing -> Left "Car not found"
            Just _ -> Right (map show $ filter (\s -> serviceCarPlate s == plate) oldServices, state)

-- Helper function to find a car by plate
findCar :: String -> [Car] -> Maybe Car
findCar plate = find (\car -> carPlate car == plate)

-- <add_car> ::= "add" "car" <plate> <make> <model> <year>
parseAddCar :: String -> Either String Command
parseAddCar input =
    case words input of
        ("add" : "car" : plate : make : model : yearStr : _) ->
            case reads yearStr of
                [(year, "")] -> Right (AddCar plate make model year)
                _ -> Left "Invalid year format"
        _ -> Left "Invalid add car command"

-- <remove_car> ::= "remove" "car" <plate>
parseRemoveCar :: String -> Either String Command
parseRemoveCar input =
    case words input of
        ("remove" : "car" : plate : _) -> Right (RemoveCar plate)
        _ -> Left "Invalid remove car command"

-- <service_car> ::= "service" "car" <plate> <service_type> <date>
parseServiceCar :: String -> Either String Command
parseServiceCar input =
    case words input of
        ("service" : "car" : plate : sType : date : _) -> 
            Right (ServiceCar plate sType date)
        _ -> Left "Invalid service car command"

-- <list_cars> ::= "list" "cars"
parseListCars :: String -> Either String Command
parseListCars input =
    case words input of
        ("list" : "cars" : _) -> Right ListCars
        _ -> Left "Invalid list cars command"

-- <list_services> ::= "list" "services" <plate>
parseListServices :: String -> Either String Command
parseListServices input =
    case words input of
        ("list" : "services" : plate : _) -> Right (ListServices plate)
        _ -> Left "Invalid list services command"

-- <query> ::= <add_car> | <remove_car> | <service_car> | <list_cars> | <list_services>
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
    orElse (Left _) (Left _) = Left "Invalid command"

-- Initial empty state
emptyState :: State
emptyState = State [] []