{-# LANGUAGE ImportQualifiedPost #-}
module Main (main) where

import Test.Tasty ( TestTree, defaultMain, testGroup )
import Test.Tasty.HUnit ( testCase, (@?=) )

import Lib1 qualified
import Lib2 qualified

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests]

unitTests :: TestTree
unitTests = testGroup "Lib2 Tests"
  [ testGroup "parseQuery Tests"
      [ testCase "AddCar parsing" $
          Lib2.parseQuery "add car ABC123 Toyota Corolla 2020" @?= Right (Lib2.AddCar "ABC123" "Toyota" "Corolla" 2020),
        testCase "RemoveCar parsing" $
          Lib2.parseQuery "remove car ABC123" @?= Right (Lib2.RemoveCar "ABC123"),
        testCase "ServiceCar parsing with nested services" $
          Lib2.parseQuery "service car ABC123 oil change 2024-01-01" @?= 
            Right (Lib2.ServiceCar "ABC123" [Lib2.SimpleService "oil change"] "2024-01-01"),
        testCase "ListCars parsing" $
          Lib2.parseQuery "list cars" @?= Right Lib2.ListCars,
        testCase "ListServices parsing" $
          Lib2.parseQuery "list services ABC123" @?= Right (Lib2.ListServices "ABC123"),
        testCase "Invalid command parsing" $
          case Lib2.parseQuery "invalid command" of
            Left _ -> True @?= True
            Right _ -> False @?= True
      ],
    testGroup "stateTransition Tests"
      [ testCase "State transition - Add car" $ do
          let initialState = Lib2.emptyState
              Right (msgs, newState) = Lib2.stateTransition initialState (Lib2.AddCar "ABC123" "Toyota" "Corolla" 2020)
          msgs @?= ["Car added: ABC123"]
          length (Lib2.cars newState) @?= 1,
        testCase "State transition - Add duplicate car" $ do
          let initialState = Lib2.State [Lib2.Car "ABC123" "Toyota" "Corolla" 2020] []
              result = Lib2.stateTransition initialState (Lib2.AddCar "ABC123" "Toyota" "Corolla" 2020)
          result @?= Left "Car with this plate already exists",
        testCase "State transition - Remove car" $ do
          let initialState = Lib2.State [Lib2.Car "ABC123" "Toyota" "Corolla" 2020] []
              Right (msgs, newState) = Lib2.stateTransition initialState (Lib2.RemoveCar "ABC123")
          msgs @?= ["Car removed: ABC123"]
          length (Lib2.cars newState) @?= 0,
        testCase "State transition - Service car" $ do
          let initialState = Lib2.State [Lib2.Car "ABC123" "Toyota" "Corolla" 2020] []
              Right (msgs, newState) = Lib2.stateTransition initialState (Lib2.ServiceCar "ABC123" [Lib2.SimpleService "oil change"] "2024-01-01")
          msgs @?= ["Car serviced: ABC123"]
          length (Lib2.services newState) @?= 1,
        testCase "State transition - List cars" $ do
          let initialState = Lib2.State [Lib2.Car "ABC123" "Toyota" "Corolla" 2020, Lib2.Car "XYZ987" "Ford" "Focus" 2010] []
              Right (msgs, _) = Lib2.stateTransition initialState Lib2.ListCars
          length msgs @?= 2
      ]
  ]