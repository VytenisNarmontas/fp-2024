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
  [ testCase "AddCar parsing" $
      Lib2.parseQuery "add car ABC123 Toyota Corolla 2020" @?= Right (Lib2.AddCar "ABC123" "Toyota" "Corolla" 2020, ""),
    testCase "RemoveCar parsing" $
      Lib2.parseQuery "remove car ABC123" @?= Right (Lib2.RemoveCar "ABC123", ""),
    testCase "ServiceCar parsing" $
      Lib2.parseQuery "service car ABC123 Oil 12-12-2022" @?= Right (Lib2.ServiceCar "ABC123" "Oil" "12-12-2022", ""),
    testCase "ListCars parsing" $
      Lib2.parseQuery "list cars" @?= Right (Lib2.ListCars, ""),
    testCase "ListServices parsing" $
      Lib2.parseQuery "list services ABC123" @?= Right (Lib2.ListServices "ABC123", ""),
    testCase "Invalid command parsing" $
      case Lib2.parseQuery "invalid command" of
        Left _ -> True @?= True
        Right _ -> False @?= True,
    testCase "State transition - Add car" $ do
      let initialState = Lib2.emptyState
          Right (newState, _) = Lib2.stateTransition initialState (Lib2.AddCar "ABC123" "Toyota" "Corolla" 2020)
      length (Lib2.cars newState) @?= 1,
    testCase "State transition - Service car" $ do
      let initialState = Lib2.State [Lib2.Car "ABC123" "Toyota" "Corolla" 2020] []
          Right (newState, _) = Lib2.stateTransition initialState (Lib2.ServiceCar "ABC123" "Oil" "12-12-2022")
      length (Lib2.services newState) @?= 1
  ]