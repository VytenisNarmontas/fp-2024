{-# LANGUAGE ImportQualifiedPost #-}
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
      parseQuery "add car ABC123 Toyota Corolla 2020" @?= Right (AddCar "ABC123" "Toyota" "Corolla" 2020),
    testCase "RemoveCar parsing" $
      parseQuery "remove car ABC123" @?= Right (RemoveCar "ABC123"),
    testCase "ServiceCar parsing" $
      parseQuery "service car ABC123 Oil 12-12-2022" @?= Right (ServiceCar "ABC123" "Oil" "12-12-2022"),
    testCase "stateTransition AddCar" $
      let initialState = emptyState
          newState = stateTransition (AddCar "ABC123" "Toyota" "Corolla" 2020) initialState
      in cars newState @?= [Car "ABC123" "Toyota" "Corolla" 2020]
  ]
