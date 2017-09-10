import Test.Hspec
import CashRegister


main = hspec $ do
   describe "Cash register" $ do
     it "multiply price by quantity" $ do
        multiply 4.5 5 `shouldBe` 22.5
     it "multiply price by quantity as Int" $do
        multiply 4.5 (6 :: Int) `shouldBe` 27.0 
