import Test.Hspec
import CashRegister2


main = hspec $ do
   describe "Cash register" $ do
     it "multiply price by quantity" $ do
        (Price 4.5) `multiply` (Quantity 5) `shouldBe` Price 22.5

     it "empty price is zero" $ do
        mempty `shouldBe` Price 0
     
     it "append two price" $ do
        (Price 10.0) `mappend` (Price 20.0) `shouldBe` Price 30.0

     it "concat prices" $ do
        mconcat [Price 12.0, Price 13.0, Price 14.0] `shouldBe` Price 39.0
     
     it "add two price" $ do
        (Price 10.0) `add` (Price 20.0) `shouldBe` Price 30.0

     it "total prices" $ do
        total [Price 12.0, Price 13.0, Price 14.0] `shouldBe` Price 39.0
     
