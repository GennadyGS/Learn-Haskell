module LeapYearSpec where

import Test.Hspec
import LeapYear

spec :: Spec
spec = 
  describe "isLeapYear" $ do
    it "isLeapYear 2021 should be false" $
        isLeapYear 2021 `shouldBe` False

    it "isLeapYear 2020 should be true" $
        isLeapYear 2020 `shouldBe` True

    it "isLeapYear 1900 should be false" $
        isLeapYear 1900 `shouldBe` False

    it "isLeapYear 2000 should be true" $
        isLeapYear 2000 `shouldBe` True

main :: IO ()
main = hspec spec
