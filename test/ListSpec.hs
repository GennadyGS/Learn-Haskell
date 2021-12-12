module ListSpec where

import List
import Test.Hspec

spec :: Spec
spec =
  describe "groupTuples" $ do
    it "groupTuples should return correct result" $
      let dic = [(1, "aa"), (1, "cc"), (2, "aa"), (3, "ff"), (3, "gg"), (1, "bb")]
          result = groupTuples dic
       in result `shouldBe` [(1, ["aa", "cc", "bb"]), (2, ["aa"]), (3, ["ff", "gg"])]

main :: IO ()
main = hspec spec
