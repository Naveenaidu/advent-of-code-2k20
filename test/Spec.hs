import Test.Hspec

import qualified Day01

main :: IO ()
main = hspec $ do
  describe "Advent of Code 2020, Day 1" $ do
    it "Part 1" $ do 
      let x = [1721,979,366,299,675,1456]

      Day01.day01a' x `shouldBe` 514579
      Day01.day01b' x `shouldBe` 241861950
