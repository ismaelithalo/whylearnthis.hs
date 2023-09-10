module Main where
import Test.Hspec
import Exercises

main :: IO ()
main = hspec $ do
  exercicesTests

exercicesTests = do
  describe "exercises-addTwoValues" $ do
    it "1 and 1" $ do
      addTwoValues 1 1 `shouldBe` 2
    it "0 and 1" $ do
      addTwoValues 0 1 `shouldBe` 1
    it "1 and -1" $ do
      addTwoValues 1 (-1) `shouldBe` 0
    it "9 and 7" $ do
      addTwoValues 9 7 `shouldBe` 16
  describe "exercises-isDivisible" $ do
    it "7 and 2" $ do
      isDivisible 7 2 `shouldBe` False
    it "168 and 7" $ do
      isDivisible 168 7`shouldBe` True
    it "28 and 3" $ do
      isDivisible 28 3 `shouldBe` False
    it "189 and 21" $ do
      isDivisible 189 21 `shouldBe` True
  describe "exercises-divisionRemainder" $ do
    it "11 and 5" $ do
      divisionRemainder 11 5 `shouldBe` (2,1)
    it "21 and 7" $ do
      divisionRemainder 21 7 `shouldBe` (3,0)
    it "29 and 3" $ do
      divisionRemainder 29 3 `shouldBe` (9,2)
    it "3 and 12" $ do
      divisionRemainder 3 12 `shouldBe` (0,3)
  describe "exercises-torricelli" $ do
    it "v0 = 0, a = 10 and deltaS = 5" $ do
      torricelli 0 10 5 `shouldBe` 10
    it "v0 = 2, a = 4 and deltaS = 4" $ do
      torricelli 2 4 4 `shouldBe` 6
    it "v0 = 1, a = 20 and deltaS = 2" $ do
      torricelli 1 20 2 `shouldBe` 9
    it "v0 = 3, a = 7 and deltaS = 8" $ do
      torricelli 3 7 8 `shouldBe` 11
  describe "exercises-momentum" $ do
    it "v0 = 0, a = 10 and deltaS = 5" $ do
      momentum 2 0 10 5 `shouldBe` 20
    it "v0 = 2, a = 4 and deltaS = 4" $ do
      momentum 9 2 4 4 `shouldBe` 54
    it "v0 = 1, a = 20 and deltaS = 2" $ do
      momentum 11 1 20 2 `shouldBe` 99
    it "v0 = 3, a = 7 and deltaS = 8" $ do
      momentum 5 3 7 8 `shouldBe` 55
  describe "exercises-sumVector" $ do
    it "(2,3) and (11,9)" $ do
      sumVector (2,3) (11,9) `shouldBe` (13,12)
    it "(1,1) and (14,5)" $ do
      sumVector (1,1) (14,5) `shouldBe` (15,6)
    it "(11,23) and (-1,8)" $ do
      sumVector (11,23) (-1,8) `shouldBe` (10,31)
    it "(9,1) and (-3,5)" $ do
      sumVector (9,1) (-3,5) `shouldBe` (6,6)
  describe "exercises-vectorOperator" $ do
    it "(2,3) add coordinates (11,9)" $ do
      vectorOperator (2,3) (\(x0,y0) (x1,y1) -> (x0+x1,y0+y1)) (11,9) `shouldBe` (13,12)
    it "(5,11) scalar product (7,8)" $ do
      vectorOperator (5,11) (\(x0,y0) (x1,y1) -> (x0*x1,y0*y1)) (7,8) `shouldBe` (35,88)
    it "(5,11) exponent (3,2)" $ do
      vectorOperator (5,11) (\(x0,y0) (x1,y1) -> (x0**x1,y0**y1)) (3,2) `shouldBe` (125,121)
    it "(2,3) determinant (11,9)" $ do
      vectorOperator (15,4) (\(x0,y0) (x1,y1) -> (x0*y1 - y0*x1, 0)) (7,2) `shouldBe` (2,0)
  describe "exercises-first5Average" $ do
    it "[10,23,42,11,4]" $ do
      first5Average [10,23,42,11,4] `shouldBe` 18
    it "[0,-11,3,34,-6]" $ do
      first5Average [0,-11,3,34,-6] `shouldBe` 4
    it "[0,0,0,0,0]" $ do
      first5Average [0,0,0,0,0] `shouldBe` 0
    it "[1..]" $ do
      first5Average [1..] `shouldBe` 3
  describe "exercises-headsFromList" $ do
    it "[13,2,9,87] [1,3,11]" $ do
      headsFromList [13,2,9,87] [1,3,11] `shouldBe` (13,1)
    it "[1] [2]" $ do
      headsFromList [1] [2] `shouldBe` (1,2)
  describe "exercises-fibonacci" $ do
    it "n=0" $ do
      fibonacci 0 `shouldBe` 0
    it "n=1" $ do
      fibonacci 1 `shouldBe` 1
    it "n=6" $ do
      fibonacci 6 `shouldBe` 8
  describe "exercises-get7Multiples" $ do
    it "[22]" $ do
      get7Multiples [22::Int] `shouldBe` []
    it "[21,84,37,4,33]" $ do
      get7Multiples [21::Int,84::Int,37::Int,4::Int,33::Int] `shouldBe` [21::Int,84::Int]
    it "[23,43,11,2,3]" $ do
      get7Multiples [23::Int,43::Int,11::Int,2::Int,3::Int] `shouldBe` ([] :: [Int]) 
    it "[7,14,21,28,35]" $ do
      get7Multiples [7::Int,14::Int,21::Int,28::Int,35::Int] `shouldBe` [7::Int,14::Int,21::Int,28::Int,35::Int]
  describe "exercises-vectorModule" $ do
    it "[(3,4),(5,12),(7,24),(8,15),(9,40)]" $ do
      vectorModule [(3,4),(5,12),(7,24),(8,15),(9,40)] `shouldBe` [5::Int,13::Int,25::Int,17::Int,41::Int]
    
  
