import Test.Hspec
import Data.Array

data State = Dead | Alive
           deriving( Show, Eq )
type Index = (Int,Int)
type Alives = Int
type World = Array Index State

transition :: State -> Alives -> State
transition currentState alives =
    if currentState == Alive
    then if ( alives == 2 ) || ( alives == 3 )
         then Alive
         else Dead
    else if alives == 3
         then Alive
         else Dead

getNeighborIndices :: Index -> [Index]
getNeighborIndices idx@(row,col) =
    [ (row+y,col+x) | x <- [-1..1], y <- [-1..1], (y,x) /= (0,0) ]
                     
getAlivesOfNeighbor :: World -> Index -> Alives
getAlivesOfNeighbor w =
    length . ( filter isAlive ) . ( filter containsInWorld ) . getNeighborIndices
    where
      containsInWorld = inRange $ bounds w
      isAlive idx = (w ! idx) == Alive

nextCell :: World -> Index -> State
nextCell w idx =
    transition currentState alives
    where
      currentState = w ! idx
      alives       = getAlivesOfNeighbor w idx
    
nextStep :: World -> World
nextStep w =
    listArray bs $ map f cells
    where
      bs    = bounds w
      cells = indices w
      f     = nextCell w
    
w1 :: World
w1 = listArray ((1,1),(5,5))
     [Dead,Dead,Dead,Dead,Dead
     ,Dead,Alive,Alive,Alive,Dead
     ,Dead,Alive,Dead,Dead,Dead
     ,Dead,Dead,Alive,Dead,Dead
     ,Dead,Dead,Dead,Dead,Dead]

w2 :: World
w2 = listArray ((1,1),(5,5))
     [Dead,Dead,Alive,Dead,Dead
     ,Dead,Alive,Alive,Dead,Dead
     ,Dead,Alive,Dead,Alive,Dead
     ,Dead,Dead,Dead,Dead,Dead
     ,Dead,Dead,Dead,Dead,Dead]

nextStepTest = 
    describe "nextStep" $ do
      it "returns next world" $
         do
           (nextStep w1) `shouldBe` w2

nextCellTest =
    describe "nextCell" $ do
      it "returns next cell" $
         do
           (nextCell w1 (4,3)) `shouldBe` Dead

getAlivesOfNeighborTest =
    describe "getAlivesOfNeighbor" $ do
      it "returns 8-neighbor" $
         do
           (getAlivesOfNeighbor w1 (2,2)) `shouldBe` 2
                                          
      it "returns 8-neighbor of corner" $
         do
           (getAlivesOfNeighbor w1 (1,1)) `shouldBe` 1
                         
main = hspec $
       do
         nextStepTest
         nextCellTest
         getAlivesOfNeighborTest
