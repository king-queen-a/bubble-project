module Test.Lib where

import Test.QuickCheck
--import Test.QuickCheck.All
--import Test.QuickCheck.Modifiers
import Lib
import System.Random


-- test distance
prop_distance :: ((Float, Float),Int) -> ((Float, Float),Int) -> Bool
prop_distance ((x1,y1),c1) ((x2,y2),c2) = 
    distance ((x1,y1),c1) ((x2,y2),c2) == distance ((y1,x1),c1) ((y2,x2),c2) && distance ((x1,y1),c1) ((x2,y2),c2) == distance ((x2,y2),c2) ((x1,y1),c1)
    && distance ((x1,y1),c1) ((x2,y2),c2) >=0

prop_distanceT :: ((Float, Float),Int) -> (Float, Float) -> Bool
prop_distanceT ((x1,y1),c) (x2,y2) = 
    distanceT ((x1,y1),c) (x2,y2) == distanceT ((y1,x1),c) (y2,x2) && distanceT ((x1,y1),c) (x2,y2) == distanceT ((x2,y2),c) (x1,y1)
    && distanceT ((x1,y1),c) (x2,y2) >= 0

prop_emptyBalls :: [((Float, Float), Int)] -> Int -> Bool
prop_emptyBalls balls rand = not (null (emptyBalls balls (mkStdGen rand)))

prop_colorBall :: Int -> Bool
prop_colorBall rand = fst (colorBall (mkStdGen rand)) <= 7 && fst (colorBall (mkStdGen rand)) >= 1

angleTable1 :: [Float]
angleTable1 = [-70..70]
angleTable2 :: [Float]
angleTable2 = map (+0.5) [-70..69]
angleTable :: [Float]
angleTable = angleTable1 ++ angleTable2

tablePomoc :: [Float]
tablePomoc = [0..]
tabc :: [Int]
tabc = [1,2,3,4,5,6,7,1,2,3,4,5,6,7,1,2,3,4,5,6,7,1,2,3,4,5,6,7,1,2,3,4,5,6,7,1,2,3,4,5,6,7,1,2,3,4,5,6,7,1,2,3,4,5,6,7,1,2,3,4,5,6,71,2,3,4,5,6,7,
    1,2,3,4,5,6,7,1,2,3,4,5,6,7,1,2,3,4,5,6,7,1,2,3,4,5,6,7,1,2,3,4,5,6,7,1,2,3,4,5,6,7,1,2,3,4,5,6,7,1,2,3,4,5,6,7,1,2,3,4,5,6,7,1,2,3,4,5,6,7,
    1,2,3,4,5,6,7,1,2,3,4,5,6,7,1,2,3,4,5,6,7,1,2,3,4,5,6,7,1,2,3,4,5,6,7,1,2,3,4,5,6,7,1,2,3,4,5,6,7,1,2,3,4,5,6,7,1,2,3,4,5,6,7,1,2,3,4,5,6,7,
    1,2,3,4,5,6,7,1,2,3,4,5,6,7,1,2,3,4,5,6,7,1,2,3,4,5,6,7,1,2,3,4,5,6,7,1,2,3,4,5,6,7,1,2,3,4,5,6,7,1,2,3,4,5,6,7,1,2,3,4,5,6,7,1,2,3,4,5,6,7,
    1,2,3,4,5,6,7,1,2,3,4,5,6,7,1,2,3,4,5,6,7,1,2,3,4,5,6,7,1,2,3,4,5,6,7,1,2,3,4,5,6,7,1,2,3,4,5,6,7,1,2,3,4,5,6,7,1,2,3,4,5,6,7,1,2,3,4,5,6,7,
    1,2,3,4,5,6,7,1,2,3,4,5,6,7,1,2,3,4,5,6,7,1,2,3,4,5,6,7,1,2,3,4,5,6,7,1,2,3,4,5,6,7,1,2,3,4,5,6,7,1,2,3,4,5,6,7,1,2,3,4,5,6,7,1,2,3,4,5,6,7,
    1,2,3,4,5,6,7,1,2,3,4,5,6,7,1,2,3,4,5,6,7,1,2,3,4,5,6,7,1,2,3,4,5,6,7,1,2,3,4,5,6,7,1,2,3,4,5,6,7,1,2,3,4,5,6,7,1,2,3,4,5,6,7,1,2,3,4,5,6,7]

instance Arbitrary Action where
    arbitrary = elements [NoA, RightA, LeftA]


instance Show Game where
    --show gameBalls = " "
    --show gameBall = " "
    --show gameOver = " "
    --show gameScore = " "
    --show gameAction = " "
    --show gameAngle = " "
    --show gameShoot  = " "
    --show pomoc = " "
    --show gameTable = " "
    --show gameRandom = " "
    --show defaultBalls = " "

instance Arbitrary Game where
    arbitrary = do
        ballx <- arbitrary
        bally <- arbitrary
        ballc <- chooseInt (1,7)
        ballspos <-sublistOf table1 
        ballsc <- sublistOf tabc
        ameOver <- elements [True,False]
        ameScore <- arbitrary
        ameAction <- elements [NoA, RightA, LeftA]
        ameAngle <- arbitrary
        ameShoot <- elements [True,False]
        omoc <- elements tablePomoc
        ameLicznik <- arbitrary
        ameTable <- sublistOf table1
        rand <- choose (1,500)
        ameRecord <- arbitrary
        return $ Game ((ballx,bally),ballc) (ameBalls ballspos ballsc) ameOver ameScore ameAction ameAngle ameShoot omoc ameLicznik ameTable (mkStdGen rand) ameRecord efaultBalls
        where
            ameBalls ballspos ballsc = zip ballspos ballsc
            efaultBalls = [((1,1),1),((2,1),1),((3,1),2),((4,1),2),((5,1),3),((6,1),1),((7,1),4),((8,1),4),((9,1),5),((10,1),5),((11,1),5),((12,1),6)]
            


prop_angleMove :: Action -> Float -> Int -> Bool
prop_angleMove act ang rand 
    | act == LeftA = if ang <= 70 then gameAngle (angleMove (Game ((6.5,24),1) [((1,1),1)] False 0 act ang False 0 0 table1 (mkStdGen rand) 10 [((1,1),1)])) == ang+0.5
        else gameAngle (angleMove (Game ((6.5,24),1) [((1,1),1)] False 0 act ang False 0 0 table1 (mkStdGen rand) 10 [((1,1),1)])) == ang
    | act == RightA = if ang >= -70 then gameAngle (angleMove (Game ((6.5,24),1) [((1,1),1)] False 0 act ang False 0 0 table1 (mkStdGen rand) 10 [((1,1),1)])) == ang-0.5
        else gameAngle (angleMove (Game ((6.5,24),1) [((1,1),1)] False 0 act ang False 0 0 table1 (mkStdGen rand) 10 [((1,1),1)])) == ang
    | otherwise = gameAngle (angleMove (Game ((6.5,24),1) [((1,1),1)] False 0 act ang False 0 0 table1 (mkStdGen rand) 10 [((1,1),1)])) == ang

prop_angleMoveAddRow :: [((Float,Float),Int)] -> Int -> Bool
prop_angleMoveAddRow balls licz
    | rem licz 6 == 5 = length (gameBalls (Game ((6.5,24),1) balls False 0 NoA 0 False 0 licz table1 (mkStdGen 200) 10 [((1,1),1)])) < length (gameBalls (angleMove (Game ((6.5,24),1) balls False 0 NoA 0 False 0 licz table1 (mkStdGen 200) 10 [((1,1),1)])))
    | otherwise = length (gameBalls (Game ((6.5,24),1) balls False 0 NoA 0 False 0 licz table1 (mkStdGen 200) 10 [((1,1),1)])) == length (gameBalls (angleMove (Game ((6.5,24),1) balls False 0 NoA 0 False 0 licz table1 (mkStdGen 200) 10 [((1,1),1)])))

gameHelp :: Game -> Game
gameHelp (Game ((x,y),c) balls over score act ang shoot pom licz _ rand re def) = Game ((x,y),c) balls over score act ang shoot pom licz acT rand re def 
    where 
        acT = if length (activeT balls table1) < length (activeT def table2) then activeT def table1 else activeT balls table2

prop_changeBalls :: Game -> Bool 
prop_changeBalls game = 
    length (changeBalls (gameBall (gameHelp game)) (gameBalls (gameHelp game))) == 1 + length (gameBalls (gameHelp game)) 
        || length (changeBalls (gameBall (gameHelp game)) (gameBalls (gameHelp game))) <= (-2) + length (gameBalls (gameHelp game))




test :: IO ()
test = do
    putStrLn "Test distance function" >> quickCheck prop_distance
    putStrLn "Test distanceT function" >> quickCheck prop_distanceT
    putStrLn "Test emptyBalls function" >> quickCheck prop_emptyBalls
    putStrLn "Test colorBall function" >> quickCheck prop_colorBall
    putStrLn "Test angleMove function" >> quickCheck (withMaxSuccess 1000 prop_angleMove) 
    putStrLn "Test angleMove with add row" >> quickCheck prop_angleMoveAddRow
    putStrLn "Test changeBalls function" >> quickCheck (withMaxSuccess 6000 prop_changeBalls)
