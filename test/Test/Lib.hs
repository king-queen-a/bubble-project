module Test.Lib where

import Test.QuickCheck
import Test.QuickCheck.All
import Test.QuickCheck.Modifiers
import Lib
import System.Random


-- test distance
prop_distance :: ((Float, Float),Int) -> ((Float, Float),Int) -> Bool
prop_distance ((x1,y1),c1) ((x2,y2),c2) = 
    distance ((x1,y1),c1) ((x2,y2),c2) == distance ((y1,x1),c1) ((y2,x2),c2) && distance ((x1,y1),c1) ((x2,y2),c2) == distance ((x2,y2),c2) ((x1,y1),c1)

prop_distanceT :: ((Float, Float),Int) -> (Float, Float) -> Bool
prop_distanceT ((x1,y1),c) (x2,y2) = 
    distanceT ((x1,y1),c) (x2,y2) == distanceT ((y1,x1),c) (y2,x2) && distanceT ((x1,y1),c) (x2,y2) == distanceT ((x2,y2),c) (x1,y1)

prop_emptyBalls :: [((Float, Float), Int)] -> Int -> Bool
prop_emptyBalls balls rand = not (null (emptyBalls balls (mkStdGen rand)))

prop_colorBall :: Int -> Bool
prop_colorBall rand = fst (colorBall (mkStdGen rand)) <= 7 && fst (colorBall (mkStdGen rand)) >= 1



test :: IO ()
test = do
    putStrLn "Test distance function" >> quickCheck prop_distance
    putStrLn "Test distanceT function" >> quickCheck prop_distanceT
    putStrLn "Test emptyBalls function" >> quickCheck prop_emptyBalls
    putStrLn "Test colorBall function" >> quickCheck prop_colorBall

