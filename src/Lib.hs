module Lib where

import Data.List
import Data.Ord
import System.Random

---------------------------------------------------------------
columns :: Float
rows :: Float
columns = 13
rows = 25

data Action = NoA | RightA | LeftA deriving (Eq, Show)
data Game = Game {    gameBall :: ((Float,Float),Int)
                    , gameBalls :: [((Float,Float),Int)]
                    , gameOver :: Bool
                    , gameScore :: Int
                    , gameAction :: Action
                    , gameAngle :: Float
                    , gameShoot :: Bool
                    , pomoc :: Float
                    , gameLicznik :: Int
                    , gameTable :: [(Float,Float)]
                    , gameRandom :: StdGen
                    , gameRecord :: Int
                    , defaultBalls :: [((Float,Float),Int)]
                    }

startGame :: Bool -> [((Float,Float),Int)] -> Int -> [((Float,Float),Int)] -> Game
startGame start balls re def = Game {   gameBall = ((6.5,24),2)
                    , gameBalls = balls
                    , gameOver = not start
                    , gameScore = 0
                    , gameAction = NoA
                    , gameAngle = 0
                    , gameShoot = False
                    , pomoc = 0
                    , gameLicznik = 0
                    , gameTable = if length (activeT balls table1) < length (activeT balls table2) then activeT balls table1
                                else activeT balls table2
                    , gameRandom = mkStdGen 100
                    , gameRecord = re
                    , defaultBalls = def
                    }

startingGame :: Bool -> Game -> Game
startingGame over (Game _ balls _ _ _ _ _ _ _ acT rand re def) = Game ((6.5,24),col) balls (not over) 0 NoA 0 False 0 0 acT nextRand re def
    where 
        (col,nextRand) = colorBall rand

startingGameDef :: Bool -> Game -> Game
startingGameDef over (Game _ _ _ _ _ _ _ _ _ _ rand re def) = Game ((6.5,24),col) def (not over) 0 NoA 0 False 0 0 acT nextRand re def
    where 
        acT = if length (activeT def table1) < length (activeT def table2) then activeT def table1 else activeT def table2
        (col,nextRand) = colorBall rand

endGame :: Bool -> Game -> Game
endGame end (Game ball balls _ score act ang shoot _ _ acT rand re def) = Game ball balls end score act ang shoot 0 0 acT rand re def

table1 :: [(Float,Float)]
table1 = [(x,y) | x <- [1,2,3,4,5,6,7,8,9,10,11,12], y <- [1,3,5,7,9,11,13,15,17,19,21]] 
    ++ [(x,y) | x <- [1.5,2.5,3.5,4.5,5.5,6.5,7.5,8.5,9.5,10.5,11.5], y <- [2,4,6,8,10,12,14,16,18,20,22]]
table2 :: [(Float,Float)]
table2 = [(x,y) | x <- [1,2,3,4,5,6,7,8,9,10,11,12], y <- [2,4,6,8,10,12,14,16,18,20,22]] 
    ++ [(x,y) | x <- [1.5,2.5,3.5,4.5,5.5,6.5,7.5,8.5,9.5,10.5,11.5], y <- [1,3,5,7,9,11,13,15,17,19,21]]

nonActiveT :: [((Float,Float),Int)] -> [(Float,Float)]
nonActiveT = map fst

activeT :: [((Float,Float),Int)] -> [(Float,Float)] -> [(Float,Float)]
activeT balls = filter ( `notElem` nonActiveT balls)

-------------------------------- keyboard -----------------------------
changeAction :: Action -> Game -> Game
changeAction LeftA (Game pos balls over score _ ang shoot pom licz at rand re def)
    | ang >= 70 = Game pos balls over score NoA ang shoot pom licz at rand re def
    | otherwise = Game pos balls over score LeftA ang shoot pom licz at rand re def
changeAction RightA (Game pos balls over score _ ang shoot pom licz at rand re def)
    | ang <= -70 = Game pos balls over score NoA ang shoot pom licz at rand re def
    | otherwise = Game pos balls over score RightA ang shoot pom licz at rand re def
changeAction NoA (Game pos balls over score _ ang shoot pom licz at rand re def) = Game pos balls over score NoA ang shoot pom licz at rand re def

changeShoot :: Bool -> Game -> Game
changeShoot shoot (Game ball balls over score act ang _ pom licz at rand re def)
    | shoot = Game ball balls over score NoA ang shoot pom licz at rand re def
    | otherwise = Game ball balls over score act ang shoot pom licz at rand re def
-------------------------------------------------------------------------

moveA :: Float -> Float -> Float
moveA change angle = angle + change 

angleMove :: Game -> Game
angleMove (Game ball balls over score act ang shoot pom licz at rand re def) = Game ball nextBalls over score act nextAng shoot pom nextLicz nextAT rand re def
    where
        nextLicz = if rem licz 6 == 5 && not shoot then licz+1 else licz
        nextBalls = if rem licz 6 == 5 && not shoot then addBallsRow balls rand else balls
        nextAT
            | rem licz 6 /= 5 = at
            | length (activeT nextBalls table1) < length (activeT nextBalls table2) = activeT nextBalls table1
            | otherwise = activeT nextBalls table2
        nextAng
            | act == LeftA && ang < 70 = moveA 0.5 ang
            | act == RightA && ang > (-70) = moveA (-0.5) ang
            | otherwise = ang

distance :: ((Float, Float),Int) -> ((Float, Float),Int) -> Float
distance ((x0,y0),_) ((x,y),_) = sqrt $ (x0-x)**2+(y0-y)**2

distanceT :: ((Float, Float),Int) -> (Float, Float) -> Float
distanceT ((x0,y0),_) (x,y) = sqrt $ (x0-x)**2+(y0-y)**2

distanceElemList :: ((Float,Float),Int) -> [((Float,Float),Int)] -> [Float]
distanceElemList ((x0,y0),c) = map (distance ((x0,y0),c))

distanceElemListT :: ((Float,Float),Int) -> [(Float,Float)] -> [Float]
distanceElemListT ((x0,y0),c) = map (distanceT ((x0,y0),c))

distanceList :: ((Float, Float), Int) -> [((Float, Float), Int)] -> Float
distanceList point list = minimum $ distanceElemList point list

minimalElemIndex :: [Float] -> Int
minimalElemIndex tab = fst . minimumBy (comparing snd) $ zip [0..] tab

shootMove :: Game -> Game
shootMove (Game ((x0,y0),c) balls over score act angle shoot pom licz at rand re def)
    | distanceList ((x0,y0),c) balls < 0.9 = Game ((6.5,24),col) nextBalls over (score + nextScore) act angle False 0 nextLicz nextAT nextRand nextRe def
    | y<=1 = Game ((6.5,24),col) nextBalls over (score + nextScore) act angle False 0 nextLicz nextAT nextRand nextRe def
    | x0<1 = Game ((a1,b1),c) balls over score act angle shoot (pom+1) licz at rand re def
    | x0>12 = Game ((a2,b2),c) balls over score act angle shoot (pom+1) licz at rand re def
    | otherwise = Game ((x,y),c) balls over score act angle shoot pom licz at rand re def
    where
        nextLicz
            | null (changeBalls (ballpos,c) balls) = 0
            | otherwise = licz + 1
            where
                ballpos = at !! minimalElemIndex (distanceElemListT ((x0,y0),c) at)
        nextRe = if score + nextScore > re then score + nextScore else re
        --nextOver = ourGameOver nextBalls
        (col,nextRand) = colorBall rand
        nextBalls = emptyBalls (changeBalls (ballpos,c) balls) rand
            where ballpos = at !! minimalElemIndex (distanceElemListT ((x0,y0),c) at)
        nextAT
            | null (changeBalls (ballpos,c) balls) = if length (activeT nextBalls table1) < length (activeT nextBalls table2)
                                 then activeT nextBalls table1 else activeT nextBalls table2
            | changeBalls (ballpos, c) balls == (ballpos, c) : balls = delete (at !! minimalElemIndex (distanceElemListT ((x0, y0), c) at)) at
            | length (activeT tableBalls table1) < length (activeT tableBalls table2) = activeT tableBalls table1
            | otherwise = activeT tableBalls table2
            where 
                ballpos = at !! minimalElemIndex (distanceElemListT ((x0,y0),c) at)
                tableBalls = filterBalls (checkDelateBalls (ballpos,c) (((ballpos,c),True): zip balls (repeat False)))
        nextScore
            | l <= 2 = 0
            | otherwise = (-1) + l + length (filterToScore (delateFreeBalls b (noColorBalls b)))
            where
                ballpos = at !! minimalElemIndex (distanceElemListT ((x0,y0),c) at)
                l = length (filterToScore (checkDelateBalls (ballpos,c) (((ballpos,c),True): zip balls (repeat False))))
                b = filterBalls (checkDelateBalls (ballpos,c) (((ballpos,c),True) : zip balls (repeat False)))
        (a1,b1) = (1,y0-0.2*cos alpha)
        (a2,b2) = (12,y0-0.2*cos alpha)
        (x,y) = (x0-0.2*((-1)**pom)*sin alpha,y0-0.2*cos alpha)
        alpha = angle * pi / 180

move :: Game -> Game
move (Game ((x0,y0),c) balls over score act angle shoot pom licz at rand re def)
    | shoot = shootMove (Game ((x0,y0),c) balls over score act angle shoot pom licz at rand re def)
    | otherwise = angleMove (Game ((x0,y0),c) balls over score act angle shoot pom licz at rand re def)

neighbourTrue :: Int -> [(((Float,Float),Int),Bool)] -> ((Float,Float),Int) -> (((Float,Float),Int),Bool)
neighbourTrue c balls ((x,y),col)
    | c /= col = (((x,y),col),False)
    | (((x,y),col),True) `elem` balls = (((x,y),col),True)
    | elem (((x-1,y),c),True) balls || elem (((x+1,y),c),True) balls || elem (((x-0.5,y+1),c),True) balls || elem (((x+0.5,y+1),c),True) balls || elem (((x-0.5,y-1),c),True) balls || elem (((x+0.5,y-1),c),True) balls = (((x,y),c),True)
    | otherwise = (((x,y),c),False)

checkDelateBalls :: ((Float,Float),Int) -> [(((Float,Float),Int),Bool)] -> [(((Float,Float),Int),Bool)]
checkDelateBalls ((x,y),c) ballsTrue
    | ballsTrue == map (neighbourTrue c ballsTrue) (map fst ballsTrue) = ballsTrue
    | otherwise = checkDelateBalls ((x,y),c) (map (neighbourTrue c ballsTrue) (map fst ballsTrue))

filterBalls :: [(((Float,Float),Int),Bool)] -> [((Float,Float),Int)]
filterBalls balls = map fst (filter (not . snd) balls)

filterToScore :: [(((Float,Float),Int),Bool)] -> [((Float,Float),Int)]
filterToScore balls = map fst (filter snd balls)

changeBalls :: ((Float,Float),Int) -> [((Float,Float),Int)] -> [((Float,Float),Int)]
changeBalls ((x,y),c) balls = if l > 2 then filterBalls (delateFreeBalls b (noColorBalls b)) else ((x,y),c) : balls
    where
        b = filterBalls (checkDelateBalls ((x,y),c) ((((x,y),c),True) : zip balls (repeat False)))
        l = length (filterToScore (checkDelateBalls ((x,y),c) ((((x,y),c),True) : zip balls (repeat False))))

noColorBall :: ((Float,Float),Int) -> ((Float,Float),Bool)
noColorBall ((x,y),_)
    | y == 1 = ((x,y),False)
    | otherwise = ((x,y),True)

noColorBalls :: [((Float,Float),Int)] -> [((Float,Float),Bool)]
noColorBalls = map noColorBall

neighbourFalse :: [((Float,Float),Bool)] -> ((Float,Float),Int) -> ((Float,Float),Bool)
neighbourFalse balls ((x,y),_)
    | ((x,y),False) `elem` balls = ((x,y),False)
    | elem ((x-1,y),False) balls || elem ((x+1,y),False) balls || elem ((x-0.5,y+1),False) balls || elem ((x+0.5,y+1),False) balls || elem ((x-0.5,y-1),False) balls || elem ((x+0.5,y-1),False) balls = ((x,y),False)
    | otherwise = ((x,y),True)

etykateFreeBalls :: [((Float,Float),Int)] -> [((Float,Float),Bool)] -> [((Float,Float),Bool)]
etykateFreeBalls balls ballsTrue
    | map (neighbourFalse ballsTrue) balls == ballsTrue = ballsTrue
    | otherwise = etykateFreeBalls balls (map (neighbourFalse ballsTrue) balls)

changeForm :: ((Float,Float),Int) -> ((Float,Float),Bool) -> (((Float,Float),Int),Bool)
changeForm ball (_,b) = (ball,b)

changeForms :: [((Float,Float),Int)] -> [((Float,Float),Bool)] -> [(((Float,Float),Int),Bool)]
changeForms = zipWith changeForm

delateFreeBalls :: [((Float,Float),Int)] -> [((Float,Float),Bool)] -> [(((Float,Float),Int),Bool)]
delateFreeBalls balls ballsFalse = changeForms balls (etykateFreeBalls balls ballsFalse)

colorBall :: StdGen -> (Int,StdGen)
colorBall = randomR (1,7)

ourGameOver :: [((Float,Float),Int)] -> Bool
ourGameOver balls
    | 22 `elem` map (snd . fst) balls = True
    | otherwise = False

colorBalls :: StdGen -> [Int]
colorBalls = take 12 . unfoldr (Just . randomR (1, 7))

translateBall :: ((Float,Float),Int) -> ((Float,Float),Int)
translateBall ((x,y),c) = ((x,y+1),c)

translateBalls :: [((Float,Float),Int)] -> [((Float,Float),Int)]
translateBalls = map translateBall

addBallsRow :: [((Float,Float),Int)] -> StdGen -> [((Float,Float),Int)]
addBallsRow balls rand
    | length (activeT (translateBalls balls) table1) < length (activeT (translateBalls balls) table2) = zip (filter (\x -> snd x == 1) table1) (colorBalls rand) ++ translateBalls balls
    | otherwise = zip (filter (\x -> snd x == 1) table2) (colorBalls rand) ++ translateBalls balls

emptyBalls :: [((Float, Float), Int)] -> StdGen -> [((Float, Float), Int)]
emptyBalls balls rand
    | null balls = addBallsRow balls rand
    | otherwise = balls
