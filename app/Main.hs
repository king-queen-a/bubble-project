--

module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game as G
import Data.List
import Data.Ord
import System.Random
--import System.IO.Unsafe
--import System.IO

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
                    }

startGame :: Bool -> [((Float,Float),Int)] -> Game
startGame start balls = Game {   gameBall = ((6.5,24),2)
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
                    }

startingGame :: Bool -> Game -> Game
startingGame over (Game _ balls _ _ _ _ _ _ _ acT rand) = Game ((6.5,24),2) balls (not over) 0 NoA 0 False 0 0 acT rand

endGame :: Bool -> Game -> Game
endGame end (Game ball balls _ score act ang shoot _ _ acT rand) = Game ball balls end score act ang shoot 0 0 acT rand

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
changeAction LeftA (Game pos balls over score _ ang shoot pom licz at rand)
    | ang >= 70 = Game pos balls over score NoA ang shoot pom licz at rand
    | otherwise = Game pos balls over score LeftA ang shoot pom licz at rand
changeAction RightA (Game pos balls over score _ ang shoot pom licz at rand)
    | ang <= -70 = Game pos balls over score NoA ang shoot pom licz at rand
    | otherwise = Game pos balls over score RightA ang shoot pom licz at rand
changeAction NoA (Game pos balls over score _ ang shoot pom licz at rand) = Game pos balls over score NoA ang shoot pom licz at rand

changeShoot :: Bool -> Game -> Game
changeShoot shoot (Game ball balls over score act ang _ pom licz at rand)
    | shoot = Game ball balls over score NoA ang shoot pom licz at rand
    | otherwise = Game ball balls over score act ang shoot pom licz at rand
-------------------------------------------------------------------------

moveA :: Float -> Float -> Float
moveA change angle = angle + change 

angleMove :: Game -> Game
angleMove (Game ball balls over score act ang shoot pom licz at rand) = Game ball balls over score act nextAng shoot pom licz at rand
    where 
        nextAng
            | act == LeftA && ang < 60 = moveA 1 ang
            | act == RightA && ang > (-60) = moveA (-1) ang
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
shootMove (Game ((x0,y0),c) balls over score act angle shoot pom licz at rand)
    | distanceList ((x0,y0),c) balls < 0.9 = Game ((6.5,24),1) nextBalls over (score + nextScore) act angle False 0 (licz+1) nextAT rand
    | y<=1 = Game ((6.5,24),1) nextBalls over (score + nextScore) act angle False 0 (licz+1) nextAT rand
    | x0<1 = Game ((a1,b1),c) balls over score act angle shoot (pom+1) licz at rand
    | x0>12 = Game ((a2,b2),c) balls over score act angle shoot (pom+1) licz at rand
    | otherwise = Game ((x,y),c) balls over score act angle shoot pom licz at rand
    where
        nextBalls = changeBalls (ballpos,c) balls
            where ballpos = at !! minimalElemIndex (distanceElemListT ((x0,y0),c) at)
        nextAT
            | changeBalls (ballpos, c) balls == (ballpos, c) : balls = delete (at !! minimalElemIndex (distanceElemListT ((x0, y0), c) at)) at
            | length (activeT tableBalls table1) < length (activeT tableBalls table2) = activeT tableBalls table1
            | otherwise = activeT tableBalls table2
            where 
                ballpos = at !! minimalElemIndex (distanceElemListT ((x0,y0),c) at)
                tableBalls = filterBalls (checkDelateBalls (ballpos,c) (((ballpos,c),True): zip balls (repeat False)))
        nextScore
            | l <= 2 = 0
            | otherwise = (-1) + l
            where
                ballpos = at !! minimalElemIndex (distanceElemListT ((x0,y0),c) at)
                l = length (filterToScore (checkDelateBalls (ballpos,c) (((ballpos,c),True): zip balls (repeat False))))
        (a1,b1) = (1,y0-0.2*cos alpha)
        (a2,b2) = (12,y0-0.2*cos alpha)
        (x,y) = (x0-0.2*((-1)**pom)*sin alpha,y0-0.2*cos alpha)
        alpha = angle * pi / 180

move :: Game -> Game
move (Game ((x0,y0),c) balls over score act angle shoot pom licz at rand)
    | shoot = shootMove (Game ((x0,y0),c) balls over score act angle shoot pom licz at rand)
    | otherwise = angleMove (Game ((x0,y0),c) balls over score act angle shoot pom licz at rand)

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
filterBalls balls = map fst (filter (\x -> snd x == False) balls)

filterBallsBool :: [(((Float,Float),Int),Bool)] -> [(((Float,Float),Int),Bool)]
filterBallsBool = filter (\x -> snd x == False)

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
changeForms balls ballsFalse = zipWith changeForm balls ballsFalse

delateFreeBalls :: [((Float,Float),Int)] -> [((Float,Float),Bool)] -> [(((Float,Float),Int),Bool)]
delateFreeBalls balls ballsFalse = changeForms balls (etykateFreeBalls balls ballsFalse)

przyk1 :: [(((Float,Float),Int),Bool)]
przyk1 = [(((1,1),1),True),(((2,1),1),False),(((3,1),2),False),(((4,1),3),False),(((1.5,2),1),False),(((2.5,2),2),False),(((3.5,2),2),False)]
przyk2 :: [(((Float,Float),Int),Bool)]
przyk2 = [(((1,1),1),False),(((2,1),1),False),(((3,1),2),False),(((4,1),3),False),(((1.5,2),1),False),(((2.5,2),2),False),(((3.5,2),2),False)]
---------------------------------------------------------------------------------------------


gameBackground :: Color
gameBackground = white
gameWindow :: Display      --gameWindow = InWindow "Bubble" (260,500) (100, 100)
gameWindow = InWindow "Bubble" (310,610) (100, 100)

keys :: Event -> Game -> IO Game
keys (EventKey (SpecialKey KeyLeft) G.Down _ _) game = return (changeAction LeftA game)
keys (EventKey (SpecialKey KeyRight) G.Down _ _) game = return (changeAction RightA game)
keys (EventKey (SpecialKey KeyDown) G.Down _ _) game = return (changeAction NoA game)
keys (EventKey (SpecialKey KeyUp) G.Down _ _) game = if not $ gameShoot game && not (gameOver game) 
                        then return (changeShoot True game) else return (changeShoot False game)
keys (EventKey (SpecialKey KeyEnter) G.Down _ _) game = if gameOver game then return (startingGame True game) else return game
keys (EventKey (SpecialKey KeyEsc) G.Down _ _) game = if gameOver game then return game else return (endGame True game)
keys _ game = return game

graphicsGame :: Game -> IO Picture
graphicsGame game = return ( pictures $ scoreGraphics ++ overGraphics
                            ++ fmap bubbleGraphics balls
                            ++ [bubbleGraphics (gameBall game)]
                            ++ scoreTextGraphics
                            ++ [gameLine (gameAngle game)]
                            )
    where
        gameLine alpha = color black $ scale 1 (-1) $ line [(6.5*20-130,24*20-250),((6.5-2*sin x)*20-130,(24-2*cos x)*20-250)]
            where
                x = alpha * pi / 180
        fillRectangle color' (tx, ty) (w, h) =  color color' $ 
                                                    scale 1 (-1) $ 
                                                    translate (tx * 20 - 130) (ty * 20 - 250) $ 
                                                    rectangleSolid w h
        balls = gameBalls game
        bubbleGraphics ::((Float,Float),Int) -> Picture
        bubbleGraphics ((x,y),col)
            | col == 1 = color red $ scale 1 (-1) $ translate (x*20 - 130) (y*20 - 250) $ circleSolid 9
            | col == 2 = color blue $ scale 1 (-1) $ translate (x*20 - 130) (y*20 - 250) $ circleSolid 9
            | col == 3 = color azure $ scale 1 (-1) $ translate (x*20 - 130) (y*20 - 250) $ circleSolid 9
            | col == 4 = color green $ scale 1 (-1) $ translate (x*20 - 130) (y*20 - 250) $ circleSolid 9
            | col == 5 = color rose $ scale 1 (-1) $ translate (x*20 - 130) (y*20 - 250) $ circleSolid 9
            | col == 6 = color orange $ scale 1 (-1) $ translate (x*20 - 130) (y*20 - 250) $ circleSolid 9
            | otherwise = color cyan  $ scale 1 (-1) $ translate (x*20 - 130) (y*20 - 250) $ circleSolid 9
        scoreGraphics = [color red $ translate 110 (-290) $ scale 0.25 0.25 $ text (show (gameScore game))]
        scoreTextGraphics = [color blue $ translate 70 (-285) $ scale 0.1 0.1 $ text "Score: "]
        overGraphics = if gameOver game then [color red $ translate (-200) 0 $ scale 0.25 0.25 $ text "Koniec gry :("
                                     ,  color green $ translate (-175) (-50) $ scale 0.1 0.1 $ text "Press Enter to start" ] 
                                    else [ fillRectangle black (6.5, 0) (260, 20)
                                        , fillRectangle black (6.5, 25) (260, 20)
                                        , fillRectangle black (0, 12.5) (20, 500)
                                        , fillRectangle black (13, 12.5) (20, 500) ]

step :: Float -> Game -> IO Game 
step _ game = if gameOver game then return game else -- print (length balls, length activ, (length balls) + (length activ)) >> 
    return (Game ((x,y),c) balls over score action angle shoot pom licz activ rand)
    where
        --((x,y),c) = gameBall game
        --((x,y),c) = if shoot then gameBall (shootMove game) else gameBall (angleMove game)
        --balls = gameBalls game
        over = gameOver game
        --score = gameScore game
        action = gameAction game
        --shoot = gameShoot game
        --Game ((x,y),c) _ _ _ _ angle shoot pom = if shoot then fst $ shootMove (game,0) else angleMove game
        --Game ((x,y),c) _ _ _ _ _ shoot = if shoot then shootMove game else game
        Game ((x,y),c) balls _ score _ angle shoot pom licz activ rand = move game
        --pom = pomoc $ shootMove game
        --rand = gameRandom game
        


f1 :: String -> Int
f1 = read
f2 :: String -> Float
f2 = read

main :: IO ()
main = do
    bx <- readFile "bubblex.txt"
    by <- readFile "bubbley.txt"
    bc <- readFile "bubblec.txt"
    let a = zip (zip (map f2 . words $ bx) (map f2 . words $ by)) (map f1 . words $ bc)
    playIO gameWindow gameBackground 25 (startGame False a) graphicsGame keys step
