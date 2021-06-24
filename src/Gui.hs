module Gui where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game as G
import Lib

gameBackground :: Color
gameBackground = white
gameWindow :: Display      --gameWindow = InWindow "Bubble" (260,500) (100, 100)
gameWindow = InWindow "Bubble" (325,620) (100, 100)

keys :: Event -> Game -> IO Game
keys (EventKey (SpecialKey KeyLeft) G.Down _ _) game = if not $ gameShoot game then return (changeAction LeftA game) else return game
keys (EventKey (SpecialKey KeyRight) G.Down _ _) game = if not $ gameShoot game then return (changeAction RightA game) else return game
keys (EventKey (SpecialKey KeyDown) G.Down _ _) game = if not $ gameShoot game then return (changeAction NoA game) else return game
keys (EventKey (SpecialKey KeyUp) G.Down _ _) game = if not $ gameShoot game && not (gameOver game) 
                        then return (changeShoot True game) else return game
keys (EventKey (SpecialKey KeySpace) G.Down _ _) game = 
    if gameOver game && (notElem 22 (map (snd . fst) (gameBalls game)) && not (null (gameBalls game))) then return (startingGame True game) else return game
keys (EventKey (SpecialKey KeyEnter) G.Down _ _) game = 
    if gameOver game then return (startingGameDef True game) else return game
keys (EventKey (SpecialKey KeyEsc) G.Down _ _) game = if gameOver game then return game else return (endGame True game)
keys _ game = return game

graphicsGame :: Game -> IO Picture
graphicsGame game = return ( pictures $ scoreGraphics
                            ++ recordGraphics
                            ++ fmap bubbleGraphics balls
                            ++ [bubbleGraphics (gameBall game)]
                            ++ scoreTextGraphics
                            ++ recordTextGraphics
                            ++ [gameLine (gameAngle game)]
                            ++ [gameOverLine]
                            ++ overGraphics
                            )
    where
        gameLine alpha = color black $ scale 1 (-1) $ line [(6.5*20-130,24*20-250),((6.5-2*sin x)*20-130,(24-2*cos x)*20-250)]
            where
                x = alpha * pi / 180
        gameOverLine = color red $ scale 1 (-1) $ line [(0.5*20-130,21.5*20-250),(12.5*20-130,21.5*20-250)]
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
        scoreGraphics = [color red $ translate 100 (-290) $ scale 0.25 0.25 $ text (show (gameScore game))]
        recordGraphics = [color red $ translate 100 275 $ scale 0.25 0.25 $ text (show (gameRecord game))]
        scoreTextGraphics = [color blue $ translate 40 (-285) $ scale 0.1 0.1 $ text "Score: "]
        recordTextGraphics = [color blue $ translate 35 280 $ scale 0.1 0.1 $ text "Record: "]
        overGraphics = if gameOver game then [color red $ translate (-120) 20 $ scale 0.25 0.25 $ text "Koniec gry :("
                                     ,  color black $ translate (-110) (-25) $ scale 0.1 0.1 $ text "Press Enter to start default game" 
                                     ,  color black $ translate (-130) (-50) $ scale 0.1 0.1 $ text "Press Space to start (only if correct file)"] 
                                    else [ fillRectangle black (6.5, 0) (260, 20)
                                        , fillRectangle black (6.5, 25) (260, 20)
                                        , fillRectangle black (0, 12.5) (20, 500)
                                        , fillRectangle black (13, 12.5) (20, 500) ]

convIntToString :: Int -> String
convIntToString = show
convFloatToString :: Float -> String
convFloatToString = show

convLIntToLString :: [Int] -> [String]
convLIntToLString = map convIntToString
convLFloatToLString :: [Float] -> [String]
convLFloatToLString = map convFloatToString

cutString :: Int -> Int -> String -> String
cutString _ _ "" = ""
cutString n m l = take n l ++ cutString n m (drop (n+m) l)


step :: Float -> Game -> IO Game
step _ game = do
    writeFile "record.txt" (show $ gameRecord game)
    writeFile "bx.txt" $ cutString (lx-1) 2 (unlines $ convLFloatToLString (map (fst . fst) (gameBalls game))) 
    writeFile "by.txt" $ cutString (ly-1) 2 (unlines $ convLFloatToLString (map (snd . fst) (gameBalls game)))
    writeFile "bc.txt" $ cutString (lc-1) 2 (unlines $ convLIntToLString (map snd (gameBalls game))) 
    if gameOver game then return game else
        return (Game ((x,y),c) balls over score action angle shoot pom licz activ rand re def)
    where
        lx = length (unlines $ convLFloatToLString (map (fst . fst) (gameBalls game)))
        ly = length (unlines $ convLFloatToLString (map (snd . fst) (gameBalls game)))
        lc = length (unlines $ convLIntToLString (map snd (gameBalls game)))
        over = ourGameOver balls
        action = gameAction game
        Game ((x,y),c) balls _ score _ angle shoot pom licz activ rand re _ = move game
        def = defaultBalls game
        


f1 :: String -> Int
f1 = read
f2 :: String -> Float
f2 = read

outFunc :: IO ()
outFunc = do
    x <- readFile "bubblex.txt"
    y <- readFile "bubbley.txt"
    c <- readFile "bubblec.txt"
    bx <- readFile "bx.txt"
    by <- readFile "by.txt"
    bc <- readFile "bc.txt"
    record <- fmap f1 (readFile "record.txt")
    let ab = zip (zip (map f2 . words $ bx) (map f2 . words $ by)) (map f1 . words $ bc)
    let a = zip (zip (map f2 . words $ x) (map f2 . words $ y)) (map f1 . words $ c)
    playIO gameWindow gameBackground 35 (startGame False ab record a) graphicsGame keys step
