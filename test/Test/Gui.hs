module Test.Gui where

import Test.QuickCheck
import Gui

prop_cutString :: String -> Bool
prop_cutString str = if length str > 1 then 1 + length (cutString (length str - 1) 1 str) == length str else null (cutString (length str - 1) 1 str)

test :: IO ()
test = putStrLn "Test cutString function" >> quickCheck prop_cutString 