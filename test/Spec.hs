import qualified Test.Lib as Lib
import qualified Test.Gui as Gui

main :: IO ()
main = Lib.test >> Gui.test

--main = putStrLn "Test suite not yet implemented"
