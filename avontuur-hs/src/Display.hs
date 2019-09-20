module Display (printDescription, printAction) where
import System.Console.ANSI.Types
import System.Console.ANSI
import Control.Concurrent (threadDelay)
import Types

seconds = 10 ^ 6
typeDelay = 2 * 10 ^ 4

showText :: [Char] -> IO ()
showText (x:xs) = do
  putStr [x]
  threadDelay typeDelay
  showText xs
showText [] = putStrLn ""

printDisplayData :: DisplayData -> IO ()
printDisplayData (Text text) | text == "" = putStrLn ""
printDisplayData (Text text) = showText text

printDisplayData (Color x) = setColor x
printDisplayData (Delay amount) = do
  threadDelay $ amount * seconds

getIntensity :: Int -> ColorIntensity
getIntensity x | x > 7 = Vivid
getIntensity _ = Dull

colors = [Black, Blue, Green, Cyan, Red, Magenta, Yellow, White]

getTint :: Int -> Color
getTint x | x > 7 = getTint (x - 8)
getTint x = colors !! x

setColor :: Int -> IO ()
setColor x =
  let intensity = getIntensity x
      tint = getTint x
  in setSGR [SetColor Foreground intensity tint]

printDescription :: Description -> IO ()
printDescription (Description _ displayData _) = do
  mapM_ printDisplayData displayData

setActionColor :: Maybe ActionColor -> IO ()
setActionColor (Just (ActionColor color)) = setColor color
setActionColor _ = setSGR [Reset]

printAction :: Action -> Int -> IO ()
printAction (Action _ _ actionColor text _) num = do
  setActionColor actionColor
  putStrLn (show num ++ ") " ++ text)

