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
printDisplayData (Text text) =
  case text of
    "" -> putStrLn ""
    _ -> showText text

printDisplayData (Color x) = setColor x
printDisplayData (Delay amount) = do
  threadDelay $ amount * seconds

setColor :: Int -> IO ()
setColor 0 = setSGR [SetColor Foreground Dull Black]
setColor 1 = setSGR [SetColor Foreground Dull Blue]
setColor 2 = setSGR [SetColor Foreground Dull Green]
setColor 3 = setSGR [SetColor Foreground Dull Cyan]
setColor 4 = setSGR [SetColor Foreground Dull Red]
setColor 5 = setSGR [SetColor Foreground Dull Magenta]
setColor 6 = setSGR [SetColor Foreground Dull Yellow]
setColor 7 = setSGR [SetColor Foreground Dull White]
setColor 8 = setSGR [SetColor Foreground Vivid Black]
setColor 9 = setSGR [SetColor Foreground Vivid Blue]
setColor 10 = setSGR [SetColor Foreground Vivid Green]
setColor 11 = setSGR [SetColor Foreground Vivid Cyan]
setColor 12 = setSGR [SetColor Foreground Vivid Red]
setColor 13 = setSGR [SetColor Foreground Vivid Magenta]
setColor 14 = setSGR [SetColor Foreground Vivid Yellow]
setColor 15 = setSGR [SetColor Foreground Vivid White]


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

