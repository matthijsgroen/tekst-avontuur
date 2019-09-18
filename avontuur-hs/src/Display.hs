module Display (printDescription, printAction) where
import System.Console.ANSI.Types
import System.Console.ANSI
import Types

printDisplayData :: DisplayData -> IO ()
printDisplayData (Text text) =
  case text of
    "" -> putStrLn "\n"
    _ -> putStr (text ++ " ")

printDisplayData (Color 0) = setSGR [SetColor Foreground Dull Black]
printDisplayData (Color 1) = setSGR [SetColor Foreground Dull Blue]
printDisplayData (Color 2) = setSGR [SetColor Foreground Dull Green]
printDisplayData (Color 3) = setSGR [SetColor Foreground Dull Cyan]
printDisplayData (Color 4) = setSGR [SetColor Foreground Dull Red]
printDisplayData (Color 5) = setSGR [SetColor Foreground Dull Magenta]
printDisplayData (Color 6) = setSGR [SetColor Foreground Dull Yellow]
printDisplayData (Color 7) = setSGR [SetColor Foreground Dull White]
printDisplayData (Color 8) = setSGR [SetColor Foreground Vivid Black]
printDisplayData (Color 9) = setSGR [SetColor Foreground Vivid Blue]
printDisplayData (Color 10) = setSGR [SetColor Foreground Vivid Green]
printDisplayData (Color 11) = setSGR [SetColor Foreground Vivid Cyan]
printDisplayData (Color 12) = setSGR [SetColor Foreground Vivid Red]
printDisplayData (Color 13) = setSGR [SetColor Foreground Vivid Magenta]
printDisplayData (Color 14) = setSGR [SetColor Foreground Vivid Yellow]
printDisplayData (Color 15) = setSGR [SetColor Foreground Vivid White]

printDisplayData (Delay code) =
  putStr ""

printDescription :: Description -> IO ()
printDescription (Description _ displayData _) = do
  mapM_ printDisplayData displayData

printAction :: Action -> Int -> IO ()
printAction (Action _ _ _ text _) num = do
  putStrLn (show num ++ ") " ++ text)

