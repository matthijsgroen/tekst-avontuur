module Display (printDescription, printAction) where
import Types

printDisplayData :: DisplayData -> IO ()
printDisplayData (Text text) =
  putStr text

printDisplayData (Color code) =
  putStr ""

printDescription :: Description -> IO ()
printDescription (Description _ displayData _) = do
  mapM_ printDisplayData displayData
  putStrLn ""

printAction :: Action -> Int -> IO ()
printAction (Action _ text _) num = do
  putStrLn (show num ++ ") " ++ text)

