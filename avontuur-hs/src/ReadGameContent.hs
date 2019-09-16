module ReadGameContent where
import Types

readGame :: FilePath -> IO Content
readGame filePath = do
  contents <- readFile filePath

  return $ Content [
    Description [ Condition 0 Equals 0 ] [Text "Hello world"] [],
    Description [ Condition 0 Equals 1 ] [Text "Goodbye", Color 14, Text " Woohoo!"] []
    ] [
    Action [ Condition 0 Equals 0 ] "Complete game" [ Mutation 0 Assign 1 ]
    ]
