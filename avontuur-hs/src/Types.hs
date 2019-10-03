module Types where

type Value = Int
type PlayerName = [Char]
data GameState = GameState PlayerName [Value]

type Slot = Int

data MutationOperator = Assign | Add | Subtract | Random Float

newtype Condition = Condition (GameState -> Bool)
instance Semigroup Condition where
  (<>) (Condition c1) (Condition c2) = Condition (\x -> c1 x && c2 x)
instance Monoid Condition where
  mempty = Condition (\x -> True)

newtype Mutation = Mutation (GameState -> GameState)
instance Semigroup Mutation where
  (<>) (Mutation m1) (Mutation m2) = Mutation (m1 . m2)
instance Monoid Mutation where
  mempty = Mutation id

type ActionKey = Char
data ActionColor = ActionColor Int
data Action = Action Condition (Maybe ActionKey) (Maybe ActionColor) String Mutation

data DisplayData = Text String | Color Int | Delay Int
data Description = Description Condition [DisplayData] Mutation

type MetaData = (String, String)
data Content = Content [MetaData] [Description] [Action]

