module Types where

type Value = Int
type PlayerName = [Char]
data GameState = GameState PlayerName [Value]

type Slot = Int

data MutationOperator = Assign | Add | Subtract | Random Float

data Condition = Condition (GameState -> Bool)
instance Semigroup Condition where
  (<>) (Condition f1) (Condition f2) = Condition (\x -> f1 x && f2 x)
instance Monoid Condition where
  mempty = Condition (\x -> True)

data Mutation = Mutation Slot MutationOperator Value

type ActionKey = Char
data ActionColor = ActionColor Int
data Action = Action Condition (Maybe ActionKey) (Maybe ActionColor) String [Mutation]

data DisplayData = Text String | Color Int | Delay Int
data Description = Description Condition [DisplayData] [Mutation]

type MetaData = (String, String)
data Content = Content [MetaData] [Description] [Action]

