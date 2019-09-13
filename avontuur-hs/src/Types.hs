module Types where

data Content = Content [Description] [Action]
data Description = Description [Condition] [DisplayData] [Mutation]
data Action = Action [Condition] String [Mutation]

data Condition = Condition Slot Operator Value
type Slot = Int
type Value = Int
data Operator = Equals | NotEquals | GreaterThan | LessThan
data DisplayData = Text String | Color Int
data Mutation = Mutation Slot MutationOperator Value
data MutationOperator = Assign | Add | Subtract | Random
data GameState = GameState [Value]

