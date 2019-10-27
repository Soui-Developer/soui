module Model.Variable(
    Variable(..),
    VariableType(..)
) where

data Variable = Variable {
    name :: String,
    variableType :: VariableType
}

data VariableType = BooleanType
