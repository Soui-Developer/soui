module Model.World(
    World(..),
    ValueAssignment(..),
    Value(..),
    emptyWorld,
    getVariableValue
) where

data World = World {
    valueAssignments :: [ValueAssignment]
} deriving (Show, Eq)

data ValueAssignment = ValueAssignment {
    variableName :: String,
    assignedValue :: Value
} deriving (Show, Eq)

data Value = BooleanValue Bool
           deriving (Show, Eq)

emptyWorld :: World
emptyWorld = World {
    valueAssignments = []
}

getVariableValue :: World -> String -> Value
getVariableValue world variableName' =
    let assignments = valueAssignments world
        matchingAssignments = filter (\a -> variableName a == variableName') assignments
    in
        if null matchingAssignments
        then error $ "No such variable: " ++ variableName'
        else assignedValue $ head matchingAssignments
