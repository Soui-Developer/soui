module Model.World(
    World(..),
    ValueAssignment(..),
    Value(..),
    emptyWorld,
    getVariableValue,
    setVariableValue
) where

data World = World {
    valueAssignments :: [ValueAssignment] -- TODO Change to a map to prevent ordering issues
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

setVariableValue :: String -> Value -> World -> World
setVariableValue variableName' value' world =
    let oldAssignments = valueAssignments world
        newAssignments = map (updateAssignment variableName' value') oldAssignments
    in
        world{valueAssignments = newAssignments}

updateAssignment :: String -> Value -> ValueAssignment -> ValueAssignment
updateAssignment variableName' value' assignment =
    if variableName assignment == variableName'
    then assignment{assignedValue = value'}
    else assignment
