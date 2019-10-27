module Kernel(
    SystemState(bestWorldYetIdentified),
    initialState,
    provideModel,
    provideWorld,
    think,
    allCombinations
) where

import Model.Model(WorldModel(variables), emptyModel)
import Model.World(World(..), emptyWorld)
import Computation.Enumeration(allPossibleAssignments)
import Model.Variable(Variable(name))
import Utils(allCombinations)

data SystemState = SystemState {
    messageCounter :: Int,
    model :: WorldModel,
    currentWorld :: World,
    -- | The most optimal world the system has been able to find so far
    bestWorldYetIdentified :: World
}

initialState :: SystemState
initialState = SystemState {
    messageCounter = 0,
    model = emptyModel,
    currentWorld = emptyWorld,
    bestWorldYetIdentified = emptyWorld
}

provideModel :: WorldModel -> SystemState -> SystemState
provideModel model' state =
    state{model = model'}

provideWorld :: World -> SystemState -> SystemState
provideWorld world' state =
    state{currentWorld = world'}

think :: SystemState -> SystemState
think state =
    let vars = variables $ model state
        -- TODO This assumes only one variable exists - eventually will need to do all combinations
        possibleAssignmentsLists = map allPossibleAssignments vars
        valAssignmentsLists = allCombinations possibleAssignmentsLists
        possibleWorlds = map (\assignmentList -> World{valueAssignments = assignmentList}) valAssignmentsLists
    in
        -- TODO Find the world with the highest utility score and store it in bestWorldYetIdentified
        error "nyi"
