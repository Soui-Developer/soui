module Kernel(
    SystemState(bestWorldYetIdentified),
    initialState,
    provideModel,
    provideWorld,
    provideUtilityFunction,
    think,
    allCombinations,
    messagesForUser,
    UtilityFunction
) where

import Data.List(sortBy)
import Data.Ord(comparing)

import Model.Model(WorldModel(variables), emptyModel)
import Model.World(World(..), emptyWorld)
import Computation.Enumeration(allPossibleAssignments)
import Utils(allCombinations)
import Model.Message(Message(..))

type UtilityFunction = World -> Int

data SystemState = SystemState {
    messageCounter :: Int,
    model :: WorldModel,
    -- | The current state of the real world (as far as the System is aware)
    currentWorld :: World,
    -- | A utility function to evaluate how favourable a world state is
    utilityFunction :: UtilityFunction,
    -- | Whether a utility function has been provided by the user
    utilityFunctionProvided :: Bool,
    -- | The most optimal world the system has been able to find so far
    bestWorldYetIdentified :: World,
    -- | The set of messages for the user to view (most-recent first)
    messagesForUser :: [Message]
}

initialState :: SystemState
initialState = SystemState {
    messageCounter = 0,
    model = emptyModel,
    currentWorld = emptyWorld,
    utilityFunction = const 0,
    utilityFunctionProvided = False,
    bestWorldYetIdentified = emptyWorld,
    messagesForUser = []
}

provideModel :: WorldModel -> SystemState -> SystemState
provideModel model' state =
    state{model = model'}

provideWorld :: World -> SystemState -> SystemState
provideWorld world' state =
    state{currentWorld = world'}

provideUtilityFunction :: UtilityFunction -> SystemState -> SystemState
provideUtilityFunction utilityFunction' state =
    state{utilityFunction = utilityFunction',
          utilityFunctionProvided = True}

think :: SystemState -> SystemState
think state =
    let vars = variables $ model state
        possibleAssignmentsLists = map allPossibleAssignments vars
        valAssignmentsLists = allCombinations possibleAssignmentsLists
        possibleWorlds = (map (\assignmentList -> World{valueAssignments = assignmentList}) valAssignmentsLists) :: [World]
        worldUtilities = (map (utilityFunction state) possibleWorlds) :: [Int]
        worldsAndUtilities = zip possibleWorlds worldUtilities
        bestWorld = fst $ last $ sortBy (comparing snd) worldsAndUtilities
        newState = state{bestWorldYetIdentified = bestWorld}
    in
        newState{
            messagesForUser = [getMessage newState]
        }

getMessage :: SystemState -> Message
getMessage state =
    let modelMissing = model state == emptyModel
        worldMissing = currentWorld state == emptyWorld
        utilityFunctionMissing = not $ utilityFunctionProvided state
    in
    if modelMissing
    then InitialModelRequest
    else
        if worldMissing
        then InitialWorldRequest
        else
            if utilityFunctionMissing
            then InitialUtilityFunctionRequest
            else ActionsTowardWorldRequest (bestWorldYetIdentified state)
