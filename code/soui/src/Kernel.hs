module Kernel(
    SystemState(bestWorldYetIdentified),
    initialState,
    provideModel,
    provideCurrentWorld,
    provideUtilityFunction,
    provideAction,
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
import Model.Action(Action(..), ActionName(..))

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
    -- | The set of actions available to the system
    availableActions :: [Action],
    -- | A map of each action to its consequence when applied to the current world
    -- It may be set to Nothing if the value becomes invalidated by changing the actions or the world
    actionConsequences :: Maybe [(ActionName, World)],
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
    availableActions = [],
    actionConsequences = Nothing,
    bestWorldYetIdentified = emptyWorld,
    messagesForUser = []
}

provideModel :: WorldModel -> SystemState -> SystemState
provideModel model' state =
    state{model = model'}

provideCurrentWorld :: World -> SystemState -> SystemState
provideCurrentWorld world' state =
    state{
        currentWorld = world',
        actionConsequences = Nothing
    }

provideUtilityFunction :: UtilityFunction -> SystemState -> SystemState
provideUtilityFunction utilityFunction' state =
    state{utilityFunction = utilityFunction',
          utilityFunctionProvided = True}

provideAction :: Action -> SystemState -> SystemState
provideAction action state = state {
    availableActions = action : (availableActions state),
    actionConsequences = Nothing
    }

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
        newConsequences = computeConsequences (currentWorld state) (availableActions state)
        newState2 = newState{actionConsequences = Just newConsequences}

    in
        newState2{
            messagesForUser = [getMessage newState2]
        }

computeConsequences :: World -> [Action] -> [(ActionName, World)]
computeConsequences world actions = map (computeConsequence world) actions

computeConsequence :: World -> Action -> (ActionName, World)
computeConsequence world action =
    (name action, effect action world)

getMessage :: SystemState -> Message
getMessage state =
    let modelMissing = model state == emptyModel
        worldMissing = currentWorld state == emptyWorld
        utilityFunctionMissing = not $ utilityFunctionProvided state
        nextActionOnPathToOptimalWorld = case actionConsequences state of
            Nothing -> Nothing
            (Just consequences) ->
                let actions = filter (\(_, world) -> world == (bestWorldYetIdentified state)) consequences
                in
                    if null actions
                    then Nothing
                    else (Just $ head actions)

    in
    if modelMissing
    then InitialModelRequest
    else
        if worldMissing
        then InitialWorldRequest
        else
            if utilityFunctionMissing
            then InitialUtilityFunctionRequest
            else
            case nextActionOnPathToOptimalWorld of
                (Just (actionName, _)) -> PerformActionRequest actionName
                Nothing -> ActionsTowardWorldRequest (bestWorldYetIdentified state)
