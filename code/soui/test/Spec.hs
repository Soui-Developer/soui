import Test.Hspec

import Model.Model(WorldModel(..))
import Model.Variable(Variable(..), VariableType(..))
import Kernel(
    initialState,
    provideModel,
    provideWorld,
    provideUtilityFunction,
    think,
    bestWorldYetIdentified,
    messagesForUser,
    UtilityFunction)
import Model.World(World(..), ValueAssignment(..), Value(..), getVariableValue)
import Model.Message(Message(..))

main :: IO ()
main = hspec $ do
    describe "Kernel" $ do
        it "supports Scenario 1 - Making a boolean be true" $ do
            let state0 = think initialState

            messagesForUser state0 `shouldBe` [InitialModelRequest]

            let model' = WorldModel {
                                variables = [
                                    Variable {
                                        name = "worldPeace",
                                        variableType = BooleanType
                                    }
                                ]
                            }
                state1 = think $ provideModel model' state0

            messagesForUser state1 `shouldBe` [InitialWorldRequest]

            let world' = World {
                    valueAssignments = [
                        ValueAssignment {variableName = "worldPeace", assignedValue = BooleanValue False}
                    ]
                }
                state2 = provideWorld world' state1
                state3 = think state2

            bestWorldYetIdentified state3 `shouldBe` (World {
                valueAssignments = [
                    ValueAssignment {variableName = "worldPeace", assignedValue = BooleanValue False}
                ]
            })

            messagesForUser state3 `shouldBe` [InitialUtilityFunctionRequest]

            let state4 = provideUtilityFunction worldPeaceUtilityFunction state3
                state5 = think state4
                idealWorld = World {
                                 valueAssignments = [
                                     ValueAssignment {variableName = "worldPeace", assignedValue = BooleanValue True}
                                 ]
                             }

            -- After some consideration, the system realises that it would be preferable for worldPeace to be true :)
            bestWorldYetIdentified state5 `shouldBe` idealWorld

            messagesForUser state5 `shouldBe` [ActionsTowardWorldRequest idealWorld]


worldPeaceUtilityFunction :: UtilityFunction
worldPeaceUtilityFunction world =
    let
        worldPeace = getVariableValue world "worldPeace"
    in
        case worldPeace of
            BooleanValue True -> 100
            BooleanValue False -> 0
