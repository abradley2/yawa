module MainTest exposing (suite)

import Expect exposing (Expectation)
import Main exposing (Effect, Model, Msg)
import ProgramDefinition
import ProgramTest exposing (ProgramTest)
import SimulatedEffects.Main
import Test exposing (Test)
import Test.Html.Selector as Selector
import Url


testWithUrl : String -> (ProgramTest Model Msg Effect -> Expectation) -> Expectation
testWithUrl urlString expect =
    Url.fromString urlString
        |> Maybe.map
            (\url ->
                expect
                    (ProgramTest.start
                        { url = url }
                        (ProgramTest.withSimulatedEffects
                            SimulatedEffects.Main.perform
                            ProgramDefinition.createProgramDefinition
                        )
                    )
            )
        |> Maybe.withDefault (Expect.fail "Invalid Url")


suite : Test
suite =
    Test.describe "main"
        [ Test.test "it should say hello" <|
            \_ ->
                testWithUrl "http://localhost:1234?city=stamford"
                    (ProgramTest.expectViewHas
                        [ Selector.text "Loading"
                        ]
                    )
        ]
