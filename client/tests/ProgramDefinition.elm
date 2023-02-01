module ProgramDefinition exposing (createProgramDefinition)

import Html.Styled
import Main exposing (Effect, Flags, Model, Msg)
import ProgramTest exposing (ProgramDefinition)


createProgramDefinition : ProgramDefinition Flags Model Msg Effect
createProgramDefinition =
    ProgramTest.createElement
        { init = Main.init
        , view = Main.view >> Html.Styled.toUnstyled
        , update = Main.update
        }
