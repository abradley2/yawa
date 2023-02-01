module SimulatedEffects.Main exposing (perform)

import Main
import ProgramTest exposing (SimulatedEffect)
import SimulatedEffect.Cmd


perform : Main.Effect -> SimulatedEffect Main.Msg
perform effect =
    case effect of
        Main.EffectNone ->
            SimulatedEffect.Cmd.none

        Main.EffectSearch _ ->
            SimulatedEffect.Cmd.none
