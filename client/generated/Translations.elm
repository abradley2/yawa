module Translations exposing (..)

{-| 
-}


import I18Next


temperatureDescription :
    List I18Next.Translations -> { location : String } -> String
temperatureDescription translations replacements =
    I18Next.trf
        translations
        I18Next.Curly
        "temperatureDescription"
        [ ( "location", replacements.location ) ]


