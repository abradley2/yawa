module Language exposing (..)

{-| 
-}


import I18Next


defaultLanguage : I18Next.Translations
defaultLanguage =
    I18Next.fromTree
        [ ( "", I18Next.object [ ( "hello", I18Next.string "World" ) ] ) ]


