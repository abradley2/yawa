module Language exposing (..)

{-| 
-}


import I18Next


defaultLanguage : I18Next.Translations
defaultLanguage =
    I18Next.fromTree
        [ ( ""
          , I18Next.object
                [ ( "temperatureDescription"
                  , I18Next.string "Here is the Temperature in {{location}}"
                  )
                ]
          )
        ]


