module Translations exposing (..)

{-| -}

import I18Next


hello : List I18Next.Translations -> String
hello translations =
    I18Next.tf translations "hello"
