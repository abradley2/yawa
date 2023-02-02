module TextInput exposing (..)

import Css
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as Attr
import Html.Styled.Events as Events
import Theme exposing (theme)


type alias Config msg =
    { label : String
    , id : String
    , value : String
    , onInput : String -> msg
    }


textInputView : Config msg -> Html msg
textInputView cfg =
    let
        errorId : String
        errorId =
            cfg.id ++ "--error"
    in
    Html.div
        [ Attr.css
            [ Css.display Css.inlineFlex
            , Css.flexDirection Css.column
            , Css.position Css.relative
            ]
        ]
        [ Html.label
            [ Attr.for cfg.id
            , Attr.css
                [ Css.marginBottom (Css.px 8)
                , Css.color theme.widgetFocusOutline
                , Css.fontSize (Css.px 18)
                , Css.letterSpacing (Css.px 1.5)
                ]
            ]
            [ Html.text cfg.label
            ]
        , Html.input
            [ Attr.value cfg.value
            , Attr.id cfg.id
            , Attr.attribute "aria-errormessage" errorId
            , Attr.css
                [ Css.padding2 (Css.px 8) (Css.px 8)
                , Css.fontSize (Css.px 16)
                , Css.focus
                    [ Css.outlineOffset (Css.px 2)
                    , Css.outline3 (Css.px 2) Css.solid theme.widgetFocusOutline
                    ]
                ]
            , Events.onInput cfg.onInput
            ]
            []
        , Html.div
            [ Attr.id errorId
            , Attr.attribute "aria-live" "polite"
            ]
            [ Html.text ""
            ]
        ]
