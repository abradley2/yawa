module Theme exposing (Theme, theme)

import Css


type alias Theme =
    { bodyBackground : Css.Color
    , bodyFont : Css.Color
    , bodySecondaryBackground : Css.Color
    , bodyForeground : Css.Color
    , bodyForegroundFont : Css.Color
    , bodyForegroundBorder : Css.Color
    , bodySecondaryForeground : Css.Color
    , bodySecondaryForegroundFont : Css.Color
    , bodySecondaryForegroundBorder : Css.Color
    , primaryActionBackground : Css.Color
    , primaryActionFont : Css.Color
    , widgetFocusOutline : Css.Color
    }


theme : Theme
theme =
    { bodyBackground = Css.hex "#252e37"
    , bodySecondaryBackground = Css.hex "#2e3a46"
    , bodyForeground = Css.hex "#3b4a5a"
    , bodySecondaryForeground = Css.hex "#4a5b6c"
    , bodyForegroundBorder = Css.hex "#5a6c7e"
    , bodySecondaryForegroundBorder = Css.hex "#6a7d90"
    , bodyFont = Css.hex "#ffffff"
    , bodyForegroundFont = Css.hex "#8a9fb4"
    , bodySecondaryForegroundFont = Css.hex "#d0d9e1"
    , primaryActionBackground = Css.hex "#add8ff"
    , primaryActionFont = Css.hex "#ffffff"
    , widgetFocusOutline = Css.hex "#f5f7fa"
    }
