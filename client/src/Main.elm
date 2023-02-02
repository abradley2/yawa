port module Main exposing (Effect(..), Flags, Model, Msg, Query, init, main, update, view)

import Browser
import Css
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as Attr
import Html.Styled.Events as Events
import Http
import Json.Decode as Decode exposing (Decoder, Value)
import Maybe.Extra as MaybeX
import RemoteData exposing (RemoteData(..), WebData)
import Result.Extra as ResultX
import TextInput
import Theme exposing (theme)
import Url exposing (Url)
import Url.Builder
import Url.Parser exposing (Parser)
import Url.Parser.Query


type alias WeatherAttribute =
    { main : String
    , description : String
    }


weatherAttributeDecoder : Decoder WeatherAttribute
weatherAttributeDecoder =
    Decode.map2
        WeatherAttribute
        (Decode.field "main" Decode.string)
        (Decode.field "description" Decode.string)


type alias ForecastResponse =
    { weatherAttributes : List WeatherAttribute
    , temperature : Float
    }


forecastResponseDecoder : Decoder ForecastResponse
forecastResponseDecoder =
    Decode.map2
        ForecastResponse
        (Decode.at [ "current", "weather" ] (Decode.list weatherAttributeDecoder))
        (Decode.at [ "current", "temp" ] Decode.float)


type Effect
    = EffectNone
    | EffectBatch (List Effect)
    | EffectFetchLocations
    | EffectSearch Query
    | EffectReplaceUrl String


port replaceUrl : String -> Cmd msg


perform : Effect -> Cmd Msg
perform effect =
    case effect of
        EffectNone ->
            Cmd.none

        EffectReplaceUrl url ->
            replaceUrl url

        EffectBatch [] ->
            Cmd.none

        EffectBatch (eff :: next) ->
            Cmd.batch (perform eff :: List.map perform next)

        EffectFetchLocations ->
            Http.request
                { method = "GET"
                , body = Http.emptyBody
                , expect =
                    Http.expectJson
                        ReceivedLocationsResponse
                        (Decode.list Decode.string)
                , timeout = Nothing
                , headers = []
                , tracker = Nothing
                , url = Url.Builder.absolute [ "locations" ] []
                }

        EffectSearch (QueryCity city) ->
            Http.request
                { method = "GET"
                , body = Http.emptyBody
                , expect = Http.expectJson (ReceivedForecastResponse (QueryCity city)) forecastResponseDecoder
                , timeout = Nothing
                , headers = []
                , tracker = Nothing
                , url = Url.Builder.absolute [ "locations", city, "weather" ] []
                }

        EffectSearch (QueryLatLng lat lng) ->
            Http.request
                { method = "GET"
                , body = Http.emptyBody
                , expect = Http.expectJson (ReceivedForecastResponse (QueryLatLng lat lng)) forecastResponseDecoder
                , timeout = Nothing
                , headers = []
                , tracker = Nothing
                , url =
                    Url.Builder.absolute
                        [ "weather" ]
                        [ Url.Builder.string "lat" (String.fromFloat lat)
                        , Url.Builder.string "lng" (String.fromFloat lng)
                        ]
                }


port logError : String -> Cmd never


queryParser : Parser (Maybe Query -> a) a
queryParser =
    Url.Parser.query <|
        Url.Parser.Query.map3
            (\cityParam latParam lngParam ->
                Nothing
                    |> MaybeX.or (Maybe.map QueryCity cityParam)
                    |> MaybeX.or
                        (Maybe.map2 QueryLatLng
                            (Maybe.andThen String.toFloat latParam)
                            (Maybe.andThen String.toFloat lngParam)
                        )
            )
            (Url.Parser.Query.string "city")
            (Url.Parser.Query.string "lat")
            (Url.Parser.Query.string "lng")


type Query
    = QueryCity String
    | QueryLatLng Float Float


type alias Flags =
    { url : Url
    }


type alias Model =
    { flags : Flags
    , query : Maybe Query
    , locationsResponse : WebData (List String)
    , forecastResponse : WebData ( String, ForecastResponse )
    , filter : String
    }


type Msg
    = ReceivedForecastResponse Query (Result Http.Error ForecastResponse)
    | ReceivedLocationsResponse (Result Http.Error (List String))
    | FilterChanged String
    | LocationClicked String


flagsDecoder : Decoder Flags
flagsDecoder =
    Decode.map
        Flags
        (Decode.field "url" urlDecoder)


urlDecoder : Decoder Url
urlDecoder =
    Decode.string
        |> Decode.andThen
            (\urlString ->
                case Url.fromString urlString of
                    Just url ->
                        Decode.succeed url

                    Nothing ->
                        Decode.fail <| "Invalid URL " ++ urlString
            )


cardView : Html Msg -> Html Msg
cardView =
    List.singleton
        >> Html.div
            [ Attr.css
                [ Css.backgroundColor theme.bodyForeground
                , Css.color theme.bodyFont
                , Css.padding (Css.px 20)
                , Css.borderRadius (Css.px 10)
                , Css.margin2 (Css.px 20) (Css.px 0)
                ]
            ]


loadingView : Html Msg
loadingView =
    Html.text "Loading..."


readyView : List String -> Model -> Html Msg
readyView locations model =
    Html.div
        []
        [ Html.div
            [ Attr.css
                [ Css.displayFlex
                , Css.flexDirection Css.column
                , Css.alignItems Css.center
                ]
            ]
            [ cardView <| weatherView model
            , cardView <|
                Html.div
                    []
                    [ TextInput.textInputView
                        { value = model.filter
                        , id = "filter-input"
                        , label = "Search Cities"
                        , onInput = FilterChanged
                        }
                    ]
            ]
        , Html.div
            [ Attr.css
                [ Css.displayFlex
                , Css.flexDirection Css.column
                , Css.justifyContent Css.center
                ]
            ]
          <|
            (locations
                |> List.filter (String.toUpper >> String.contains (String.toUpper model.filter))
                |> List.map (locationLink model)
            )
        ]


weatherView : Model -> Html Msg
weatherView model =
    case model.forecastResponse of
        Loading ->
            loadingView

        Failure error ->
            errorView error

        Success ( location, forecast ) ->
            Html.div
                []
                [ Html.div
                    [ Attr.css
                        [ Css.color theme.primaryActionBackground
                        , Css.fontSize (Css.px 20)
                        ]
                    ]
                    [ Html.text <| "Temperature in " ++ location ]
                , Html.hr [] []
                , Html.text <| String.fromFloat forecast.temperature ++ "°F"
                , Html.hr [] []
                , Html.div
                    []
                    (List.map
                        (\attr ->
                            Html.div
                                []
                                [ Html.text <| attr.main ++ ": " ++ attr.description
                                ]
                        )
                        forecast.weatherAttributes
                    )
                ]

        NotAsked ->
            Html.text "No Location Selected"


errorView : Http.Error -> Html Msg
errorView =
    always <|
        Html.div
            []
            [ Html.text "There was an application error" ]


locationLink : Model -> String -> Html Msg
locationLink model location =
    Html.a
        [ Attr.href <| "?city=" ++ location
        , Attr.css
            [ Css.color theme.primaryActionFont
            , Css.fontWeight Css.lighter
            , Css.fontSize (Css.px 20)
            , Css.letterSpacing (Css.px 1.5)
            , Css.display Css.inlineBlock
            , Css.padding2 (Css.px 16) (Css.px 8)
            , Css.textDecoration Css.none
            , Css.alignSelf Css.center
            , Css.hover
                [ Css.textDecoration Css.underline
                ]
            ]
        , Events.preventDefaultOn
            "click"
            (Decode.succeed ( LocationClicked location, True ))
        ]
        (getOptionText location model.filter)


view : Model -> Html Msg
view model =
    Html.div
        [ Attr.css
            [ Css.color theme.bodyFont
            , Css.backgroundColor theme.bodyBackground
            , Css.height (Css.vh 100)
            , Css.width (Css.vw 100)
            , Css.overflow Css.auto
            ]
        ]
        [ case model.locationsResponse of
            Loading ->
                loadingView

            Success locations ->
                readyView locations model

            Failure err ->
                errorView err

            NotAsked ->
                Html.text "There was a fatal application error, oh no"
        ]


init : Flags -> ( Model, Effect )
init flags =
    let
        query : Maybe Query
        query =
            MaybeX.join (Url.Parser.parse queryParser flags.url)
    in
    ( { flags = flags
      , query = query
      , filter = ""
      , locationsResponse = Loading
      , forecastResponse =
            if MaybeX.isJust query then
                Loading

            else
                NotAsked
      }
    , EffectBatch
        [ case query of
            Just searchParams ->
                EffectSearch searchParams

            Nothing ->
                EffectNone
        , EffectFetchLocations
        ]
    )


update : Msg -> Model -> ( Model, Effect )
update msg model =
    case msg of
        FilterChanged filter ->
            ( { model | filter = filter }
            , EffectNone
            )

        LocationClicked city ->
            let
                url : Url
                url =
                    model.flags.url

                nextUrl : Url
                nextUrl =
                    { url
                        | query = Just <| "city=" ++ city
                    }
            in
            ( { model | forecastResponse = Loading }
            , EffectBatch
                [ EffectSearch (QueryCity city)
                , EffectReplaceUrl (Url.toString nextUrl)
                ]
            )

        ReceivedForecastResponse query (Ok res) ->
            ( { model
                | forecastResponse =
                    Success
                        ( case query of
                            QueryCity city ->
                                city

                            QueryLatLng lat lon ->
                                String.fromFloat lat ++ ", " ++ String.fromFloat lon
                        , res
                        )
              }
            , EffectNone
            )

        ReceivedForecastResponse _ (Err err) ->
            ( { model | forecastResponse = Failure err }
            , EffectNone
            )

        ReceivedLocationsResponse (Ok locations) ->
            ( { model | locationsResponse = Success locations }
            , EffectNone
            )

        ReceivedLocationsResponse (Err err) ->
            ( { model | locationsResponse = Failure err }
            , EffectNone
            )


main : Program Value (Result Decode.Error Model) Msg
main =
    Browser.element
        { init =
            \flagsJson ->
                case Decode.decodeValue flagsDecoder flagsJson |> Result.map init of
                    Ok ( model, cmd ) ->
                        ( Ok model, perform cmd )

                    Err err ->
                        ( Err err, logError <| Decode.errorToString err )
        , subscriptions = always Sub.none
        , view =
            \result ->
                Result.map view result
                    |> Result.mapError (Decode.errorToString >> Html.text)
                    |> ResultX.merge
                    |> Html.toUnstyled
        , update =
            \msg result ->
                Result.map (\model -> update msg model) result
                    |> Result.map (Tuple.mapFirst Ok >> Tuple.mapSecond perform)
                    |> Result.withDefault ( result, Cmd.none )
        }


type alias StringGetter msg =
    { workingWordList : List (Html msg)
    , currentString : String
    }


getOptionText : String -> String -> List (Html msg)
getOptionText optionLabel searchValue =
    List.foldl
        (getOptionTextHelper searchValue)
        { workingWordList = []
        , currentString = ""
        }
        (String.split "" optionLabel)
        |> (\result ->
                result.workingWordList ++ [ Html.span [] [ Html.text result.currentString ] ]
           )


getOptionTextHelper : String -> String -> StringGetter msg -> StringGetter msg
getOptionTextHelper matcher letter stringGetter =
    let
        nextString =
            stringGetter.currentString ++ letter

        index =
            List.head (String.indices (String.toUpper matcher) (String.toUpper nextString))

        found =
            Maybe.withDefault False <| Maybe.map (\_ -> True) index

        rightPart =
            Maybe.map
                (\i ->
                    Html.b
                        [ Attr.css
                            [ Css.color theme.primaryActionBackground
                            ]
                        ]
                        [ Html.text <| String.dropLeft i nextString ]
                )
                index

        leftPart =
            Maybe.map
                (\i -> Html.span [] [ Html.text <| String.left i nextString ])
                index
    in
    { stringGetter
        | workingWordList =
            Maybe.withDefault stringGetter.workingWordList <|
                Maybe.map2
                    (\l r -> stringGetter.workingWordList ++ [ l, r ])
                    leftPart
                    rightPart
        , currentString =
            if found then
                ""

            else
                nextString
    }
