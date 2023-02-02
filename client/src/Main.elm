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
import Url exposing (Url)
import Url.Builder
import Url.Parser exposing (Parser)
import Url.Parser.Query


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
                , expect = Http.expectWhatever ReceivedForecastResponse
                , timeout = Nothing
                , headers = []
                , tracker = Nothing
                , url = Url.Builder.absolute [ "locations", city, "weather" ] []
                }

        EffectSearch (QueryLatLng lat lng) ->
            Http.request
                { method = "GET"
                , body = Http.emptyBody
                , expect = Http.expectWhatever ReceivedForecastResponse
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
    , forecastResponse : WebData ()
    }


type Msg
    = ReceivedForecastResponse (Result Http.Error ())
    | ReceivedLocationsResponse (Result Http.Error (List String))
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
                ]
            ]
          <|
            List.map locationLink locations
        ]


errorView : Http.Error -> Html Msg
errorView =
    always <|
        Html.div
            []
            [ Html.text "There was an application error" ]


locationLink : String -> Html Msg
locationLink location =
    Html.a
        [ Attr.href <| "?city=" ++ location
        , Events.preventDefaultOn
            "click"
            (Decode.succeed ( LocationClicked location, True ))
        ]
        [ Html.text location ]


view : Model -> Html Msg
view model =
    case model.locationsResponse of
        Loading ->
            loadingView

        Success locations ->
            readyView locations model

        Failure err ->
            errorView err

        NotAsked ->
            Html.text "There was a fatal application error, oh no"


init : Flags -> ( Model, Effect )
init flags =
    let
        query : Maybe Query
        query =
            MaybeX.join (Url.Parser.parse queryParser flags.url)
    in
    ( { flags = flags
      , query = query
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
        LocationClicked city ->
            let
                url =
                    model.flags.url

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

        ReceivedForecastResponse (Ok res) ->
            ( { model | forecastResponse = Success res }
            , EffectNone
            )

        ReceivedForecastResponse (Err err) ->
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
