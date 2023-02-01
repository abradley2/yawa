port module Main exposing (Effect(..), Flags, Model, Msg, Query, init, main, update, view)

import Browser
import Html.Styled as Html exposing (Html)
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
    | EffectSearch Query


perform : Effect -> Cmd Msg
perform effect =
    case effect of
        EffectNone ->
            Cmd.none

        EffectSearch (QueryCity city) ->
            Http.request
                { method = "GET"
                , body = Http.emptyBody
                , expect = Http.expectWhatever ReceivedSearchResponse
                , timeout = Nothing
                , headers = []
                , tracker = Nothing
                , url = Url.Builder.absolute [ "search" ] [ Url.Builder.string "city" city ]
                }

        EffectSearch (QueryLatLng lat lng) ->
            Http.request
                { method = "GET"
                , body = Http.emptyBody
                , expect = Http.expectWhatever ReceivedSearchResponse
                , timeout = Nothing
                , headers = []
                , tracker = Nothing
                , url =
                    Url.Builder.absolute
                        [ "search" ]
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
    { query : Maybe Query
    , searchResults : WebData ()
    }


type Msg
    = ReceivedSearchResponse (Result Http.Error ())


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


view : Model -> Html Msg
view _ =
    Html.text "Hello World"


init : Flags -> ( Model, Effect )
init flags =
    let
        query : Maybe Query
        query =
            MaybeX.join (Url.Parser.parse queryParser flags.url)
    in
    ( { query = query
      , searchResults =
            if MaybeX.isJust query then
                Loading

            else
                NotAsked
      }
    , case query of
        Just searchParams ->
            EffectSearch searchParams

        Nothing ->
            EffectNone
    )


update : Msg -> Model -> ( Model, Effect )
update msg model =
    case msg of
        ReceivedSearchResponse (Ok res) ->
            ( { model | searchResults = Success res }
            , EffectNone
            )

        ReceivedSearchResponse (Err err) ->
            ( { model | searchResults = Failure err }
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
