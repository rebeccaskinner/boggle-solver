module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (srcdoc, style, height, width)
import Html.Events exposing (..)
import Http
import Json.Decode as Decode
import Json.Encode as Encode
import Time as Time

-- MODEL
type alias Model =
    { dimension : Int
    , inputString : String
    , resultString : List String
    , errMsg : Maybe String
    }

mkModel : Model
mkModel =
    { dimension = 4
    , inputString = ""
    , resultString = []
    , errMsg = Nothing
    }

 -- UPDATE
type Msg
    = UpdateDimensions Int
    | UpdateInputString String
    | SolveRequest
    | SolveResponse (Result Http.Error (List String))

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateDimensions n -> ({model | dimension = n}, Cmd.none)
        UpdateInputString s -> ({model | inputString = s}, Cmd.none)
        SolveRequest -> (model, solveBoggleRequest model)
        SolveResponse (Err error) ->
            let msgStr = stringifyErr error
            in ({model | errMsg = Just msgStr}, Cmd.none)
        SolveResponse (Ok results) ->
            ({model | resultString = results, errMsg = Nothing}, Cmd.none)

stringifyErr : Http.Error -> String
stringifyErr e =
    case e of
        Http.BadUrl s       -> "Bad URL" ++ s
        Http.Timeout        -> "Request Timeout"
        Http.NetworkError   -> "Network Error"
        Http.BadStatus _    -> "Bad Status"
        Http.BadPayload _ _ -> "Bad Payload"

solveBoggleRequest : Model -> Cmd Msg
solveBoggleRequest m =
    solveBoggle m |> Http.send SolveResponse

solveBoggle : Model -> Http.Request (List String)
solveBoggle m =
    Http.request
        { method = "POST"
        , headers = []
        , url = "http://localhost:8080/boggle"
        , body = Http.jsonBody (boggleRequestBody m)
        , expect = Http.expectJson boggleParseResponse
        , timeout = Nothing
        , withCredentials = False
        }

boggleParseResponse : Decode.Decoder (List String)
boggleParseResponse =
    Decode.list Decode.string

boggleRequestBody : Model -> Encode.Value
boggleRequestBody m =
    Encode.object [ ("reqSize", Encode.int m.dimension)
                  , ("reqData", Encode.string m.inputString)
                  ]

-- VIEW
view : Model -> Html Msg
view m = div [ style "width" "100%"
             , style "height" "100%"]
             [ inputBox
             , solveButton
             , outputDiv m
             ]

solveButton : Html Msg
solveButton =
    label [ Html.Attributes.style "padding" "20px" ]
          [ input
              [ Html.Attributes.type_ "button"
              , Html.Attributes.name "Solve"
              , onClick (SolveRequest)
              ]
              []
          , text "Solve!"
          ]

inputBox : Html Msg
inputBox =
    div []
        [ h2 [] [ text "Input" ]
        , textarea [ onInput UpdateInputString ] []
        ]

outputDiv : Model -> Html Msg
outputDiv m =
    ul [] (List.map (\l -> li [] [text l]) m.resultString)
-- outputBox : Model -> Html Msg
-- outputBox m =
--     let innerHTML = Maybe.withDefault "" m.outputData
--     in div [style "width" "100%", style "height" "100%"]
--         [ div [] [ h2 [] [ text "Preview" ] ]
--         , iframe [style "height" "800px", style "width" "80%",  srcdoc innerHTML ] []
--         ]

subscriptions : Model -> Sub Msg
subscriptions model = Sub.none

init : () -> ( Model, Cmd Msg )
init = const (mkModel, Cmd.none)

main =
  Browser.element { init = init
                  , view = view
                  , update = update
                  , subscriptions = subscriptions
                  }

-- UTIL
formatListURL : String
formatListURL = "/supportedformats"

previewURL : String -> String
previewURL fmt = "/html?format=" ++fmt

defaultFormatList : List String
defaultFormatList = [ "markdown" ]

const : a -> b -> a
const val = \_ -> val
