module Bingo exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Random
import Http
import Json.Decode as Decode exposing (Decoder, field, succeed)
import Json.Encode as Encode

import ViewHelpers exposing (..)
import Entry
import Score

-- MODEL

type GameState = EnteringName | Playing

type alias Model =
    { name : String
    , gameNumber : Int
    , entries : List Entry.Entry 
    , alertMessage : Maybe String
    , nameInput : String
    , gameState : GameState
    }

initialModel : Model
initialModel =
    Model "Anon" 1 [] Nothing "" EnteringName

-- UDPDATE

type Msg
    = NewGame
    | Mark Int
    | NewRandom Int
    | NewEntries (Result Http.Error (List Entry.Entry))
    | CloseAlert
    | ShareScore
    | NewScore (Result Http.Error Score.Score)
    | SetNameInput String
    | SaveName
    | CancelName
    | ChangeGameState GameState

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model = 
    case msg of
        ChangeGameState state ->
            ( { model | gameState = state }, Cmd.none )
        
        SaveName ->
            if String.isEmpty model.nameInput then
                ( model, Cmd.none )
            else
                ( { model | name = model.nameInput,
                            nameInput = "",
                            gameState = Playing}, Cmd.none )

        CancelName ->
            ( { model | nameInput = "",
                        gameState = Playing }, Cmd.none )

        SetNameInput value ->
            ( { model | nameInput = value }, Cmd.none )

        NewRandom randomNumber ->
            ( { model | gameNumber = randomNumber }, Cmd.none )

        ShareScore ->
            ( model, Score.postScore NewScore model.name model.entries )
        
        NewScore (Ok score) ->
            let
                message = 
                    "Your score of "
                        ++ (toString score.score)
                        ++ " was successfully shared!"
            in
                ( { model | alertMessage = Just message }, Cmd.none )
        
        NewScore (Err error) ->
            ( { model | alertMessage = Just (httpErrorToMessage error) }, Cmd.none )

        NewGame ->
            ( { model | gameNumber = model.gameNumber + 1 }, getEntries )

        NewEntries (Ok randomEntries) ->
            ( { model | entries = List.sortBy .points randomEntries }, Cmd.none )

        NewEntries (Err error) ->
            ( { model | alertMessage = Just (httpErrorToMessage error) }, Cmd.none )

        CloseAlert ->
            ( { model | alertMessage = Nothing }, Cmd.none )

        Mark id ->
            ( { model | entries = Entry.markEntryWithId model.entries id }, Cmd.none )

httpErrorToMessage : Http.Error -> String
httpErrorToMessage error =
    case error of
        Http.NetworkError ->
            "Is the server running?"

        Http.Timeout ->
            "Request timed out!"

        Http.BadUrl url ->
            ("Invalid URL: " ++ url)

        Http.BadStatus response ->
            case response.status.code of
                401 ->
                    "Unauthorized"
                404 ->
                    "Not Found"
                code -> 
                    (toString code)

        Http.BadPayload reason response ->
            reason

-- COMMANDS

generateRandomNumber : Cmd Msg
generateRandomNumber = 
    Random.generate NewRandom (Random.int 1 100)

getEntries : Cmd Msg
getEntries =
    Entry.getEntries NewEntries "http://localhost:3000/random-entries"

hasZeroScore : Model -> Bool
hasZeroScore model =
  (Entry.sumMarkedPoints model.entries) == 0  

-- VIEW

viewPlayer : String -> Int -> Html Msg
viewPlayer name gameNumber =
    h2 [ id "info", class "classy" ]
        [ a [ href "#", onClick (ChangeGameState EnteringName) ]
        [ text name ]
        , text (" - Game #" ++ (toString gameNumber)) ]

viewHeader : String -> Html Msg
viewHeader title =
    header [] [ h1 [] [ text title ] ]

viewFooter : Html Msg
viewFooter =
    footer [] [ a [ href "http://elm-lang.org" ]
        [ text "Powered by Elm" ] ]

view : Model -> Html Msg
view model = 
    div [ class "content" ]
        [ viewHeader "BUZZWORD BINGO"
        , viewPlayer model.name model.gameNumber
        , alert CloseAlert model.alertMessage
        , viewNameInput model
        , Entry.viewEntryList Mark model.entries
        , Score.viewScore (Entry.sumMarkedPoints model.entries)
        , div [ class "button-group" ]
              [ button [ class "primary", onClick NewGame ] [ text "New Game" ]
              , button [ class "primary", onClick ShareScore, disabled (hasZeroScore model) ] [ text "Share score" ]
              ]
        -- , div [ class "debug" ] [ text (toString model) ]
        , viewFooter
        ]

viewNameInput : Model -> Html Msg
viewNameInput model =
    case model.gameState of
        EnteringName ->
            div [ class "name-input" ]
                [ input
                    [ type_ "text"
                    , placeholder "Who's playing?"
                    , autofocus True
                    , value model.nameInput
                    , onInput SetNameInput
                    ]
                    []
                , primaryButton SaveName "Save"
                , primaryButton CancelName "Cancel"
                ]
        Playing ->
            text ""

init : (Model, Cmd Msg)
init =
    (initialModel, getEntries)

main : Program Never Model Msg
main =
    Html.program 
        { init = init
        , view = view
        , update = update
        , subscriptions = (always Sub.none)
        }