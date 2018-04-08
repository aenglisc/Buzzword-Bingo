module Score exposing
    ( Score
    , viewScore
    , postScore
    )

import Html exposing (..)
import Html.Attributes exposing (..)
import Http
import Entry
import Json.Decode as Decode exposing (Decoder, field, succeed)
import Json.Encode as Encode

type alias Score = 
    { id : Int
    , name : String
    , score : Int
    }

viewScore : Int -> Html msg
viewScore sum =
    div [class "score"]
        [ span [ class "label" ] [ text "Score" ]
        , span [ class "value" ] [ text (toString sum) ]
        ]

postScore : (Result Http.Error Score -> msg) -> String -> List Entry.Entry -> Cmd msg
postScore msg name entries =
    let
        url =
            "http://localhost:3000/scores"

        body =
            encodeScore name entries
                |> Http.jsonBody

        request =
            Http.post url body scoreDecoder
    in
        Http.send msg request   

scoreDecoder : Decoder Score
scoreDecoder =
    Decode.map3 Score
        (field "id" Decode.int)
        (field "name" Decode.string)
        (field "score" Decode.int)

encodeScore : String -> List Entry.Entry -> Encode.Value
encodeScore name entries =
    Encode.object
        [ ("name", Encode.string name)
        , ("score", Encode.int (Entry.sumMarkedPoints entries))
        ]