module Main exposing (main)

import Browser
import Html exposing (Html, a, button, div, footer, h1, i, input, li, span, table, tbody, td, text, thead, tr, ul)
import Html.Attributes exposing (class, href, id, placeholder, target, type_, value)
import Html.Entity as Ent
import Html.Events exposing (onClick, onInput)
import List.Extra exposing (transpose)
import TruthTable exposing (truthTable)



-- Main


main : Program () Model Msg
main =
    Browser.sandbox
        { init = init
        , update = update
        , view = view
        }



-- Model


type alias Model =
    { expression : String
    , showInstructions : Bool
    , truthTable : Maybe (List ( String, List Bool ))
    }


init : Model
init =
    { expression = ""
    , showInstructions = True
    , truthTable = Just [ ( "", [] ) ]
    }



-- Update


type Msg
    = TruthTable
    | OpenInst
    | CloseInst
    | Expr String


update : Msg -> Model -> Model
update msg model =
    case msg of
        Expr str ->
            { model | expression = str }

        TruthTable ->
            { model | truthTable = truthTable model.expression }

        OpenInst ->
            { model | showInstructions = True }

        CloseInst ->
            { model | showInstructions = False }


view : Model -> Html Msg
view model =
    div [ id "app" ]
        [ h1 [ class "title" ]
            [ text "Truth Table Generator"
            ]
        , showInstructions model
        , div [ class "user-input" ]
            [ input
                [ placeholder "Ex.: (A | B)"
                , onInput Expr
                , value model.expression
                ]
                []
            , button
                [ type_ "submit"
                , onClick TruthTable
                ]
                [ text "Generate"
                ]
            ]
        , div [ class "result" ]
            [ resultTruthTable model ]
        , footer []
            [ span [ class "author" ]
                [ text "Developed by "
                , a
                    [ href "http://github.com/matheusAMDS"
                    , target "_blank"
                    ]
                    [ text "Matheus Andrade" ]
                , text " | Email: matheusa56@gmail.com"
                ]
            ]
        ]


showInstructions : Model -> Html Msg
showInstructions model =
    if model.showInstructions then
        ul [ class "instructions" ]
            [ button
                [ onClick CloseInst
                , class "show-content"
                ]
                [ i [ class "fas fa-angle-up" ] []
                ]
            , li []
                [ text "Remember to put each operation in parenthesis. Ex.: (X | (Y | Z));"
                ]
            , li []
                [ text ("To enter " ++ Ent.not ++ "P : ~P;")
                ]
            , li []
                [ text ("To enter P " ++ Ent.and ++ " Q : P & Q;")
                ]
            , li []
                [ text ("To enter P " ++ Ent.or ++ " Q : P | Q;")
                ]
            , li []
                [ text ("To enter P " ++ Ent.rarr ++ " Q : P > Q;")
                ]
            , li []
                [ text ("To enter P " ++ Ent.harr ++ " Q : P % Q")
                ]
            ]

    else
        ul [ class "instructions" ]
            [ li []
                [ span [ class "help-msg" ] [ text "Need some help?" ]
                , button
                    [ onClick OpenInst
                    , class "show-content"
                    ]
                    [ i [ class "fas fa-angle-down" ] []
                    ]
                ]
            ]


resultTruthTable : Model -> Html msg
resultTruthTable model =
    case model.truthTable of
        Nothing ->
            div [ class "error" ]
                [ span [ class "error-msg" ]
                    [ text "Impossible to generate the expression. If you need some help, check the yellow box."
                    ]
                ]

        Just result ->
            let
                unzipped =
                    List.unzip result

                headers =
                    Tuple.first unzipped

                body =
                    Tuple.second unzipped
                        |> transpose

                boolToString bool =
                    if bool then
                        "T"

                    else
                        "F"

                toTableRow list f =
                    List.map (\d -> td [] [ text <| f d ]) list
            in
            if result == [ ( "", [] ) ] then
                div [] []

            else
                table []
                    [ thead []
                        [ tr [] (toTableRow headers identity)
                        ]
                    , tbody []
                        (List.map (\ld -> tr [] (toTableRow ld boolToString)) body)
                    ]
