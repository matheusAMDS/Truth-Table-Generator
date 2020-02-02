module TruthTable exposing (truthTable)

import Expression exposing (Expression(..))
import ExpressionParser exposing (read)
import List.Extra exposing (cycle, transpose, zip)


addElem : List a -> a -> List a
addElem list elem =
    list ++ [ elem ]


genValues : Int -> List (List Bool)
genValues n =
    let
        len =
            2 ^ n

        generate i =
            if i == 1 then
                [ cycle len [ True, False ] ]

            else
                List.map (\c -> List.repeat i c) [ True, False ]
                    |> List.concat
                    |> cycle len
                    |> addElem (generate (i // 2))
    in
    generate (len // 2)


getProps : Expression -> List String
getProps expression =
    let
        auxGetProps op =
            case op of
                Only name ->
                    [ name ]

                Not br ->
                    getProps br

                And br1 br2 ->
                    getProps br1 ++ getProps br2

                Or br1 br2 ->
                    getProps br1 ++ getProps br2

                Cond br1 br2 ->
                    getProps br1 ++ getProps br2

                Bcon br1 br2 ->
                    getProps br1 ++ getProps br2
    in
    auxGetProps expression
        |> List.Extra.unique


eval : Expression -> List ( String, Bool ) -> Bool
eval expression values =
    case expression of
        Only name ->
            List.filter (\( n, _ ) -> n == name) values
                |> List.head
                |> Maybe.withDefault ( "", False )
                |> Tuple.second

        Not br ->
            not (eval br values)

        And br1 br2 ->
            eval br1 values && eval br2 values

        Or br1 br2 ->
            eval br1 values || eval br2 values

        Cond br1 br2 ->
            not (eval br1 values) || eval br2 values

        Bcon br1 br2 ->
            eval br1 values
                && eval br1 values
                |> (||) (not (eval br1 values) && not (eval br2 values))


truthTable : String -> Maybe (List ( String, List Bool ))
truthTable string =
    case read string of
        Nothing ->
            Nothing

        Just expression ->
            let
                props =
                    getProps expression

                values =
                    genValues <| List.length props

                ref =
                    zip props values

                ref2 =
                    List.map (\( n, bs ) -> zip (cycle (List.length bs) [ n ]) bs) ref
            in
            transpose ref2
                |> List.map (\val -> eval expression val)
                |> Tuple.pair string
                |> addElem ref
                |> Just
