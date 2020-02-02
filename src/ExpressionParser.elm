module ExpressionParser exposing (read)

import Expression exposing (Expression(..))
import Parser exposing (..)


type ExpressionHandler
    = HOnly String
    | HN ExpressionHandler
    | HOp ExpressionHandler String ExpressionHandler


prop : Parser String
prop =
    succeed ()
        |. chompIf Char.isAlphaNum
        |. chompWhile Char.isAlphaNum
        |> getChompedString


operators : Parser String
operators =
    oneOf
        [ succeed ()
            |. symbol "&"
            |> getChompedString
        , succeed ()
            |. symbol "|"
            |> getChompedString
        , succeed ()
            |. symbol ">"
            |> getChompedString
        , succeed ()
            |. symbol "%"
            |> getChompedString
        ]


operation : Parser ExpressionHandler
operation =
    succeed HOp
        |= term
        |. spaces
        |= operators
        |. spaces
        |= term


term : Parser ExpressionHandler
term =
    oneOf
        [ succeed HN
            |. symbol "~"
            |. spaces
            |= lazy (\_ -> term)
        , succeed HOnly
            |= prop
        , succeed identity
            |. symbol "("
            |. spaces
            |= lazy (\_ -> operation)
            |. spaces
            |. symbol ")"
        ]


toExpression : ExpressionHandler -> Expression
toExpression op =
    case op of
        HOnly name ->
            Only name

        HN br ->
            Not (toExpression br)

        HOp br1 "&" br2 ->
            And (toExpression br1) (toExpression br2)

        HOp br1 "|" br2 ->
            Or (toExpression br1) (toExpression br2)

        HOp br1 ">" br2 ->
            Cond (toExpression br1) (toExpression br2)

        HOp br1 "%" br2 ->
            Bcon (toExpression br1) (toExpression br2)

        _ ->
            Only ""


read : String -> Maybe Expression
read expr =
    run (map toExpression term) expr
        |> Result.toMaybe
