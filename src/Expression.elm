module Expression exposing (Expression(..))


type Expression
    = Only String
    | Not Expression
    | And Expression Expression
    | Or Expression Expression
    | Cond Expression Expression
    | Bcon Expression Expression
