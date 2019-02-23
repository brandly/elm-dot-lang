module DotLang exposing (Directed(..), Dot(..), block, parse, statement)

import Parser
    exposing
        ( (|.)
        , (|=)
        , Parser
        , Step(..)
        , Trailing(..)
        , chompIf
        , chompWhile
        , getChompedString
        , loop
        , map
        , oneOf
        , sequence
        , spaces
        , succeed
        , symbol
        )


type Dot
    = Dot Directed (List Stmt)


type Directed
    = Graph
    | Digraph


parse : String -> Result (List Parser.DeadEnd) Dot
parse =
    Parser.run dot


dot : Parser Dot
dot =
    succeed Dot
        |= directed
        |. spaces
        |= block


type alias Stmt =
    String


statement : Parser Stmt
statement =
    let
        legal c =
            c /= ';' && c /= '}'
    in
    getChompedString <|
        succeed identity
            |. chompIf legal
            |. chompWhile legal


block : Parser (List Stmt)
block =
    sequence
        { start = "{"
        , separator = ";"
        , end = "}"
        , spaces = spaces
        , item = statement
        , trailing = Mandatory
        }


directed : Parser Directed
directed =
    oneOf
        [ map (\_ -> Graph) (symbol "graph")
        , map (\_ -> Digraph) (symbol "digraph")
        ]
