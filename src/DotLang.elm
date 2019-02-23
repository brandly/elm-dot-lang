module DotLang exposing (Directed(..), Dot(..), NodeId(..), Stmt(..), block, parse, statement)

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


type
    Stmt
    -- TODO: this second one is really `List NodeId`
    = EdgeStmt NodeId NodeId


type NodeId
    = NodeId String


parse : String -> Result (List Parser.DeadEnd) Dot
parse =
    Parser.run dot


dot : Parser Dot
dot =
    succeed Dot
        |= directed
        |. spaces
        |= block


statement : Parser Stmt
statement =
    succeed (\a b -> EdgeStmt a b)
        |= id
        |. spaces
        |. edgeOp
        |. spaces
        |= id


edgeOp : Parser ()
edgeOp =
    oneOf
        [ symbol "--"
        , symbol "->"
        ]


id : Parser NodeId
id =
    map NodeId
        (getChompedString <|
            succeed identity
                -- TODO: this should be a whitelist
                |. chompWhile (\c -> c /= ';' && c /= '}' && c /= '-' && c /= '\n' && c /= ' ')
        )


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
