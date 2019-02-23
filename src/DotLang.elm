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
        , variable
        )
import Set


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
        |= nodeId
        |. spaces
        |. edgeOp
        |. spaces
        |= nodeId


edgeOp : Parser ()
edgeOp =
    oneOf
        [ symbol "--"
        , symbol "->"
        ]


nodeId : Parser NodeId
nodeId =
    map NodeId <|
        -- TODO: quoted strings, HTML
        variable
            { start = \c -> Char.isAlphaNum c || c == '_'
            , inner = \c -> Char.isAlphaNum c || c == '_' || Char.isDigit c
            , reserved = Set.fromList []
            }


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
