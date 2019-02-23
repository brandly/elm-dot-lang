module DotLang exposing (Dot(..), EdgeType(..), NodeId(..), Stmt(..), block, parse, statement)

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
    = Dot EdgeType (List Stmt)


type EdgeType
    = Graph
    | Digraph


type
    Stmt
    --TODO: (List (EdgeType, NodeId))
    = EdgeStmt NodeId ( EdgeType, NodeId )


type NodeId
    = NodeId String


parse : String -> Result (List Parser.DeadEnd) Dot
parse =
    Parser.run dot


dot : Parser Dot
dot =
    succeed Dot
        |= edgeType
        |. spaces
        |= block


statement : Parser Stmt
statement =
    succeed (\a edge b -> EdgeStmt a ( edge, b ))
        |= nodeId
        |. spaces
        |= edgeOp
        |. spaces
        |= nodeId


edgeOp : Parser EdgeType
edgeOp =
    oneOf
        [ map (\_ -> Graph) (symbol "--")
        , map (\_ -> Digraph) (symbol "->")
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


edgeType : Parser EdgeType
edgeType =
    oneOf
        [ map (\_ -> Graph) (symbol "graph")
        , map (\_ -> Digraph) (symbol "digraph")
        ]
