module DotLang exposing
    ( Dot(..)
    , EdgeType(..)
    , ID(..)
    , NodeId(..)
    , Stmt(..)
    , block
    , parse
    , statement
    )

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


parse : String -> Result (List Parser.DeadEnd) Dot
parse =
    Parser.run dot


type Dot
    = Dot EdgeType (List Stmt)


dot : Parser Dot
dot =
    succeed Dot
        |= edgeType
        |. spaces
        |= block


type EdgeType
    = Graph
    | Digraph


edgeType : Parser EdgeType
edgeType =
    oneOf
        [ map (\_ -> Graph) (symbol "graph")
        , map (\_ -> Digraph) (symbol "digraph")
        ]


type
    Stmt
    --TODO: (List (EdgeType, NodeId))
    = EdgeStmt NodeId ( EdgeType, NodeId )


block : Parser (List Stmt)
block =
    sequence
        { start = "{"
        , separator = ";"
        , end = "}"
        , spaces = spaces
        , item = statement
        , trailing = Optional
        }


statement : Parser Stmt
statement =
    oneOf
        [ edgeStmt ]


edgeStmt : Parser Stmt
edgeStmt =
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


type NodeId
    = NodeId ID (Maybe Port)


nodeId : Parser NodeId
nodeId =
    map (\i -> NodeId i Nothing) id


type ID
    = ID String


id : Parser ID
id =
    map ID <|
        -- TODO: quoted strings, HTML
        variable
            { start = \c -> Char.isAlphaNum c || c == '_'
            , inner = \c -> Char.isAlphaNum c || c == '_' || Char.isDigit c
            , reserved = Set.fromList []
            }


type
    Port
    -- TODO: parse these
    = Port ID (Maybe CompassPt)


type CompassPt
    = N
    | NE
    | E
    | SE
    | S
    | SW
    | W
    | NW
    | C
    | UND
