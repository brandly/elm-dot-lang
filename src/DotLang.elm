module DotLang exposing
    ( Attr(..)
    , Dot(..)
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
    = EdgeStmt NodeId ( EdgeType, NodeId ) (List Attr)


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
    succeed (\a rhs attrs -> EdgeStmt a rhs attrs)
        |= nodeId
        |. spaces
        |= edgeRHS
        |. spaces
        |= parseWithDefault attrList []


edgeRHS : Parser ( EdgeType, NodeId )
edgeRHS =
    succeed Tuple.pair
        |= edgeOp
        |. spaces
        |= nodeId


type AttrStmtType
    = AttrGraph
    | AttrNode
    | AttrEdge


attrStmtType : Parser AttrStmtType
attrStmtType =
    oneOf
        [ map (\_ -> AttrGraph) (symbol "graph")
        , map (\_ -> AttrNode) (symbol "node")
        , map (\_ -> AttrEdge) (symbol "edge")
        ]


type AttrStmt
    = AttrStmt AttrStmtType (List Attr)


attrStmt : Parser AttrStmt
attrStmt =
    succeed AttrStmt
        |= attrStmtType
        |. spaces
        |= attrList


type Attr
    = Attr ID ID


parseWithDefault : Parser a -> a -> Parser a
parseWithDefault parser default =
    oneOf
        [ parser
        , map (\_ -> default) <|
            succeed ()
                |. chompWhile (\_ -> False)
        ]


attrList : Parser (List Attr)
attrList =
    -- TODO: trailing attr_list?
    -- attr_list : '[' [ a_list ] ']' [ attr_list ]
    sequence
        { start = "["

        -- TODO: comma OR semi
        , separator = ","
        , end = "]"
        , spaces = spaces
        , item = attr
        , trailing = Optional
        }


attr : Parser Attr
attr =
    succeed Attr
        |= id
        |. spaces
        |. symbol "="
        |. spaces
        |= id


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
