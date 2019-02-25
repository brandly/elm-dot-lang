module DotLang exposing
    ( Attr(..)
    , AttrStmtType(..)
    , Dot(..)
    , EdgeType(..)
    , ID(..)
    , NodeId(..)
    , Stmt(..)
    , parse
    , statement
    , stmtList
    )

import DoubleQuoteString as DQS
import Parser exposing (..)
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
        |= stmtList


type EdgeType
    = Graph
    | Digraph


edgeType : Parser EdgeType
edgeType =
    oneOf
        [ map (\_ -> Graph) (symbol "graph")
        , map (\_ -> Digraph) (symbol "digraph")
        ]


type Stmt
    = NodeStmt NodeId (List Attr)
    | EdgeStmt NodeId ( EdgeType, NodeId ) (List ( EdgeType, NodeId )) (List Attr)
    | AttrStmt AttrStmtType (List Attr)
      -- probably a better name but i don't understand what it does
    | LooseAttr Attr
    | Subgraph (Maybe ID) (List Stmt)


stmtList : Parser (List Stmt)
stmtList =
    let
        help : List Stmt -> Parser (Step (List Stmt) (List Stmt))
        help revStmts =
            oneOf
                [ succeed (\stmt -> Loop (stmt :: revStmts))
                    |= statement
                    |. spaces
                    |. oneOf
                        [ chompIf ((==) ';')
                        , symbol ""
                        ]
                    |. spaces
                , succeed ()
                    |> map (\_ -> Done (List.reverse revStmts))
                ]
    in
    succeed identity
        |. symbol "{"
        |. spaces
        |= loop [] help
        |. spaces
        |. symbol "}"


statement : Parser Stmt
statement =
    oneOf
        [ attrStmt
        , subgraph
        , edgeStmt

        -- TODO: can't parse `nodeStmt`/`LooseAttr` cause it'll commit to `edgeStmt` first
        , nodeStmt
        , map LooseAttr attr
        ]


nodeStmt : Parser Stmt
nodeStmt =
    succeed (\node attrs -> NodeStmt node attrs)
        |= nodeId
        |. spaces
        |= parseWithDefault attrList []


edgeStmt : Parser Stmt
edgeStmt =
    succeed (\a rhs attrs -> EdgeStmt a rhs attrs)
        |= nodeId
        |. spaces
        |= edgeRHS
        |. spaces
        |= repeatingRhs
        |. spaces
        |= parseWithDefault attrList []


edgeRHS : Parser ( EdgeType, NodeId )
edgeRHS =
    succeed Tuple.pair
        |= edgeOp
        |. spaces
        |= nodeId


edgeOp : Parser EdgeType
edgeOp =
    oneOf
        [ map (\_ -> Graph) (symbol "--")
        , map (\_ -> Digraph) (symbol "->")
        ]


repeatingRhs : Parser (List ( EdgeType, NodeId ))
repeatingRhs =
    let
        help : List ( EdgeType, NodeId ) -> Parser (Step (List ( EdgeType, NodeId )) (List ( EdgeType, NodeId )))
        help revStmts =
            oneOf
                [ succeed (\stmt -> Loop (stmt :: revStmts))
                    |= edgeRHS
                    |. spaces
                , succeed ()
                    |> map (\_ -> Done (List.reverse revStmts))
                ]
    in
    loop [] help


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


attrStmt : Parser Stmt
attrStmt =
    succeed AttrStmt
        |= attrStmtType
        |. spaces
        |= attrList


type Attr
    = Attr ID ID


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


subgraph : Parser Stmt
subgraph =
    oneOf
        [ succeed Subgraph
            |. symbol "subgraph"
            |. spaces
            |= maybeParse id
            |. spaces
            |= stmtList
        , succeed (Subgraph Nothing)
            |. spaces
            |= stmtList
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
        -- TODO: HTML
        oneOf
            [ DQS.string
            , variable
                { start = \c -> Char.isAlpha c || c == '_'
                , inner = \c -> Char.isAlphaNum c || c == '_'
                , reserved = Set.fromList []
                }
            , map String.fromFloat float
            ]


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



--


parseWithDefault : Parser a -> a -> Parser a
parseWithDefault parser default =
    oneOf
        [ parser
        , map (\_ -> default) <|
            succeed ()
                |. chompWhile (\_ -> False)
        ]


maybeParse : Parser a -> Parser (Maybe a)
maybeParse parser =
    oneOf
        [ map Just parser
        , map (\_ -> Nothing) <|
            succeed ()
                |. chompWhile (\_ -> False)
        ]
