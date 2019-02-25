module DotLang exposing
    ( Attr(..)
    , AttrStmtType(..)
    , Dot(..)
    , EdgeRHS(..)
    , EdgeType(..)
    , ID(..)
    , NodeId(..)
    , Stmt(..)
    , Subgraph(..)
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
    symbolToType
        [ ( "graph", Graph )
        , ( "digraph", Digraph )
        ]


type Stmt
    = NodeStmt NodeId (List Attr)
    | EdgeStmt NodeId EdgeRHS (List EdgeRHS) (List Attr)
    | AttrStmt AttrStmtType (List Attr)
      -- probably a better name but i don't understand what it does
    | LooseAttr Attr
    | SubgraphStmt Subgraph
    | Comment String


type Subgraph
    = Subgraph (Maybe ID) (List Stmt)


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
        [ comment
        , attrStmt
        , map SubgraphStmt subgraph

        -- TODO: can we refactor to use these original definitions?
        --, edgeStmt
        --, nodeStmt
        --, map LooseAttr attr
        , andThen
            (\( id_, maybePort ) ->
                oneOf
                    [ -- TODO: we should only consider LooseAttr if maybePort == Nothing
                      map LooseAttr <|
                        succeed (Attr id_)
                            |. symbol "="
                            |. spaces
                            |= id
                    , succeed (EdgeStmt (NodeId id_ maybePort))
                        |= edgeRHS
                        |. spaces
                        |= repeatingRhs
                        |. spaces
                        |= parseWithDefault attrList []
                    , succeed (NodeStmt (NodeId id_ maybePort))
                        |= parseWithDefault attrList []
                    ]
            )
            (succeed Tuple.pair
                |= id
                |. spaces
                |= maybeParse port_
                |. spaces
            )
        ]


nodeStmt : Parser Stmt
nodeStmt =
    succeed NodeStmt
        |= nodeId
        |. spaces
        |= parseWithDefault attrList []


edgeStmt : Parser Stmt
edgeStmt =
    -- TODO: edgeStmt can start with a subgraph
    succeed EdgeStmt
        |= nodeId
        |. spaces
        |= edgeRHS
        |. spaces
        |= repeatingRhs
        |. spaces
        |= parseWithDefault attrList []


type EdgeRHS
    = EdgeNode EdgeType NodeId
    | EdgeSubgraph EdgeType Subgraph


edgeRHS : Parser EdgeRHS
edgeRHS =
    succeed identity
        |= edgeOp
        |. spaces
        |> andThen
            (\edge ->
                oneOf
                    [ succeed (EdgeSubgraph edge)
                        |= subgraph
                    , succeed (EdgeNode edge)
                        |= nodeId
                    ]
            )


edgeOp : Parser EdgeType
edgeOp =
    symbolToType
        [ ( "--", Graph )
        , ( "->", Digraph )
        ]


repeatingRhs : Parser (List EdgeRHS)
repeatingRhs =
    let
        help : List EdgeRHS -> Parser (Step (List EdgeRHS) (List EdgeRHS))
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
    symbolToType
        [ ( "graph", AttrGraph )
        , ( "node", AttrNode )
        , ( "edge", AttrEdge )
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


subgraph : Parser Subgraph
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
    succeed NodeId
        |= id
        |. spaces
        |= maybeParse port_


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


type Port
    = PortId ID (Maybe CompassPt)
    | PortPt CompassPt


port_ : Parser Port
port_ =
    oneOf
        [ succeed PortPt
            |. symbol ":"
            |. spaces
            |= compassPt
        , succeed PortId
            |. symbol ":"
            |. spaces
            |= id
            |. spaces
            |= maybeParse compassPt
        ]


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


compassPt : Parser CompassPt
compassPt =
    symbolToType
        [ ( "n", N )
        , ( "ne", NE )
        , ( "e", E )
        , ( "se", SE )
        , ( "s", S )
        , ( "sw", SW )
        , ( "w", W )
        , ( "nw", NW )
        , ( "c", C )
        , ( "_", UND )
        ]


comment : Parser Stmt
comment =
    -- TODO: a line beginning with a '#' character is considered a line output
    -- from a C preprocessor (e.g., # 34 to indicate line 34 ) and discarded.
    map Comment <|
        getChompedString <|
            oneOf
                [ lineComment "//"
                , multiComment "/*" "*/" NotNestable
                ]


symbolToType : List ( String, a ) -> Parser a
symbolToType list =
    oneOf <|
        List.map
            (\( str, t ) -> map (\_ -> t) (symbol str))
            list


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
