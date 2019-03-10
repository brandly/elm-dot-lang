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
    , dot
    , fromString
    )

import DoubleQuoteString as DQS
import Html.Parser exposing (Node(..), node)
import Parser exposing (..)
import Set


fromString : String -> Result (List Parser.DeadEnd) Dot
fromString =
    Parser.run dot


type Dot
    = Dot EdgeType (Maybe ID) (List Stmt)


dot : Parser Dot
dot =
    succeed Dot
        |. spacing
        |= edgeType
        |. spacing
        |= maybeParse id
        |. spacing
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
    | EdgeStmtNode NodeId EdgeRHS (List EdgeRHS) (List Attr)
    | EdgeStmtSubgraph Subgraph EdgeRHS (List EdgeRHS) (List Attr)
    | AttrStmt AttrStmtType (List Attr)
      -- probably a better name but i don't understand what it does
    | LooseAttr Attr
    | SubgraphStmt Subgraph


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
                    |. spacing
                    |. oneOf
                        [ symbol ";"
                        , succeed ()
                        ]
                    |. spacing
                , succeed ()
                    |> map (\_ -> Done (List.reverse revStmts))
                ]
    in
    succeed identity
        |. symbol "{"
        |. spacing
        |= loop [] help
        |. spacing
        |. symbol "}"


statement : Parser Stmt
statement =
    oneOf
        [ attrStmt
        , (succeed identity
            |= subgraph
            |. spacing
          )
            |> andThen
                (\sg ->
                    oneOf
                        [ succeed (EdgeStmtSubgraph sg)
                            |= edgeRHS
                            |. spacing
                            |= repeatingRhs
                            |. spacing
                            |= parseWithDefault attrList []
                        , succeed (SubgraphStmt sg)
                        ]
                )
        , (succeed Tuple.pair
            |= id
            |. spacing
            |= maybeParse port_
            |. spacing
          )
            |> andThen
                (\( id_, maybePort ) ->
                    let
                        loose =
                            map LooseAttr <|
                                succeed (Attr id_)
                                    |. symbol "="
                                    |. spacing
                                    |= id

                        edge =
                            succeed (EdgeStmtNode (NodeId id_ maybePort))
                                |= edgeRHS
                                |. spacing
                                |= repeatingRhs
                                |. spacing
                                |= parseWithDefault attrList []

                        node =
                            succeed (NodeStmt (NodeId id_ maybePort))
                                |= parseWithDefault attrList []
                    in
                    if maybePort == Nothing then
                        oneOf
                            [ loose
                            , edge
                            , node
                            ]

                    else
                        oneOf
                            [ edge
                            , node
                            ]
                )
        ]


nodeStmt : Parser Stmt
nodeStmt =
    succeed NodeStmt
        |= nodeId
        |. spacing
        |= parseWithDefault attrList []


edgeStmtNode : Parser Stmt
edgeStmtNode =
    succeed EdgeStmtNode
        |= nodeId
        |. spacing
        |= edgeRHS
        |. spacing
        |= repeatingRhs
        |. spacing
        |= parseWithDefault attrList []


type EdgeRHS
    = EdgeNode EdgeType NodeId
    | EdgeSubgraph EdgeType Subgraph


edgeRHS : Parser EdgeRHS
edgeRHS =
    succeed identity
        |= edgeOp
        |. spacing
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
                    |. spacing
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
        |. spacing
        |= attrList


type Attr
    = Attr ID ID


attrList : Parser (List Attr)
attrList =
    let
        help : List Attr -> Parser (Step (List Attr) (List Attr))
        help revStmts =
            oneOf
                [ succeed (\stmt -> Loop (stmt :: revStmts))
                    |= attr
                    |. spacing
                    |. oneOf
                        [ symbol ";"
                        , symbol ","
                        , succeed ()
                        ]
                    |. spacing
                , succeed ()
                    |> map (\_ -> Done (List.reverse revStmts))
                ]
    in
    map (\( a, b ) -> List.concat [ a, b ]) <|
        succeed Tuple.pair
            |. symbol "["
            |. spacing
            |= loop [] help
            |. spacing
            |. symbol "]"
            |= lazy (\_ -> parseWithDefault attrList [])


attr : Parser Attr
attr =
    succeed Attr
        |= id
        |. spacing
        |. symbol "="
        |. spacing
        |= id


subgraph : Parser Subgraph
subgraph =
    oneOf
        [ succeed Subgraph
            |. symbol "subgraph"
            |. spacing
            |= maybeParse id
            |. spacing
            |= stmtList
        , succeed (Subgraph Nothing)
            |. spacing
            |= stmtList
        ]


type NodeId
    = NodeId ID (Maybe Port)


nodeId : Parser NodeId
nodeId =
    succeed NodeId
        |= id
        |. spacing
        |= maybeParse port_


type ID
    = ID String
    | HtmlID Node
    | NumeralID Float


id : Parser ID
id =
    oneOf
        [ map ID <|
            oneOf
                [ DQS.string
                , variable
                    { start = \c -> Char.isAlpha c || c == '_'
                    , inner = \c -> Char.isAlphaNum c || c == '_'
                    , reserved = Set.fromList []
                    }
                ]
        , map NumeralID float
        , succeed HtmlID
            |. symbol "<"
            |= node
            |. symbol ">"
        ]


type Port
    = PortId ID (Maybe CompassPt)
    | PortPt CompassPt


port_ : Parser Port
port_ =
    succeed identity
        |. symbol ":"
        |. spacing
        |= oneOf
            [ succeed PortPt
                |= compassPt
            , succeed PortId
                |= id
                |. spacing
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


comment : Parser ()
comment =
    oneOf
        [ lineComment "//"
        , multiComment "/*" "*/" NotNestable
        , lineComment "#"
        ]


spacing : Parser ()
spacing =
    let
        isSpace =
            \c -> c == ' ' || c == '\n' || c == '\u{000D}'
    in
    succeed ()
        |. (repeat <|
                oneOf
                    [ succeed ()
                        |. chompIf isSpace
                        |. chompWhile isSpace
                    , comment
                    ]
           )


repeat : Parser a -> Parser (List a)
repeat parser =
    let
        help : List a -> Parser (Step (List a) (List a))
        help revStmts =
            oneOf
                [ succeed (\stmt -> Loop (stmt :: revStmts))
                    |= parser
                , succeed ()
                    |> map (\_ -> Done (List.reverse revStmts))
                ]
    in
    loop [] help


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
        , succeed default
        ]


maybeParse : Parser a -> Parser (Maybe a)
maybeParse parser =
    oneOf
        [ map Just parser
        , succeed Nothing
        ]
