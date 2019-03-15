module DotLang exposing
    ( fromString, Dot(..)
    , EdgeType(..), ID(..), Stmt(..)
    , NodeId(..), Attr(..), AttrStmtType(..), EdgeRHS(..), Subgraph(..)
    , dot
    , toString
    )

{-| Parse DOT Language in Elm.
Take a look at the grammar <https://www.graphviz.org/doc/info/lang.html>

@docs fromString, Dot


# DOT Components

@docs EdgeType, ID, Stmt


# Stmt Components

@docs NodeId, Attr, AttrStmtType, EdgeRHS, EdgeType, Subgraph


# Internal

@docs dot

-}

import DoubleQuoteString as DQS
import Html.Parser exposing (Node(..), node, nodeToString)
import Parser exposing (..)
import Set


{-| Parse a DOT string.

    fromString "graph {}" == Dot Graph Nothing []

-}
fromString : String -> Result (List Parser.DeadEnd) Dot
fromString =
    Parser.run dot


toString : Dot -> String
toString (Dot type_ maybeId stmts) =
    let
        showId : ID -> String
        showId id__ =
            case id__ of
                ID str ->
                    str

                HtmlID node ->
                    nodeToString node

                NumeralID float ->
                    String.fromFloat float

        showType : EdgeType -> String
        showType type__ =
            case type__ of
                Graph ->
                    "graph"

                Digraph ->
                    "digraph"

        showStmt : Stmt -> String
        showStmt stmt =
            Debug.todo "showStmt"

        id_ =
            maybeId
                |> Maybe.map (showId >> (\str -> " " ++ str))
                |> Maybe.withDefault ""
    in
    showType type_ ++ id_ ++ " {" ++ String.join "\n" (List.map showStmt stmts) ++ "}"


{-| A DOT file. Either a `graph` or `digraph` is represented. It might have
an `ID`. `Stmt`s describe the graph's properties, including vertices and edges.
-}
type Dot
    = Dot EdgeType (Maybe ID) (List Stmt)


{-| The core `Parser`, in case you want to embed it in some other parser.
-}
dot : Parser Dot
dot =
    (succeed identity
        |. spacing
        |= edgeType
    )
        |> andThen
            (\type_ ->
                succeed (Dot type_)
                    |. spacing
                    |= maybeParse id
                    |. spacing
                    |= stmtList type_
            )


{-| A DOT file representing an undirected graph starts with `graph` and edges
are described with `--`. A directed graph starts with `digraph` and uses `->`
for its edges.
-}
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


stmtList : EdgeType -> Parser (List Stmt)
stmtList type_ =
    let
        help : List Stmt -> Parser (Step (List Stmt) (List Stmt))
        help revStmts =
            oneOf
                [ succeed (\stmt -> Loop (stmt :: revStmts))
                    |= statement type_
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


statement : EdgeType -> Parser Stmt
statement type_ =
    oneOf
        [ attrStmt
        , (succeed identity
            |= subgraph type_
            |. spacing
          )
            |> andThen
                (\sg ->
                    oneOf
                        [ succeed (EdgeStmtSubgraph sg)
                            |= edgeRHS type_
                            |. spacing
                            |= repeatingRhs type_
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
                                |= edgeRHS type_
                                |. spacing
                                |= repeatingRhs type_
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


type EdgeRHS
    = EdgeNode NodeId
    | EdgeSubgraph Subgraph


edgeRHS : EdgeType -> Parser EdgeRHS
edgeRHS type_ =
    succeed identity
        |. (edgeOp
                |> andThen
                    (\op ->
                        if op == type_ then
                            succeed op

                        else
                            problem <|
                                String.join ""
                                    [ "Expected a "
                                    , showEdgeType type_
                                    , ", but this edge is for a "
                                    , showEdgeType op
                                    , "."
                                    ]
                    )
           )
        |. spacing
        |> andThen
            (\edge ->
                oneOf
                    [ succeed EdgeSubgraph
                        |= subgraph type_
                    , succeed EdgeNode
                        |= nodeId
                    ]
            )


edgeOp : Parser EdgeType
edgeOp =
    symbolToType
        [ ( "--", Graph )
        , ( "->", Digraph )
        ]


showEdgeType : EdgeType -> String
showEdgeType type_ =
    case type_ of
        Graph ->
            "graph"

        Digraph ->
            "digraph"


repeatingRhs : EdgeType -> Parser (List EdgeRHS)
repeatingRhs type_ =
    let
        help : List EdgeRHS -> Parser (Step (List EdgeRHS) (List EdgeRHS))
        help revStmts =
            oneOf
                [ succeed (\stmt -> Loop (stmt :: revStmts))
                    |= edgeRHS type_
                    |. spacing
                , succeed ()
                    |> map (\_ -> Done (List.reverse revStmts))
                ]
    in
    loop [] help


attrStmt : Parser Stmt
attrStmt =
    succeed AttrStmt
        |= attrStmtType
        |. spacing
        |= attrList


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


type Attr
    = Attr ID ID


attr : Parser Attr
attr =
    succeed Attr
        |= id
        |. spacing
        |. symbol "="
        |. spacing
        |= id


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


type Subgraph
    = Subgraph (Maybe ID) (List Stmt)


subgraph : EdgeType -> Parser Subgraph
subgraph type_ =
    oneOf
        [ succeed Subgraph
            |. symbol "subgraph"
            |. spacing
            |= maybeParse id
            |. spacing
            |= stmtList type_
        , succeed (Subgraph Nothing)
            |. spacing
            |= stmtList type_
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
