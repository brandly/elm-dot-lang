module DotLang exposing
    ( fromString, Dot(..)
    , EdgeType(..), ID(..), Stmt(..)
    , NodeId(..), Attr(..), AttrStmtType(..), EdgeRHS(..), Subgraph(..), Port(..), CompassPt(..)
    , toString, Config(..), toStringWithConfig
    , dot
    )

{-| Parse DOT Language in Elm.
Take a look at the grammar <https://www.graphviz.org/doc/info/lang.html>

@docs fromString, Dot


# DOT Components

@docs EdgeType, ID, Stmt


# Stmt Components

@docs NodeId, Attr, AttrStmtType, EdgeRHS, Subgraph, Port, CompassPt


# toString

@docs toString, Config, toStringWithConfig


# Internal

@docs dot

-}

import DoubleQuoteString as DQS
import Html.Parser exposing (Node(..), node, nodeToString)
import Parser exposing (..)
import Set


{-| Parse a DOT string.

    fromString "graph {}" == Ok (Dot Graph Nothing [])

-}
fromString : String -> Result (List Parser.DeadEnd) Dot
fromString =
    Parser.run dot


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


{-| This is the core of a graph's definition. DOT holds a list of statements
describing the vertices and edges, along with their properties.
-}
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


{-| The right-hand side of an edge describes what the left-hand side is
connected to. In DOT, you can string together many right-hand sides to describe
large graph structures in a single `Stmt`.
-}
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
            (\_ ->
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


{-| An `AttrStmt` might apply to all nodes or edges, or even the graph as a
whole. The `AttrStmtType` indicates which is being described.
-}
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


{-| An `Attr` is a key/value pair describing a property of the graph.
-}
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


{-| A `Subgraph` defines a subset of vertices and edges within a graph. You
might use this for to visually group a set of vertices together or just as a
shorthand for defining edges between one vertex and a list of other vertices.
-}
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


{-| `NodeId` describes the `ID` of a vertex. Potentially, it has a `Port` which
describes where edges can attach to the vertex.
-}
type NodeId
    = NodeId ID (Maybe Port)


nodeId : Parser NodeId
nodeId =
    succeed NodeId
        |= id
        |. spacing
        |= maybeParse port_


{-| The identifier for a vertex.
-}
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


{-| A `Port` is a point where edges can attach to a vertex. The `Port` can have
an `ID`, but they're primarily described by the 8 compass directions.
-}
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


{-| A `CompassPt` describes the 8 compass directions, as well as `C` for
"center" and `UND` for the default, unspecified direction.
-}
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


filterEmpty : List String -> List String
filterEmpty =
    List.filter (String.length >> (<) 0)


{-| Configure `toStringWithConfig`, either exporting a graph onto `OneLine`,
if you don't care about readability, or with some number of spaces per
`Indent`.
-}
type Config
    = OneLine
    | Indent Int


{-| Export `Dot` into valid DOT Language syntax, using four spaces for
indentation.

    toString (Dot Graph Nothing []) == "graph {}"

-}
toString : Dot -> String
toString =
    toStringWithConfig (Indent 4)


{-| Export `Dot` into valid DOT Language syntax with a given `Config`.
-}
toStringWithConfig : Config -> Dot -> String
toStringWithConfig config (Dot type_ maybeId stmts) =
    let
        separator : Int -> String
        separator depth =
            case config of
                OneLine ->
                    " "

                Indent count ->
                    let
                        indent : String
                        indent =
                            String.repeat count " "
                    in
                    String.cons '\n' (String.repeat depth indent)

        id_ : String
        id_ =
            maybeId
                |> Maybe.map (showId >> (\str -> str ++ " "))
                |> Maybe.withDefault ""

        edgeStr : String
        edgeStr =
            case type_ of
                Graph ->
                    "--"

                Digraph ->
                    "->"

        showRHS : Int -> EdgeRHS -> String
        showRHS depth rhs =
            String.join " "
                [ edgeStr
                , case rhs of
                    EdgeNode nodeId_ ->
                        showNodeId nodeId_

                    EdgeSubgraph subgraph_ ->
                        showSubgraph depth subgraph_
                ]

        showSubgraph : Int -> Subgraph -> String
        showSubgraph depth (Subgraph maybeId_ stmts_) =
            (filterEmpty >> String.join " ")
                [ "subgraph"
                , maybeId_
                    |> Maybe.map showId
                    |> Maybe.withDefault ""
                , showStmts (depth + 1) stmts_
                ]

        showStmts : Int -> List Stmt -> String
        showStmts depth stmts_ =
            if List.isEmpty stmts_ then
                "{}"

            else
                String.join (separator depth)
                    ("{" :: List.map (showStmt depth) stmts_)
                    ++ (separator (depth - 1) ++ "}")

        showStmt : Int -> Stmt -> String
        showStmt depth stmt =
            case stmt of
                NodeStmt nodeId_ attrs ->
                    (filterEmpty >> String.join "")
                        [ showNodeId nodeId_
                        , showAttrs "" attrs
                        ]

                EdgeStmtNode nodeId_ rhs moreRhs attrs ->
                    (filterEmpty >> String.join " ")
                        [ showNodeId nodeId_
                        , showRHS depth rhs
                        , String.join "" (List.map (showRHS depth) moreRhs)
                        ]
                        ++ showAttrs "" attrs

                EdgeStmtSubgraph subgraph_ rhs moreRhs attrs ->
                    (filterEmpty >> String.join " ")
                        [ showSubgraph depth subgraph_
                        , showRHS depth rhs
                        , String.join "" (List.map (showRHS depth) moreRhs)
                        , showAttrs "" attrs
                        ]

                AttrStmt type__ attrs ->
                    (filterEmpty >> String.join " ")
                        [ showAttrStmtType type__, showAttrs "[]" attrs ]

                LooseAttr attr_ ->
                    showAttr attr_

                SubgraphStmt subgraph_ ->
                    showSubgraph depth subgraph_
    in
    showType type_ ++ " " ++ id_ ++ showStmts 1 stmts


showId : ID -> String
showId id_ =
    case id_ of
        ID str ->
            let
                escaped =
                    String.replace "\"" "\\\"" str
            in
            if String.contains " " str || String.contains "\"" str then
                "\"" ++ escaped ++ "\""

            else
                str

        HtmlID node ->
            String.join "" [ "<", nodeToString node, ">" ]

        NumeralID float ->
            String.fromFloat float


showNodeId : NodeId -> String
showNodeId (NodeId id_ maybePort) =
    showId id_
        ++ (maybePort
                |> Maybe.map (showPort >> String.cons ':')
                |> Maybe.withDefault ""
           )


showType : EdgeType -> String
showType type_ =
    case type_ of
        Graph ->
            "graph"

        Digraph ->
            "digraph"


showAttrs : String -> List Attr -> String
showAttrs default attrs =
    if List.isEmpty attrs then
        default

    else
        "[" ++ String.join "," (List.map showAttr attrs) ++ "]"


showAttr : Attr -> String
showAttr (Attr a b) =
    String.join "=" [ showId a, showId b ]


showAttrStmtType : AttrStmtType -> String
showAttrStmtType type_ =
    case type_ of
        AttrGraph ->
            "graph"

        AttrNode ->
            "node"

        AttrEdge ->
            "edge"


showPort : Port -> String
showPort port__ =
    case port__ of
        PortId id__ maybeCompassPt ->
            showId id__
                ++ (maybeCompassPt
                        |> Maybe.map (showCompassPt >> String.cons ':')
                        |> Maybe.withDefault ""
                   )

        PortPt compassPt_ ->
            showCompassPt compassPt_


showCompassPt : CompassPt -> String
showCompassPt compassPt_ =
    case compassPt_ of
        N ->
            "n"

        NE ->
            "ne"

        E ->
            "e"

        SE ->
            "se"

        S ->
            "s"

        SW ->
            "sw"

        W ->
            "w"

        NW ->
            "nw"

        C ->
            "c"

        UND ->
            "_"
