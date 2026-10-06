module FuzzTests exposing (suite)

import DotLang exposing (..)
import Expect exposing (Expectation)
import Fuzz exposing (..)
import Html.Parser exposing (Attribute, Node(..))
import Parser
import Test exposing (..)


suite : Test
suite =
    describe "fuzzy tests"
        [ fuzz fuzzDot "can parse toString output" <|
            \theDot ->
                Expect.equal (Ok theDot) (fromString (toString theDot))
        , fuzz niceFloat "writes numerals using only DOT numeral characters" <|
            \f ->
                toStringWithConfig OneLine (Dot Graph Nothing [ NodeStmt (NodeId (NumeralID f) Nothing) [] ])
                    |> String.dropLeft (String.length "graph { ")
                    |> String.dropRight (String.length " }")
                    |> String.all (\c -> Char.isDigit c || c == '-' || c == '.')
                    |> Expect.equal True
        ]


fuzzDot : Fuzzer Dot
fuzzDot =
    map3 Dot fuzzEdgeType (maybe fuzzId) (fuzzStmts 1)


fuzzEdgeType : Fuzzer EdgeType
fuzzEdgeType =
    oneOf
        [ constant Graph
        , constant Digraph
        ]


fuzzId : Fuzzer ID
fuzzId =
    oneOf
        [ map ID string
        , map NumeralID niceFloat

        -- TODO: HtmlID
        ]


fuzzStmts : Int -> Fuzzer (List Stmt)
fuzzStmts depth =
    shortList
        (oneOf
            [ map2 NodeStmt
                fuzzNodeId
                (list fuzzAttr)
            , map4 EdgeStmtNode
                fuzzNodeId
                (fuzzEdgeRHS depth)
                (shortList (fuzzEdgeRHS depth))
                (list fuzzAttr)
            , map4 EdgeStmtSubgraph
                (fuzzSubgraph depth)
                (fuzzEdgeRHS depth)
                (shortList (fuzzEdgeRHS depth))
                (list fuzzAttr)
            , map2 AttrStmt
                fuzzAttrStmtType
                (list fuzzAttr)
            , map LooseAttr fuzzAttr
            , map SubgraphStmt (fuzzSubgraph depth)
            ]
        )


shortList : Fuzzer a -> Fuzzer (List a)
shortList fuzzer =
    map3 (\a b c -> [ a, b, c ]) fuzzer fuzzer fuzzer


fuzzNodeId : Fuzzer NodeId
fuzzNodeId =
    map2 NodeId fuzzId (maybe fuzzPort)


fuzzPort : Fuzzer Port
fuzzPort =
    oneOf
        [ map2 PortId fuzzId (maybe fuzzCompassPt)
        , map PortPt fuzzCompassPt
        ]


fuzzCompassPt : Fuzzer CompassPt
fuzzCompassPt =
    oneOf
        [ constant N
        , constant NE
        , constant E
        , constant SE
        , constant S
        , constant SW
        , constant W
        , constant NW
        , constant C
        , constant UND
        ]


fuzzAttr : Fuzzer Attr
fuzzAttr =
    map2 Attr fuzzId fuzzId


fuzzAttrStmtType : Fuzzer AttrStmtType
fuzzAttrStmtType =
    oneOf
        [ constant AttrGraph
        , constant AttrNode
        , constant AttrEdge
        ]


fuzzEdgeRHS : Int -> Fuzzer EdgeRHS
fuzzEdgeRHS depth =
    oneOf
        [ map EdgeNode fuzzNodeId
        , map EdgeSubgraph (fuzzSubgraph depth)
        ]


fuzzSubgraph : Int -> Fuzzer Subgraph
fuzzSubgraph depth =
    let
        stm =
            if depth <= 0 then
                constant []

            else
                fuzzStmts (depth - 1)
    in
    map2 Subgraph (maybe fuzzId) stm
