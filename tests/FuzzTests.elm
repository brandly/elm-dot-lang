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
        ]


fuzzDot : Fuzzer Dot
fuzzDot =
    map3 Dot fuzzEdgeType (maybe fuzzId) fuzzStmts


fuzzEdgeType : Fuzzer EdgeType
fuzzEdgeType =
    oneOf
        [ constant Graph
        , constant Digraph
        ]


fuzzId : Fuzzer ID
fuzzId =
    --TODO : ID, HtmlID
    map NumeralID float


fuzzStmts : Fuzzer (List Stmt)
fuzzStmts =
    list
        (oneOf
            [ map2 NodeStmt fuzzNodeId (list fuzzAttr)

            -- TODO
            --    | EdgeStmtNode NodeId EdgeRHS (List EdgeRHS) (List Attr)
            --    | EdgeStmtSubgraph Subgraph EdgeRHS (List EdgeRHS) (List Attr)
            , map2 AttrStmt fuzzAttrStmtType (list fuzzAttr)
            , map LooseAttr fuzzAttr

            --    | SubgraphStmt Subgraph
            ]
        )


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
