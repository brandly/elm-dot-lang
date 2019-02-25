module Example exposing (suite)

import DotLang
    exposing
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
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Parser
import Test exposing (..)


simpleGraph : String
simpleGraph =
    "graph {\n    a -- b;\n    b -- c;\n    a -- c;\n    d -- c;\n    e -- c;\n    e -- a;\n}"


simpleDigraph : String
simpleDigraph =
    "digraph {\n    a -> b;\n    b -> c;\n    c -> d;\n    d -> a;\n}"


fullDigraph : String
fullDigraph =
    "digraph {\n    a -> b[label=\"0.2\",weight=\"0.2\"];\n    a -> c[label=\"0.4\",weight=\"0.4\"];\n    c -> b[label=\"0.6\",weight=\"0.6\"];\n    c -> e[label=\"0.6\",weight=\"0.6\"];\n    e -> e[label=\"0.1\",weight=\"0.1\"];\n    e -> b[label=\"0.7\",weight=\"0.7\"];\n}"


showingPath : String
showingPath =
    "graph {\n    a -- b[color=red,penwidth=3.0];\n    b -- c;\n    c -- d[color=red,penwidth=3.0];\n    d -- e;\n    e -- f;\n    a -- d;\n    b -- d[color=red,penwidth=3.0];\n    c -- f[color=red,penwidth=3.0];\n}"


showingPathShortHand : String
showingPathShortHand =
    "graph {\n    a -- b -- d -- c -- f[color=red,penwidth=3.0];\n    b -- c;\n    d -- e;\n    e -- f;\n    a -- d;\n}"


subgraph : String
subgraph =
    "digraph {\n    subgraph cluster_0 {\n        label=\"Subgraph A\";\n        a -> b;\n        b -> c;\n        c -> d;\n    }\n\n    subgraph cluster_1 {\n        label=\"Subgraph B\";\n        a -> f;\n        f -> c;\n    }\n}\n    "


largeGraph : String
largeGraph =
    "graph {\n    rankdir=LR; // Left to Right, instead of Top to Bottom\n    a -- { b c d };\n    b -- { c e };\n    c -- { e f };\n    d -- { f g };\n    e -- h;\n    f -- { h i j g };\n    g -- k;\n    h -- { o l };\n    i -- { l m j };\n    j -- { m n k };\n    k -- { n r };\n    l -- { o m };\n    m -- { o p n };\n    n -- { q r };\n    o -- { s p };\n    p -- { s t q };\n    q -- { t r };\n    r -- t;\n    s -- z;\n    t -- z;\n}"


suite : Test
suite =
    let
        edge : String -> EdgeType -> String -> Stmt
        edge a uh b =
            EdgeStmt (NodeId (ID a) Nothing) (EdgeNode uh (NodeId (ID b) Nothing)) [] []
    in
    describe "Dot Lang Parser"
        [ test "stmtList" <|
            \_ ->
                Expect.equal (Parser.run stmtList "{}") (Ok [])
        , test "statement" <|
            \_ ->
                Expect.equal (Parser.run statement "sup -- dude;dont -- care") (Ok (edge "sup" Graph "dude"))
        , test "statement with attrs" <|
            \_ ->
                Expect.equal (Parser.run statement "sup -- dude[dont=care]")
                    (Ok
                        (EdgeStmt (NodeId (ID "sup") Nothing)
                            (EdgeNode Graph (NodeId (ID "dude") Nothing))
                            []
                            [ Attr (ID "dont") (ID "care") ]
                        )
                    )
        , test "attr_stmt" <|
            \_ ->
                Expect.equal (Parser.run statement "graph [hm=yeah]")
                    (Ok
                        (AttrStmt AttrGraph [ Attr (ID "hm") (ID "yeah") ])
                    )
        , test "parsing simple graph" <|
            \_ ->
                Expect.equal (parse simpleGraph)
                    (Ok
                        (Dot Graph
                            [ edge "a" Graph "b"
                            , edge "b" Graph "c"
                            , edge "a" Graph "c"
                            , edge "d" Graph "c"
                            , edge "e" Graph "c"
                            , edge "e" Graph "a"
                            ]
                        )
                    )
        , test "don't need semicolons" <|
            \_ ->
                Expect.equal (parse (String.filter (\c -> c /= ';') simpleGraph))
                    (Ok
                        (Dot Graph
                            [ edge "a" Graph "b"
                            , edge "b" Graph "c"
                            , edge "a" Graph "c"
                            , edge "d" Graph "c"
                            , edge "e" Graph "c"
                            , edge "e" Graph "a"
                            ]
                        )
                    )
        , test "parsing simple digraph" <|
            \_ ->
                Expect.equal (parse simpleDigraph)
                    (Ok
                        (Dot Digraph
                            [ edge "a" Digraph "b"
                            , edge "b" Digraph "c"
                            , edge "c" Digraph "d"
                            , edge "d" Digraph "a"
                            ]
                        )
                    )
        , test "parsing full digraph" <|
            \_ ->
                Expect.equal (parse fullDigraph)
                    (Ok
                        (Dot Digraph
                            [ EdgeStmt (NodeId (ID "a") Nothing)
                                (EdgeNode Digraph (NodeId (ID "b") Nothing))
                                []
                                [ Attr (ID "label") (ID "0.2"), Attr (ID "weight") (ID "0.2") ]
                            , EdgeStmt (NodeId (ID "a") Nothing)
                                (EdgeNode Digraph (NodeId (ID "c") Nothing))
                                []
                                [ Attr (ID "label") (ID "0.4"), Attr (ID "weight") (ID "0.4") ]
                            , EdgeStmt (NodeId (ID "c") Nothing)
                                (EdgeNode Digraph (NodeId (ID "b") Nothing))
                                []
                                [ Attr (ID "label") (ID "0.6"), Attr (ID "weight") (ID "0.6") ]
                            , EdgeStmt (NodeId (ID "c") Nothing)
                                (EdgeNode Digraph (NodeId (ID "e") Nothing))
                                []
                                [ Attr (ID "label") (ID "0.6"), Attr (ID "weight") (ID "0.6") ]
                            , EdgeStmt (NodeId (ID "e") Nothing)
                                (EdgeNode Digraph (NodeId (ID "e") Nothing))
                                []
                                [ Attr (ID "label") (ID "0.1"), Attr (ID "weight") (ID "0.1") ]
                            , EdgeStmt (NodeId (ID "e") Nothing)
                                (EdgeNode Digraph (NodeId (ID "b") Nothing))
                                []
                                [ Attr (ID "label") (ID "0.7"), Attr (ID "weight") (ID "0.7") ]
                            ]
                        )
                    )
        , test "parsing 'showing a path'" <|
            \_ ->
                Expect.equal (parse showingPath)
                    (Ok
                        (Dot Graph
                            [ EdgeStmt (NodeId (ID "a") Nothing)
                                (EdgeNode Graph (NodeId (ID "b") Nothing))
                                []
                                [ Attr (ID "color") (ID "red"), Attr (ID "penwidth") (ID "3") ]
                            , edge "b" Graph "c"
                            , EdgeStmt (NodeId (ID "c") Nothing)
                                (EdgeNode Graph (NodeId (ID "d") Nothing))
                                []
                                [ Attr (ID "color") (ID "red"), Attr (ID "penwidth") (ID "3") ]
                            , edge "d" Graph "e"
                            , edge "e" Graph "f"
                            , edge "a" Graph "d"
                            , EdgeStmt (NodeId (ID "b") Nothing)
                                (EdgeNode Graph (NodeId (ID "d") Nothing))
                                []
                                [ Attr (ID "color") (ID "red"), Attr (ID "penwidth") (ID "3") ]
                            , EdgeStmt (NodeId (ID "c") Nothing)
                                (EdgeNode Graph (NodeId (ID "f") Nothing))
                                []
                                [ Attr (ID "color") (ID "red"), Attr (ID "penwidth") (ID "3") ]
                            ]
                        )
                    )
        , test "parsing 'showing a path' shorthand" <|
            \_ ->
                Expect.equal (parse showingPathShortHand)
                    (Ok
                        (Dot Graph
                            [ EdgeStmt (NodeId (ID "a") Nothing)
                                (EdgeNode Graph (NodeId (ID "b") Nothing))
                                [ EdgeNode Graph (NodeId (ID "d") Nothing)
                                , EdgeNode Graph (NodeId (ID "c") Nothing)
                                , EdgeNode Graph (NodeId (ID "f") Nothing)
                                ]
                                [ Attr (ID "color") (ID "red"), Attr (ID "penwidth") (ID "3") ]
                            , edge "b" Graph "c"
                            , edge "d" Graph "e"
                            , edge "e" Graph "f"
                            , edge "a" Graph "d"
                            ]
                        )
                    )
        , test "parsing subgraph" <|
            \_ ->
                Expect.equal (parse subgraph)
                    (Ok
                        (Dot Digraph
                            [ SubgraphStmt <|
                                Subgraph (Just (ID "cluster_0"))
                                    [ LooseAttr (Attr (ID "label") (ID "Subgraph A"))
                                    , edge "a" Digraph "b"
                                    , edge "b" Digraph "c"
                                    , edge "c" Digraph "d"
                                    ]
                            , SubgraphStmt <|
                                Subgraph (Just (ID "cluster_1"))
                                    [ LooseAttr (Attr (ID "label") (ID "Subgraph B"))
                                    , edge "a" Digraph "f"
                                    , edge "f" Digraph "c"
                                    ]
                            ]
                        )
                    )
        , test "subgraph edge" <|
            \_ ->
                Expect.equal
                    (parse
                        (String.join "\n"
                            [ "graph {"
                            , "    a -- { b c d };"
                            , "}"
                            ]
                        )
                    )
                    (Ok
                        (Dot Graph
                            [ EdgeStmt (NodeId (ID "a") Nothing)
                                (EdgeSubgraph Graph
                                    (Subgraph Nothing
                                        [ NodeStmt (NodeId (ID "b") Nothing) []
                                        , NodeStmt (NodeId (ID "c") Nothing) []
                                        , NodeStmt (NodeId (ID "d") Nothing) []
                                        ]
                                    )
                                )
                                []
                                []
                            ]
                        )
                    )
        , test "large graph" <|
            \_ ->
                Expect.equal
                    (parse
                        (String.join "\n"
                            [ "graph {"
                            , "    rankdir=LR; // Left to Right, instead of Top to Bottom"
                            , "    a -- { b c d };"
                            , "    b -- { c e };"
                            , "    c -- { e f };"
                            , "    d -- { f g };"
                            , "    e -- h;"
                            , "    f -- { h i j g };"
                            , "    g -- k;"
                            , "    h -- { o l };"
                            , "    i -- { l m j };"
                            , "    j -- { m n k };"
                            , "    k -- { n r };"
                            , "    l -- { o m };"
                            , "    m -- { o p n };"
                            , "    n -- { q r };"
                            , "    o -- { s p };"
                            , "    p -- { s t q };"
                            , "    q -- { t r };"
                            , "    r -- t;"
                            , "    s -- z;"
                            , "    t -- z;"
                            , "}"
                            ]
                        )
                    )
                    (Ok
                        (Dot Graph
                            [ LooseAttr (Attr (ID "rankdir") (ID "LR"))
                            , Comment "// Left to Right, instead of Top to Bottom"
                            , EdgeStmt (NodeId (ID "a") Nothing)
                                (EdgeSubgraph Graph
                                    (Subgraph Nothing
                                        [ NodeStmt (NodeId (ID "b") Nothing) []
                                        , NodeStmt (NodeId (ID "c") Nothing) []
                                        , NodeStmt (NodeId (ID "d") Nothing) []
                                        ]
                                    )
                                )
                                []
                                []
                            , EdgeStmt (NodeId (ID "b") Nothing)
                                (EdgeSubgraph Graph
                                    (Subgraph Nothing
                                        [ NodeStmt (NodeId (ID "c") Nothing) []
                                        , NodeStmt (NodeId (ID "e") Nothing) []
                                        ]
                                    )
                                )
                                []
                                []
                            , EdgeStmt (NodeId (ID "c") Nothing)
                                (EdgeSubgraph
                                    Graph
                                    (Subgraph Nothing
                                        [ NodeStmt (NodeId (ID "e") Nothing) []
                                        , NodeStmt (NodeId (ID "f") Nothing) []
                                        ]
                                    )
                                )
                                []
                                []
                            , EdgeStmt (NodeId (ID "d") Nothing)
                                (EdgeSubgraph Graph
                                    (Subgraph Nothing
                                        [ NodeStmt (NodeId (ID "f") Nothing) []
                                        , NodeStmt (NodeId (ID "g") Nothing) []
                                        ]
                                    )
                                )
                                []
                                []
                            , EdgeStmt (NodeId (ID "e") Nothing)
                                (EdgeNode Graph (NodeId (ID "h") Nothing))
                                []
                                []
                            , EdgeStmt (NodeId (ID "f") Nothing)
                                (EdgeSubgraph Graph
                                    (Subgraph Nothing
                                        [ NodeStmt (NodeId (ID "h") Nothing) []
                                        , NodeStmt (NodeId (ID "i") Nothing) []
                                        , NodeStmt (NodeId (ID "j") Nothing) []
                                        , NodeStmt (NodeId (ID "g") Nothing) []
                                        ]
                                    )
                                )
                                []
                                []
                            , EdgeStmt (NodeId (ID "g") Nothing)
                                (EdgeNode Graph (NodeId (ID "k") Nothing))
                                []
                                []
                            , EdgeStmt (NodeId (ID "h") Nothing)
                                (EdgeSubgraph Graph
                                    (Subgraph Nothing
                                        [ NodeStmt (NodeId (ID "o") Nothing) []
                                        , NodeStmt (NodeId (ID "l") Nothing) []
                                        ]
                                    )
                                )
                                []
                                []
                            , EdgeStmt (NodeId (ID "i") Nothing)
                                (EdgeSubgraph Graph
                                    (Subgraph Nothing
                                        [ NodeStmt (NodeId (ID "l") Nothing) []
                                        , NodeStmt (NodeId (ID "m") Nothing) []
                                        , NodeStmt (NodeId (ID "j") Nothing) []
                                        ]
                                    )
                                )
                                []
                                []
                            , EdgeStmt (NodeId (ID "j") Nothing)
                                (EdgeSubgraph Graph
                                    (Subgraph Nothing
                                        [ NodeStmt (NodeId (ID "m") Nothing) []
                                        , NodeStmt (NodeId (ID "n") Nothing) []
                                        , NodeStmt (NodeId (ID "k") Nothing) []
                                        ]
                                    )
                                )
                                []
                                []
                            , EdgeStmt (NodeId (ID "k") Nothing)
                                (EdgeSubgraph Graph
                                    (Subgraph Nothing
                                        [ NodeStmt (NodeId (ID "n") Nothing) []
                                        , NodeStmt (NodeId (ID "r") Nothing) []
                                        ]
                                    )
                                )
                                []
                                []
                            , EdgeStmt (NodeId (ID "l") Nothing)
                                (EdgeSubgraph Graph
                                    (Subgraph Nothing
                                        [ NodeStmt (NodeId (ID "o") Nothing) []
                                        , NodeStmt (NodeId (ID "m") Nothing) []
                                        ]
                                    )
                                )
                                []
                                []
                            , EdgeStmt (NodeId (ID "m") Nothing)
                                (EdgeSubgraph Graph
                                    (Subgraph Nothing
                                        [ NodeStmt (NodeId (ID "o") Nothing) []
                                        , NodeStmt (NodeId (ID "p") Nothing) []
                                        , NodeStmt (NodeId (ID "n") Nothing) []
                                        ]
                                    )
                                )
                                []
                                []
                            , EdgeStmt (NodeId (ID "n") Nothing)
                                (EdgeSubgraph Graph
                                    (Subgraph Nothing
                                        [ NodeStmt (NodeId (ID "q") Nothing) []
                                        , NodeStmt (NodeId (ID "r") Nothing) []
                                        ]
                                    )
                                )
                                []
                                []
                            , EdgeStmt (NodeId (ID "o") Nothing)
                                (EdgeSubgraph Graph
                                    (Subgraph Nothing
                                        [ NodeStmt (NodeId (ID "s") Nothing) []
                                        , NodeStmt (NodeId (ID "p") Nothing) []
                                        ]
                                    )
                                )
                                []
                                []
                            , EdgeStmt (NodeId (ID "p") Nothing)
                                (EdgeSubgraph Graph
                                    (Subgraph Nothing
                                        [ NodeStmt (NodeId (ID "s") Nothing) []
                                        , NodeStmt (NodeId (ID "t") Nothing) []
                                        , NodeStmt (NodeId (ID "q") Nothing) []
                                        ]
                                    )
                                )
                                []
                                []
                            , EdgeStmt (NodeId (ID "q") Nothing)
                                (EdgeSubgraph Graph
                                    (Subgraph Nothing
                                        [ NodeStmt (NodeId (ID "t") Nothing) []
                                        , NodeStmt (NodeId (ID "r") Nothing) []
                                        ]
                                    )
                                )
                                []
                                []
                            , EdgeStmt (NodeId (ID "r") Nothing)
                                (EdgeNode Graph (NodeId (ID "t") Nothing))
                                []
                                []
                            , EdgeStmt (NodeId (ID "s") Nothing)
                                (EdgeNode Graph (NodeId (ID "z") Nothing))
                                []
                                []
                            , EdgeStmt (NodeId (ID "t") Nothing)
                                (EdgeNode Graph (NodeId (ID "z") Nothing))
                                []
                                []
                            ]
                        )
                    )
        ]
