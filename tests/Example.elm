module Example exposing (suite)

import DotLang exposing (..)
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Parser
import Test exposing (..)



-- examples from https://graphs.grevian.org/example


simpleGraph : String
simpleGraph =
    String.join "\n"
        [ "graph {"
        , "    a -- b;"
        , "    b -- c;"
        , "    a -- c;"
        , "    d -- c;"
        , "    e -- c;"
        , "    e -- a;"
        , "}"
        ]


suite : Test
suite =
    let
        edge : String -> EdgeType -> String -> Stmt
        edge a edgeType b =
            EdgeStmtNode (NodeId (ID a) Nothing) (EdgeNode edgeType (NodeId (ID b) Nothing)) [] []
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
                        (EdgeStmtNode (NodeId (ID "sup") Nothing)
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
                            Nothing
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
                            Nothing
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
                Expect.equal
                    (parse
                        (String.join "\n"
                            [ "digraph {"
                            , "    a -> b;"
                            , "    b -> c;"
                            , "    c -> d;"
                            , "    d -> a;"
                            , "}"
                            ]
                        )
                    )
                    (Ok
                        (Dot Digraph
                            Nothing
                            [ edge "a" Digraph "b"
                            , edge "b" Digraph "c"
                            , edge "c" Digraph "d"
                            , edge "d" Digraph "a"
                            ]
                        )
                    )
        , test "parsing full digraph" <|
            \_ ->
                Expect.equal
                    (parse
                        (String.join "\n"
                            [ "digraph {"
                            , "    a -> b[label=\"0.2\",weight=\"0.2\"];"
                            , "    a -> c[label=\"0.4\",weight=\"0.4\"];"
                            , "    c -> b[label=\"0.6\",weight=\"0.6\"];"
                            , "    c -> e[label=\"0.6\",weight=\"0.6\"];"
                            , "    e -> e[label=\"0.1\",weight=\"0.1\"];"
                            , "    e -> b[label=\"0.7\",weight=\"0.7\"];"
                            , "}"
                            ]
                        )
                    )
                    (Ok
                        (Dot Digraph
                            Nothing
                            [ EdgeStmtNode (NodeId (ID "a") Nothing)
                                (EdgeNode Digraph (NodeId (ID "b") Nothing))
                                []
                                [ Attr (ID "label") (ID "0.2"), Attr (ID "weight") (ID "0.2") ]
                            , EdgeStmtNode (NodeId (ID "a") Nothing)
                                (EdgeNode Digraph (NodeId (ID "c") Nothing))
                                []
                                [ Attr (ID "label") (ID "0.4"), Attr (ID "weight") (ID "0.4") ]
                            , EdgeStmtNode (NodeId (ID "c") Nothing)
                                (EdgeNode Digraph (NodeId (ID "b") Nothing))
                                []
                                [ Attr (ID "label") (ID "0.6"), Attr (ID "weight") (ID "0.6") ]
                            , EdgeStmtNode (NodeId (ID "c") Nothing)
                                (EdgeNode Digraph (NodeId (ID "e") Nothing))
                                []
                                [ Attr (ID "label") (ID "0.6"), Attr (ID "weight") (ID "0.6") ]
                            , EdgeStmtNode (NodeId (ID "e") Nothing)
                                (EdgeNode Digraph (NodeId (ID "e") Nothing))
                                []
                                [ Attr (ID "label") (ID "0.1"), Attr (ID "weight") (ID "0.1") ]
                            , EdgeStmtNode (NodeId (ID "e") Nothing)
                                (EdgeNode Digraph (NodeId (ID "b") Nothing))
                                []
                                [ Attr (ID "label") (ID "0.7"), Attr (ID "weight") (ID "0.7") ]
                            ]
                        )
                    )
        , test "parsing 'showing a path'" <|
            \_ ->
                Expect.equal
                    (parse
                        (String.join "\n"
                            [ "graph {"
                            , "    a -- b[color=red,penwidth=3.0];"
                            , "    b -- c;"
                            , "    c -- d[color=red,penwidth=3.0];"
                            , "    d -- e;"
                            , "    e -- f;"
                            , "    a -- d;"
                            , "    b -- d[color=red,penwidth=3.0];"
                            , "    c -- f[color=red,penwidth=3.0];"
                            , "}"
                            ]
                        )
                    )
                    (Ok
                        (Dot Graph
                            Nothing
                            [ EdgeStmtNode (NodeId (ID "a") Nothing)
                                (EdgeNode Graph (NodeId (ID "b") Nothing))
                                []
                                [ Attr (ID "color") (ID "red"), Attr (ID "penwidth") (ID "3") ]
                            , edge "b" Graph "c"
                            , EdgeStmtNode (NodeId (ID "c") Nothing)
                                (EdgeNode Graph (NodeId (ID "d") Nothing))
                                []
                                [ Attr (ID "color") (ID "red"), Attr (ID "penwidth") (ID "3") ]
                            , edge "d" Graph "e"
                            , edge "e" Graph "f"
                            , edge "a" Graph "d"
                            , EdgeStmtNode (NodeId (ID "b") Nothing)
                                (EdgeNode Graph (NodeId (ID "d") Nothing))
                                []
                                [ Attr (ID "color") (ID "red"), Attr (ID "penwidth") (ID "3") ]
                            , EdgeStmtNode (NodeId (ID "c") Nothing)
                                (EdgeNode Graph (NodeId (ID "f") Nothing))
                                []
                                [ Attr (ID "color") (ID "red"), Attr (ID "penwidth") (ID "3") ]
                            ]
                        )
                    )
        , test "parsing 'showing a path' shorthand" <|
            \_ ->
                Expect.equal
                    (parse
                        (String.join "\n"
                            [ "graph {"
                            , "    a -- b -- d -- c -- f[color=red,penwidth=3.0];"
                            , "    b -- c;"
                            , "    d -- e;"
                            , "    e -- f;"
                            , "    a -- d;"
                            , "}"
                            ]
                        )
                    )
                    (Ok
                        (Dot Graph
                            Nothing
                            [ EdgeStmtNode (NodeId (ID "a") Nothing)
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
                Expect.equal
                    (parse
                        (String.join "\n"
                            [ "digraph {"
                            , "    subgraph cluster_0 {"
                            , "        label=\"Subgraph A\";"
                            , "        a -> b;"
                            , "        b -> c;"
                            , "        c -> d;"
                            , "    }"
                            , "    subgraph cluster_1 {"
                            , "        label=\"Subgraph B\";"
                            , "        a -> f;"
                            , "        f -> c;"
                            , "    }"
                            , "}"
                            , "    "
                            ]
                        )
                    )
                    (Ok
                        (Dot Digraph
                            Nothing
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
                            Nothing
                            [ EdgeStmtNode (NodeId (ID "a") Nothing)
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
                            Nothing
                            [ LooseAttr (Attr (ID "rankdir") (ID "LR"))
                            , Comment "// Left to Right, instead of Top to Bottom"
                            , EdgeStmtNode (NodeId (ID "a") Nothing)
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
                            , EdgeStmtNode (NodeId (ID "b") Nothing)
                                (EdgeSubgraph Graph
                                    (Subgraph Nothing
                                        [ NodeStmt (NodeId (ID "c") Nothing) []
                                        , NodeStmt (NodeId (ID "e") Nothing) []
                                        ]
                                    )
                                )
                                []
                                []
                            , EdgeStmtNode (NodeId (ID "c") Nothing)
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
                            , EdgeStmtNode (NodeId (ID "d") Nothing)
                                (EdgeSubgraph Graph
                                    (Subgraph Nothing
                                        [ NodeStmt (NodeId (ID "f") Nothing) []
                                        , NodeStmt (NodeId (ID "g") Nothing) []
                                        ]
                                    )
                                )
                                []
                                []
                            , EdgeStmtNode (NodeId (ID "e") Nothing)
                                (EdgeNode Graph (NodeId (ID "h") Nothing))
                                []
                                []
                            , EdgeStmtNode (NodeId (ID "f") Nothing)
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
                            , EdgeStmtNode (NodeId (ID "g") Nothing)
                                (EdgeNode Graph (NodeId (ID "k") Nothing))
                                []
                                []
                            , EdgeStmtNode (NodeId (ID "h") Nothing)
                                (EdgeSubgraph Graph
                                    (Subgraph Nothing
                                        [ NodeStmt (NodeId (ID "o") Nothing) []
                                        , NodeStmt (NodeId (ID "l") Nothing) []
                                        ]
                                    )
                                )
                                []
                                []
                            , EdgeStmtNode (NodeId (ID "i") Nothing)
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
                            , EdgeStmtNode (NodeId (ID "j") Nothing)
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
                            , EdgeStmtNode (NodeId (ID "k") Nothing)
                                (EdgeSubgraph Graph
                                    (Subgraph Nothing
                                        [ NodeStmt (NodeId (ID "n") Nothing) []
                                        , NodeStmt (NodeId (ID "r") Nothing) []
                                        ]
                                    )
                                )
                                []
                                []
                            , EdgeStmtNode (NodeId (ID "l") Nothing)
                                (EdgeSubgraph Graph
                                    (Subgraph Nothing
                                        [ NodeStmt (NodeId (ID "o") Nothing) []
                                        , NodeStmt (NodeId (ID "m") Nothing) []
                                        ]
                                    )
                                )
                                []
                                []
                            , EdgeStmtNode (NodeId (ID "m") Nothing)
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
                            , EdgeStmtNode (NodeId (ID "n") Nothing)
                                (EdgeSubgraph Graph
                                    (Subgraph Nothing
                                        [ NodeStmt (NodeId (ID "q") Nothing) []
                                        , NodeStmt (NodeId (ID "r") Nothing) []
                                        ]
                                    )
                                )
                                []
                                []
                            , EdgeStmtNode (NodeId (ID "o") Nothing)
                                (EdgeSubgraph Graph
                                    (Subgraph Nothing
                                        [ NodeStmt (NodeId (ID "s") Nothing) []
                                        , NodeStmt (NodeId (ID "p") Nothing) []
                                        ]
                                    )
                                )
                                []
                                []
                            , EdgeStmtNode (NodeId (ID "p") Nothing)
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
                            , EdgeStmtNode (NodeId (ID "q") Nothing)
                                (EdgeSubgraph Graph
                                    (Subgraph Nothing
                                        [ NodeStmt (NodeId (ID "t") Nothing) []
                                        , NodeStmt (NodeId (ID "r") Nothing) []
                                        ]
                                    )
                                )
                                []
                                []
                            , EdgeStmtNode (NodeId (ID "r") Nothing)
                                (EdgeNode Graph (NodeId (ID "t") Nothing))
                                []
                                []
                            , EdgeStmtNode (NodeId (ID "s") Nothing)
                                (EdgeNode Graph (NodeId (ID "z") Nothing))
                                []
                                []
                            , EdgeStmtNode (NodeId (ID "t") Nothing)
                                (EdgeNode Graph (NodeId (ID "z") Nothing))
                                []
                                []
                            ]
                        )
                    )
        , test "repeating attr list" <|
            \_ ->
                Expect.equal
                    (parse
                        (String.join "\n" [ "graph { a -- b[color=red][business=good]\n }" ])
                    )
                    (Ok
                        (Dot Graph
                            Nothing
                            [ EdgeStmtNode (NodeId (ID "a") Nothing)
                                (EdgeNode Graph (NodeId (ID "b") Nothing))
                                []
                                [ Attr (ID "color") (ID "red"), Attr (ID "business") (ID "good") ]
                            ]
                        )
                    )
        , test "edge starts with subgraph" <|
            \_ ->
                Expect.equal
                    (parse
                        (String.join "\n" [ "graph { { a b } -- c\n }" ])
                    )
                    (Ok
                        (Dot Graph
                            Nothing
                            [ EdgeStmtSubgraph
                                (Subgraph Nothing
                                    [ NodeStmt (NodeId (ID "a") Nothing) []
                                    , NodeStmt (NodeId (ID "b") Nothing) []
                                    ]
                                )
                                (EdgeNode Graph (NodeId (ID "c") Nothing))
                                []
                                []
                            ]
                        )
                    )
        , test "graph has id" <|
            \_ ->
                Expect.equal
                    (parse
                        (String.join "\n" [ "graph world { a -- b }" ])
                    )
                    (Ok
                        (Dot Graph
                            (Just (ID "world"))
                            [ edge "a" Graph "b" ]
                        )
                    )
        ]
