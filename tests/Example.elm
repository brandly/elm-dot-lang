module Example exposing (suite)

import DotLang
    exposing
        ( Attr(..)
        , AttrStmtType(..)
        , Dot(..)
        , EdgeType(..)
        , ID(..)
        , NodeId(..)
        , Stmt(..)
        , block
        , parse
        , statement
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


suite : Test
suite =
    let
        edge a uh b =
            EdgeStmt (NodeId (ID a) Nothing) ( uh, NodeId (ID b) Nothing ) []
    in
    describe "Dot Lang Parser"
        [ test "block" <|
            \_ ->
                Expect.equal (Parser.run block "{}") (Ok [])
        , test "statement" <|
            \_ ->
                Expect.equal (Parser.run statement "sup -- dude;dont -- care") (Ok (edge "sup" Graph "dude"))
        , test "statement with attrs" <|
            \_ ->
                Expect.equal (Parser.run statement "sup -- dude[dont=care]")
                    (Ok
                        (EdgeStmt (NodeId (ID "sup") Nothing)
                            ( Graph, NodeId (ID "dude") Nothing )
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
                                ( Digraph, NodeId (ID "b") Nothing )
                                [ Attr (ID "label") (ID "0.2"), Attr (ID "weight") (ID "0.2") ]
                            , EdgeStmt (NodeId (ID "a") Nothing)
                                ( Digraph, NodeId (ID "c") Nothing )
                                [ Attr (ID "label") (ID "0.4"), Attr (ID "weight") (ID "0.4") ]
                            , EdgeStmt (NodeId (ID "c") Nothing)
                                ( Digraph, NodeId (ID "b") Nothing )
                                [ Attr (ID "label") (ID "0.6"), Attr (ID "weight") (ID "0.6") ]
                            , EdgeStmt (NodeId (ID "c") Nothing)
                                ( Digraph, NodeId (ID "e") Nothing )
                                [ Attr (ID "label") (ID "0.6"), Attr (ID "weight") (ID "0.6") ]
                            , EdgeStmt (NodeId (ID "e") Nothing)
                                ( Digraph, NodeId (ID "e") Nothing )
                                [ Attr (ID "label") (ID "0.1"), Attr (ID "weight") (ID "0.1") ]
                            , EdgeStmt (NodeId (ID "e") Nothing)
                                ( Digraph, NodeId (ID "b") Nothing )
                                [ Attr (ID "label") (ID "0.7"), Attr (ID "weight") (ID "0.7") ]
                            ]
                        )
                    )
        ]
