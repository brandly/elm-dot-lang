module Example exposing (suite)

import DotLang
    exposing
        ( Attr(..)
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
        ]
