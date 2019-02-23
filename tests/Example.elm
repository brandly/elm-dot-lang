module Example exposing (suite)

import DotLang exposing (Dot(..), EdgeType(..), NodeId(..), Stmt(..), block, parse, statement)
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
            EdgeStmt (NodeId a) ( uh, NodeId b )
    in
    describe "Dot Lang Parser"
        [ test "block" <|
            \_ ->
                Expect.equal (Parser.run block "{}") (Ok [])
        , test "statement" <|
            \_ ->
                Expect.equal (Parser.run statement "sup -- dude;dont -- care") (Ok (edge "sup" Graph "dude"))
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
