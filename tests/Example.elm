module Example exposing (suite)

import DotLang exposing (Directed(..), Dot(..), NodeId(..), Stmt(..), block, parse, statement)
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
        edge a b =
            EdgeStmt (NodeId a) (NodeId b)
    in
    describe "Dot Lang Parser"
        [ test "block" <|
            \_ ->
                Expect.equal (Parser.run block "{}") (Ok [])
        , test "statement" <|
            \_ ->
                Expect.equal (Parser.run statement "sup -- dude;dont -- care") (Ok (EdgeStmt (NodeId "sup") (NodeId "dude")))
        , test "parsing simple graph" <|
            \_ ->
                Expect.equal (parse simpleGraph)
                    (Ok
                        (Dot Graph
                            [ edge "a" "b"
                            , edge "b" "c"
                            , edge "a" "c"
                            , edge "d" "c"
                            , edge "e" "c"
                            , edge "e" "a"
                            ]
                        )
                    )
        , test "parsing simple digraph" <|
            \_ ->
                Expect.equal (parse simpleDigraph)
                    (Ok
                        (Dot Digraph
                            [ edge "a" "b"
                            , edge "b" "c"
                            , edge "c" "d"
                            , edge "d" "a"
                            ]
                        )
                    )
        ]
