module Example exposing (suite)

import DotLang exposing (Directed(..), Dot(..), NodeId(..), Stmt(..), block, parse, statement)
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Parser
import Test exposing (..)


simpleGraph : String
simpleGraph =
    "graph {\n    a -- b;\n    b -- c;\n    a -- c;\n    d -- c;\n    e -- c;\n    e -- a;\n}"


suite : Test
suite =
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
                            [ EdgeStmt (NodeId "a") (NodeId "b")
                            , EdgeStmt (NodeId "b") (NodeId "c")
                            , EdgeStmt (NodeId "a") (NodeId "c")
                            , EdgeStmt (NodeId "d") (NodeId "c")
                            , EdgeStmt (NodeId "e") (NodeId "c")
                            , EdgeStmt (NodeId "e") (NodeId "a")
                            ]
                        )
                    )
        ]
