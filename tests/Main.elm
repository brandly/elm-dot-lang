module Main exposing (testFromString, testToString)

import DotLang exposing (..)
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Html.Parser exposing (Attribute, Node(..))
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


psg : String
psg =
    -- https://graphviz.gitlab.io/_pages/Gallery/directed/psg.html
    "##\"I made a program to generate dot files representing the LR(0) state graph along with computed LALR(1) lookahead for an arbitrary context-free grammar, to make the diagrams I used in this article: http://blog.lab49.com/archives/2471. The program also highlights errant nodes in red if the grammar would produce a shift/reduce or reduce/reduce conflict -- you may be able to go to http://kthielen.dnsalias.com:8082/ to produce a graph more to your liking\". Contributed by Kalani Thielen.\n\n##Command to get the layout: \"dot -Gsize=10,15 -Tpng thisfile > thisfile.png\"\n\ndigraph g {\n  graph [fontsize=30 labelloc=\"t\" label=\"\" splines=true overlap=false rankdir = \"LR\"];\n  ratio = auto;\n  \"state0\" [ style = \"filled, bold\" penwidth = 5 fillcolor = \"white\" fontname = \"Courier New\" shape = \"Mrecord\" label =<<table border=\"0\" cellborder=\"0\" cellpadding=\"3\" bgcolor=\"white\"><tr><td bgcolor=\"black\" align=\"center\" colspan=\"2\"><font color=\"white\">State #0</font></td></tr><tr><td align=\"left\" port=\"r0\">&#40;0&#41; s -&gt; &bull;e $ </td></tr><tr><td align=\"left\" port=\"r1\">&#40;1&#41; e -&gt; &bull;l '=' r </td></tr><tr><td align=\"left\" port=\"r2\">&#40;2&#41; e -&gt; &bull;r </td></tr><tr><td align=\"left\" port=\"r3\">&#40;3&#41; l -&gt; &bull;'*' r </td></tr><tr><td align=\"left\" port=\"r4\">&#40;4&#41; l -&gt; &bull;'n' </td></tr><tr><td align=\"left\" port=\"r5\">&#40;5&#41; r -&gt; &bull;l </td></tr></table>> ];\n  \"state1\" [ style = \"filled\" penwidth = 1 fillcolor = \"white\" fontname = \"Courier New\" shape = \"Mrecord\" label =<<table border=\"0\" cellborder=\"0\" cellpadding=\"3\" bgcolor=\"white\"><tr><td bgcolor=\"black\" align=\"center\" colspan=\"2\"><font color=\"white\">State #1</font></td></tr><tr><td align=\"left\" port=\"r3\">&#40;3&#41; l -&gt; &bull;'*' r </td></tr><tr><td align=\"left\" port=\"r3\">&#40;3&#41; l -&gt; '*' &bull;r </td></tr><tr><td align=\"left\" port=\"r4\">&#40;4&#41; l -&gt; &bull;'n' </td></tr><tr><td align=\"left\" port=\"r5\">&#40;5&#41; r -&gt; &bull;l </td></tr></table>> ];\n  \"state2\" [ style = \"filled\" penwidth = 1 fillcolor = \"white\" fontname = \"Courier New\" shape = \"Mrecord\" label =<<table border=\"0\" cellborder=\"0\" cellpadding=\"3\" bgcolor=\"white\"><tr><td bgcolor=\"black\" align=\"center\" colspan=\"2\"><font color=\"white\">State #2</font></td></tr><tr><td align=\"left\" port=\"r4\">&#40;4&#41; l -&gt; 'n' &bull;</td><td bgcolor=\"grey\" align=\"right\">=$</td></tr></table>> ];\n  \"state3\" [ style = \"filled\" penwidth = 1 fillcolor = \"white\" fontname = \"Courier New\" shape = \"Mrecord\" label =<<table border=\"0\" cellborder=\"0\" cellpadding=\"3\" bgcolor=\"white\"><tr><td bgcolor=\"black\" align=\"center\" colspan=\"2\"><font color=\"white\">State #3</font></td></tr><tr><td align=\"left\" port=\"r5\">&#40;5&#41; r -&gt; l &bull;</td><td bgcolor=\"grey\" align=\"right\">=$</td></tr></table>> ];\n  \"state4\" [ style = \"filled\" penwidth = 1 fillcolor = \"white\" fontname = \"Courier New\" shape = \"Mrecord\" label =<<table border=\"0\" cellborder=\"0\" cellpadding=\"3\" bgcolor=\"white\"><tr><td bgcolor=\"black\" align=\"center\" colspan=\"2\"><font color=\"white\">State #4</font></td></tr><tr><td align=\"left\" port=\"r3\">&#40;3&#41; l -&gt; '*' r &bull;</td><td bgcolor=\"grey\" align=\"right\">=$</td></tr></table>> ];\n  \"state5\" [ style = \"filled\" penwidth = 1 fillcolor = \"black\" fontname = \"Courier New\" shape = \"Mrecord\" label =<<table border=\"0\" cellborder=\"0\" cellpadding=\"3\" bgcolor=\"black\"><tr><td bgcolor=\"black\" align=\"center\" colspan=\"2\"><font color=\"white\">State #5</font></td></tr><tr><td align=\"left\" port=\"r0\"><font color=\"white\">&#40;0&#41; s -&gt; e &bull;$ </font></td></tr></table>> ];\n  \"state6\" [ style = \"filled\" penwidth = 1 fillcolor = \"white\" fontname = \"Courier New\" shape = \"Mrecord\" label =<<table border=\"0\" cellborder=\"0\" cellpadding=\"3\" bgcolor=\"white\"><tr><td bgcolor=\"black\" align=\"center\" colspan=\"2\"><font color=\"white\">State #6</font></td></tr><tr><td align=\"left\" port=\"r1\">&#40;1&#41; e -&gt; l &bull;'=' r </td></tr><tr><td align=\"left\" port=\"r5\">&#40;5&#41; r -&gt; l &bull;</td><td bgcolor=\"grey\" align=\"right\">$</td></tr></table>> ];\n  \"state7\" [ style = \"filled\" penwidth = 1 fillcolor = \"white\" fontname = \"Courier New\" shape = \"Mrecord\" label =<<table border=\"0\" cellborder=\"0\" cellpadding=\"3\" bgcolor=\"white\"><tr><td bgcolor=\"black\" align=\"center\" colspan=\"2\"><font color=\"white\">State #7</font></td></tr><tr><td align=\"left\" port=\"r1\">&#40;1&#41; e -&gt; l '=' &bull;r </td></tr><tr><td align=\"left\" port=\"r3\">&#40;3&#41; l -&gt; &bull;'*' r </td></tr><tr><td align=\"left\" port=\"r4\">&#40;4&#41; l -&gt; &bull;'n' </td></tr><tr><td align=\"left\" port=\"r5\">&#40;5&#41; r -&gt; &bull;l </td></tr></table>> ];\n  \"state8\" [ style = \"filled\" penwidth = 1 fillcolor = \"white\" fontname = \"Courier New\" shape = \"Mrecord\" label =<<table border=\"0\" cellborder=\"0\" cellpadding=\"3\" bgcolor=\"white\"><tr><td bgcolor=\"black\" align=\"center\" colspan=\"2\"><font color=\"white\">State #8</font></td></tr><tr><td align=\"left\" port=\"r1\">&#40;1&#41; e -&gt; l '=' r &bull;</td><td bgcolor=\"grey\" align=\"right\">$</td></tr></table>> ];\n  \"state9\" [ style = \"filled\" penwidth = 1 fillcolor = \"white\" fontname = \"Courier New\" shape = \"Mrecord\" label =<<table border=\"0\" cellborder=\"0\" cellpadding=\"3\" bgcolor=\"white\"><tr><td bgcolor=\"black\" align=\"center\" colspan=\"2\"><font color=\"white\">State #9</font></td></tr><tr><td align=\"left\" port=\"r2\">&#40;2&#41; e -&gt; r &bull;</td><td bgcolor=\"grey\" align=\"right\">$</td></tr></table>> ];\n  state0 -> state5 [ penwidth = 5 fontsize = 28 fontcolor = \"black\" label = \"e\" ];\n  state0 -> state6 [ penwidth = 5 fontsize = 28 fontcolor = \"black\" label = \"l\" ];\n  state0 -> state9 [ penwidth = 5 fontsize = 28 fontcolor = \"black\" label = \"r\" ];\n  state0 -> state1 [ penwidth = 1 fontsize = 14 fontcolor = \"grey28\" label = \"'*'\" ];\n  state0 -> state2 [ penwidth = 1 fontsize = 14 fontcolor = \"grey28\" label = \"'n'\" ];\n  state1 -> state1 [ penwidth = 1 fontsize = 14 fontcolor = \"grey28\" label = \"'*'\" ];\n  state1 -> state4 [ penwidth = 5 fontsize = 28 fontcolor = \"black\" label = \"r\" ];\n  state1 -> state2 [ penwidth = 1 fontsize = 14 fontcolor = \"grey28\" label = \"'n'\" ];\n  state1 -> state3 [ penwidth = 5 fontsize = 28 fontcolor = \"black\" label = \"l\" ];\n  state6 -> state7 [ penwidth = 1 fontsize = 14 fontcolor = \"grey28\" label = \"'='\" ];\n  state7 -> state8 [ penwidth = 5 fontsize = 28 fontcolor = \"black\" label = \"r\" ];\n  state7 -> state1 [ penwidth = 1 fontsize = 14 fontcolor = \"grey28\" label = \"'*'\" ];\n  state7 -> state2 [ penwidth = 1 fontsize = 14 fontcolor = \"grey28\" label = \"'n'\" ];\n  state7 -> state3 [ penwidth = 5 fontsize = 28 fontcolor = \"black\" label = \"l\" ];\n}\n"


testFromString : Test
testFromString =
    let
        edge : String -> String -> Stmt
        edge a b =
            EdgeStmtNode (NodeId (ID a) Nothing) (EdgeNode (NodeId (ID b) Nothing)) [] []
    in
    describe "Dot Lang Parser"
        [ test "parsing simple graph" <|
            \_ ->
                Expect.equal (fromString simpleGraph)
                    (Ok
                        (Dot Graph
                            Nothing
                            [ edge "a" "b"
                            , edge "b" "c"
                            , edge "a" "c"
                            , edge "d" "c"
                            , edge "e" "c"
                            , edge "e" "a"
                            ]
                        )
                    )
        , test "don't need semicolons" <|
            \_ ->
                Expect.equal (fromString (String.filter ((/=) ';') simpleGraph))
                    (Ok
                        (Dot Graph
                            Nothing
                            [ edge "a" "b"
                            , edge "b" "c"
                            , edge "a" "c"
                            , edge "d" "c"
                            , edge "e" "c"
                            , edge "e" "a"
                            ]
                        )
                    )
        , test "can start with comments" <|
            \_ ->
                Expect.equal (fromString ("## This is a comment\n\n// Here's another\n\n" ++ simpleGraph))
                    (Ok
                        (Dot Graph
                            Nothing
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
                Expect.equal
                    (fromString
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
                            [ edge "a" "b"
                            , edge "b" "c"
                            , edge "c" "d"
                            , edge "d" "a"
                            ]
                        )
                    )
        , test "graph with id" <|
            \_ ->
                Expect.equal
                    (fromString
                        (String.join "\n"
                            [ "digraph hello {"
                            , "    a -> b;"
                            , "}"
                            ]
                        )
                    )
                    (Ok
                        (Dot Digraph
                            (Just (ID "hello"))
                            [ edge "a" "b"
                            ]
                        )
                    )
        , test "parsing full digraph" <|
            \_ ->
                Expect.equal
                    (fromString
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
                                (EdgeNode (NodeId (ID "b") Nothing))
                                []
                                [ Attr (ID "label") (ID "0.2"), Attr (ID "weight") (ID "0.2") ]
                            , EdgeStmtNode (NodeId (ID "a") Nothing)
                                (EdgeNode (NodeId (ID "c") Nothing))
                                []
                                [ Attr (ID "label") (ID "0.4"), Attr (ID "weight") (ID "0.4") ]
                            , EdgeStmtNode (NodeId (ID "c") Nothing)
                                (EdgeNode (NodeId (ID "b") Nothing))
                                []
                                [ Attr (ID "label") (ID "0.6"), Attr (ID "weight") (ID "0.6") ]
                            , EdgeStmtNode (NodeId (ID "c") Nothing)
                                (EdgeNode (NodeId (ID "e") Nothing))
                                []
                                [ Attr (ID "label") (ID "0.6"), Attr (ID "weight") (ID "0.6") ]
                            , EdgeStmtNode (NodeId (ID "e") Nothing)
                                (EdgeNode (NodeId (ID "e") Nothing))
                                []
                                [ Attr (ID "label") (ID "0.1"), Attr (ID "weight") (ID "0.1") ]
                            , EdgeStmtNode (NodeId (ID "e") Nothing)
                                (EdgeNode (NodeId (ID "b") Nothing))
                                []
                                [ Attr (ID "label") (ID "0.7"), Attr (ID "weight") (ID "0.7") ]
                            ]
                        )
                    )
        , test "parsing 'showing a path'" <|
            \_ ->
                Expect.equal
                    (fromString
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
                                (EdgeNode (NodeId (ID "b") Nothing))
                                []
                                [ Attr (ID "color") (ID "red"), Attr (ID "penwidth") (NumeralID 3) ]
                            , edge "b" "c"
                            , EdgeStmtNode (NodeId (ID "c") Nothing)
                                (EdgeNode (NodeId (ID "d") Nothing))
                                []
                                [ Attr (ID "color") (ID "red"), Attr (ID "penwidth") (NumeralID 3) ]
                            , edge "d" "e"
                            , edge "e" "f"
                            , edge "a" "d"
                            , EdgeStmtNode (NodeId (ID "b") Nothing)
                                (EdgeNode (NodeId (ID "d") Nothing))
                                []
                                [ Attr (ID "color") (ID "red"), Attr (ID "penwidth") (NumeralID 3) ]
                            , EdgeStmtNode (NodeId (ID "c") Nothing)
                                (EdgeNode (NodeId (ID "f") Nothing))
                                []
                                [ Attr (ID "color") (ID "red"), Attr (ID "penwidth") (NumeralID 3) ]
                            ]
                        )
                    )
        , test "parsing 'showing a path' shorthand" <|
            \_ ->
                Expect.equal
                    (fromString
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
                                (EdgeNode (NodeId (ID "b") Nothing))
                                [ EdgeNode (NodeId (ID "d") Nothing)
                                , EdgeNode (NodeId (ID "c") Nothing)
                                , EdgeNode (NodeId (ID "f") Nothing)
                                ]
                                [ Attr (ID "color") (ID "red"), Attr (ID "penwidth") (NumeralID 3) ]
                            , edge "b" "c"
                            , edge "d" "e"
                            , edge "e" "f"
                            , edge "a" "d"
                            ]
                        )
                    )
        , test "parsing subgraph" <|
            \_ ->
                Expect.equal
                    (fromString
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
                            ]
                        )
                    )
                    (Ok
                        (Dot Digraph
                            Nothing
                            [ SubgraphStmt <|
                                Subgraph (Just (ID "cluster_0"))
                                    [ LooseAttr (Attr (ID "label") (ID "Subgraph A"))
                                    , edge "a" "b"
                                    , edge "b" "c"
                                    , edge "c" "d"
                                    ]
                            , SubgraphStmt <|
                                Subgraph (Just (ID "cluster_1"))
                                    [ LooseAttr (Attr (ID "label") (ID "Subgraph B"))
                                    , edge "a" "f"
                                    , edge "f" "c"
                                    ]
                            ]
                        )
                    )
        , test "subgraph edge" <|
            \_ ->
                Expect.equal
                    (fromString
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
                                (EdgeSubgraph
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
                    (fromString
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
                            , EdgeStmtNode (NodeId (ID "a") Nothing)
                                (EdgeSubgraph
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
                                (EdgeSubgraph
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
                                    (Subgraph Nothing
                                        [ NodeStmt (NodeId (ID "e") Nothing) []
                                        , NodeStmt (NodeId (ID "f") Nothing) []
                                        ]
                                    )
                                )
                                []
                                []
                            , EdgeStmtNode (NodeId (ID "d") Nothing)
                                (EdgeSubgraph
                                    (Subgraph Nothing
                                        [ NodeStmt (NodeId (ID "f") Nothing) []
                                        , NodeStmt (NodeId (ID "g") Nothing) []
                                        ]
                                    )
                                )
                                []
                                []
                            , EdgeStmtNode (NodeId (ID "e") Nothing)
                                (EdgeNode (NodeId (ID "h") Nothing))
                                []
                                []
                            , EdgeStmtNode (NodeId (ID "f") Nothing)
                                (EdgeSubgraph
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
                                (EdgeNode (NodeId (ID "k") Nothing))
                                []
                                []
                            , EdgeStmtNode (NodeId (ID "h") Nothing)
                                (EdgeSubgraph
                                    (Subgraph Nothing
                                        [ NodeStmt (NodeId (ID "o") Nothing) []
                                        , NodeStmt (NodeId (ID "l") Nothing) []
                                        ]
                                    )
                                )
                                []
                                []
                            , EdgeStmtNode (NodeId (ID "i") Nothing)
                                (EdgeSubgraph
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
                                (EdgeSubgraph
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
                                (EdgeSubgraph
                                    (Subgraph Nothing
                                        [ NodeStmt (NodeId (ID "n") Nothing) []
                                        , NodeStmt (NodeId (ID "r") Nothing) []
                                        ]
                                    )
                                )
                                []
                                []
                            , EdgeStmtNode (NodeId (ID "l") Nothing)
                                (EdgeSubgraph
                                    (Subgraph Nothing
                                        [ NodeStmt (NodeId (ID "o") Nothing) []
                                        , NodeStmt (NodeId (ID "m") Nothing) []
                                        ]
                                    )
                                )
                                []
                                []
                            , EdgeStmtNode (NodeId (ID "m") Nothing)
                                (EdgeSubgraph
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
                                (EdgeSubgraph
                                    (Subgraph Nothing
                                        [ NodeStmt (NodeId (ID "q") Nothing) []
                                        , NodeStmt (NodeId (ID "r") Nothing) []
                                        ]
                                    )
                                )
                                []
                                []
                            , EdgeStmtNode (NodeId (ID "o") Nothing)
                                (EdgeSubgraph
                                    (Subgraph Nothing
                                        [ NodeStmt (NodeId (ID "s") Nothing) []
                                        , NodeStmt (NodeId (ID "p") Nothing) []
                                        ]
                                    )
                                )
                                []
                                []
                            , EdgeStmtNode (NodeId (ID "p") Nothing)
                                (EdgeSubgraph
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
                                (EdgeSubgraph
                                    (Subgraph Nothing
                                        [ NodeStmt (NodeId (ID "t") Nothing) []
                                        , NodeStmt (NodeId (ID "r") Nothing) []
                                        ]
                                    )
                                )
                                []
                                []
                            , EdgeStmtNode (NodeId (ID "r") Nothing)
                                (EdgeNode (NodeId (ID "t") Nothing))
                                []
                                []
                            , EdgeStmtNode (NodeId (ID "s") Nothing)
                                (EdgeNode (NodeId (ID "z") Nothing))
                                []
                                []
                            , EdgeStmtNode (NodeId (ID "t") Nothing)
                                (EdgeNode (NodeId (ID "z") Nothing))
                                []
                                []
                            ]
                        )
                    )
        , test "repeating attr list" <|
            \_ ->
                Expect.equal
                    (fromString
                        (String.join "\n" [ "graph { a -- b[color=red][business=good]\n }" ])
                    )
                    (Ok
                        (Dot Graph
                            Nothing
                            [ EdgeStmtNode (NodeId (ID "a") Nothing)
                                (EdgeNode (NodeId (ID "b") Nothing))
                                []
                                [ Attr (ID "color") (ID "red"), Attr (ID "business") (ID "good") ]
                            ]
                        )
                    )
        , test "edge starts with subgraph" <|
            \_ ->
                Expect.equal
                    (fromString
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
                                (EdgeNode (NodeId (ID "c") Nothing))
                                []
                                []
                            ]
                        )
                    )
        , test "graph has id" <|
            \_ ->
                Expect.equal
                    (fromString
                        (String.join "\n" [ "graph world { a -- b }" ])
                    )
                    (Ok
                        (Dot Graph
                            (Just (ID "world"))
                            [ edge "a" "b" ]
                        )
                    )
        , test "psg" <|
            \_ ->
                Expect.equal (fromString psg)
                    (Ok
                        (Dot
                            Digraph
                            (Just (ID "g"))
                            [ AttrStmt AttrGraph
                                [ Attr (ID "fontsize") (NumeralID 30), Attr (ID "labelloc") (ID "t"), Attr (ID "label") (ID ""), Attr (ID "splines") (ID "true"), Attr (ID "overlap") (ID "false"), Attr (ID "rankdir") (ID "LR") ]
                            , LooseAttr (Attr (ID "ratio") (ID "auto"))
                            , NodeStmt (NodeId (ID "state0") Nothing) [ Attr (ID "style") (ID "filled, bold"), Attr (ID "penwidth") (NumeralID 5), Attr (ID "fillcolor") (ID "white"), Attr (ID "fontname") (ID "Courier New"), Attr (ID "shape") (ID "Mrecord"), Attr (ID "label") (HtmlID (Element "table" [ ( "border", "0" ), ( "cellborder", "0" ), ( "cellpadding", "3" ), ( "bgcolor", "white" ) ] [ Element "tr" [] [ Element "td" [ ( "bgcolor", "black" ), ( "align", "center" ), ( "colspan", "2" ) ] [ Element "font" [ ( "color", "white" ) ] [ Text "State #0" ] ] ], Element "tr" [] [ Element "td" [ ( "align", "left" ), ( "port", "r0" ) ] [ Text "(0) s -> •e $ " ] ], Element "tr" [] [ Element "td" [ ( "align", "left" ), ( "port", "r1" ) ] [ Text "(1) e -> •l '=' r " ] ], Element "tr" [] [ Element "td" [ ( "align", "left" ), ( "port", "r2" ) ] [ Text "(2) e -> •r " ] ], Element "tr" [] [ Element "td" [ ( "align", "left" ), ( "port", "r3" ) ] [ Text "(3) l -> •'*' r " ] ], Element "tr" [] [ Element "td" [ ( "align", "left" ), ( "port", "r4" ) ] [ Text "(4) l -> •'n' " ] ], Element "tr" [] [ Element "td" [ ( "align", "left" ), ( "port", "r5" ) ] [ Text "(5) r -> •l " ] ] ])) ]
                            , NodeStmt (NodeId (ID "state1") Nothing) [ Attr (ID "style") (ID "filled"), Attr (ID "penwidth") (NumeralID 1), Attr (ID "fillcolor") (ID "white"), Attr (ID "fontname") (ID "Courier New"), Attr (ID "shape") (ID "Mrecord"), Attr (ID "label") (HtmlID (Element "table" [ ( "border", "0" ), ( "cellborder", "0" ), ( "cellpadding", "3" ), ( "bgcolor", "white" ) ] [ Element "tr" [] [ Element "td" [ ( "bgcolor", "black" ), ( "align", "center" ), ( "colspan", "2" ) ] [ Element "font" [ ( "color", "white" ) ] [ Text "State #1" ] ] ], Element "tr" [] [ Element "td" [ ( "align", "left" ), ( "port", "r3" ) ] [ Text "(3) l -> •'*' r " ] ], Element "tr" [] [ Element "td" [ ( "align", "left" ), ( "port", "r3" ) ] [ Text "(3) l -> '*' •r " ] ], Element "tr" [] [ Element "td" [ ( "align", "left" ), ( "port", "r4" ) ] [ Text "(4) l -> •'n' " ] ], Element "tr" [] [ Element "td" [ ( "align", "left" ), ( "port", "r5" ) ] [ Text "(5) r -> •l " ] ] ])) ]
                            , NodeStmt (NodeId (ID "state2") Nothing) [ Attr (ID "style") (ID "filled"), Attr (ID "penwidth") (NumeralID 1), Attr (ID "fillcolor") (ID "white"), Attr (ID "fontname") (ID "Courier New"), Attr (ID "shape") (ID "Mrecord"), Attr (ID "label") (HtmlID (Element "table" [ ( "border", "0" ), ( "cellborder", "0" ), ( "cellpadding", "3" ), ( "bgcolor", "white" ) ] [ Element "tr" [] [ Element "td" [ ( "bgcolor", "black" ), ( "align", "center" ), ( "colspan", "2" ) ] [ Element "font" [ ( "color", "white" ) ] [ Text "State #2" ] ] ], Element "tr" [] [ Element "td" [ ( "align", "left" ), ( "port", "r4" ) ] [ Text "(4) l -> 'n' •" ], Element "td" [ ( "bgcolor", "grey" ), ( "align", "right" ) ] [ Text "=$" ] ] ])) ]
                            , NodeStmt (NodeId (ID "state3") Nothing) [ Attr (ID "style") (ID "filled"), Attr (ID "penwidth") (NumeralID 1), Attr (ID "fillcolor") (ID "white"), Attr (ID "fontname") (ID "Courier New"), Attr (ID "shape") (ID "Mrecord"), Attr (ID "label") (HtmlID (Element "table" [ ( "border", "0" ), ( "cellborder", "0" ), ( "cellpadding", "3" ), ( "bgcolor", "white" ) ] [ Element "tr" [] [ Element "td" [ ( "bgcolor", "black" ), ( "align", "center" ), ( "colspan", "2" ) ] [ Element "font" [ ( "color", "white" ) ] [ Text "State #3" ] ] ], Element "tr" [] [ Element "td" [ ( "align", "left" ), ( "port", "r5" ) ] [ Text "(5) r -> l •" ], Element "td" [ ( "bgcolor", "grey" ), ( "align", "right" ) ] [ Text "=$" ] ] ])) ]
                            , NodeStmt (NodeId (ID "state4") Nothing) [ Attr (ID "style") (ID "filled"), Attr (ID "penwidth") (NumeralID 1), Attr (ID "fillcolor") (ID "white"), Attr (ID "fontname") (ID "Courier New"), Attr (ID "shape") (ID "Mrecord"), Attr (ID "label") (HtmlID (Element "table" [ ( "border", "0" ), ( "cellborder", "0" ), ( "cellpadding", "3" ), ( "bgcolor", "white" ) ] [ Element "tr" [] [ Element "td" [ ( "bgcolor", "black" ), ( "align", "center" ), ( "colspan", "2" ) ] [ Element "font" [ ( "color", "white" ) ] [ Text "State #4" ] ] ], Element "tr" [] [ Element "td" [ ( "align", "left" ), ( "port", "r3" ) ] [ Text "(3) l -> '*' r •" ], Element "td" [ ( "bgcolor", "grey" ), ( "align", "right" ) ] [ Text "=$" ] ] ])) ]
                            , NodeStmt (NodeId (ID "state5") Nothing) [ Attr (ID "style") (ID "filled"), Attr (ID "penwidth") (NumeralID 1), Attr (ID "fillcolor") (ID "black"), Attr (ID "fontname") (ID "Courier New"), Attr (ID "shape") (ID "Mrecord"), Attr (ID "label") (HtmlID (Element "table" [ ( "border", "0" ), ( "cellborder", "0" ), ( "cellpadding", "3" ), ( "bgcolor", "black" ) ] [ Element "tr" [] [ Element "td" [ ( "bgcolor", "black" ), ( "align", "center" ), ( "colspan", "2" ) ] [ Element "font" [ ( "color", "white" ) ] [ Text "State #5" ] ] ], Element "tr" [] [ Element "td" [ ( "align", "left" ), ( "port", "r0" ) ] [ Element "font" [ ( "color", "white" ) ] [ Text "(0) s -> e •$ " ] ] ] ])) ]
                            , NodeStmt (NodeId (ID "state6") Nothing) [ Attr (ID "style") (ID "filled"), Attr (ID "penwidth") (NumeralID 1), Attr (ID "fillcolor") (ID "white"), Attr (ID "fontname") (ID "Courier New"), Attr (ID "shape") (ID "Mrecord"), Attr (ID "label") (HtmlID (Element "table" [ ( "border", "0" ), ( "cellborder", "0" ), ( "cellpadding", "3" ), ( "bgcolor", "white" ) ] [ Element "tr" [] [ Element "td" [ ( "bgcolor", "black" ), ( "align", "center" ), ( "colspan", "2" ) ] [ Element "font" [ ( "color", "white" ) ] [ Text "State #6" ] ] ], Element "tr" [] [ Element "td" [ ( "align", "left" ), ( "port", "r1" ) ] [ Text "(1) e -> l •'=' r " ] ], Element "tr" [] [ Element "td" [ ( "align", "left" ), ( "port", "r5" ) ] [ Text "(5) r -> l •" ], Element "td" [ ( "bgcolor", "grey" ), ( "align", "right" ) ] [ Text "$" ] ] ])) ]
                            , NodeStmt (NodeId (ID "state7") Nothing) [ Attr (ID "style") (ID "filled"), Attr (ID "penwidth") (NumeralID 1), Attr (ID "fillcolor") (ID "white"), Attr (ID "fontname") (ID "Courier New"), Attr (ID "shape") (ID "Mrecord"), Attr (ID "label") (HtmlID (Element "table" [ ( "border", "0" ), ( "cellborder", "0" ), ( "cellpadding", "3" ), ( "bgcolor", "white" ) ] [ Element "tr" [] [ Element "td" [ ( "bgcolor", "black" ), ( "align", "center" ), ( "colspan", "2" ) ] [ Element "font" [ ( "color", "white" ) ] [ Text "State #7" ] ] ], Element "tr" [] [ Element "td" [ ( "align", "left" ), ( "port", "r1" ) ] [ Text "(1) e -> l '=' •r " ] ], Element "tr" [] [ Element "td" [ ( "align", "left" ), ( "port", "r3" ) ] [ Text "(3) l -> •'*' r " ] ], Element "tr" [] [ Element "td" [ ( "align", "left" ), ( "port", "r4" ) ] [ Text "(4) l -> •'n' " ] ], Element "tr" [] [ Element "td" [ ( "align", "left" ), ( "port", "r5" ) ] [ Text "(5) r -> •l " ] ] ])) ]
                            , NodeStmt (NodeId (ID "state8") Nothing) [ Attr (ID "style") (ID "filled"), Attr (ID "penwidth") (NumeralID 1), Attr (ID "fillcolor") (ID "white"), Attr (ID "fontname") (ID "Courier New"), Attr (ID "shape") (ID "Mrecord"), Attr (ID "label") (HtmlID (Element "table" [ ( "border", "0" ), ( "cellborder", "0" ), ( "cellpadding", "3" ), ( "bgcolor", "white" ) ] [ Element "tr" [] [ Element "td" [ ( "bgcolor", "black" ), ( "align", "center" ), ( "colspan", "2" ) ] [ Element "font" [ ( "color", "white" ) ] [ Text "State #8" ] ] ], Element "tr" [] [ Element "td" [ ( "align", "left" ), ( "port", "r1" ) ] [ Text "(1) e -> l '=' r •" ], Element "td" [ ( "bgcolor", "grey" ), ( "align", "right" ) ] [ Text "$" ] ] ])) ]
                            , NodeStmt (NodeId (ID "state9") Nothing) [ Attr (ID "style") (ID "filled"), Attr (ID "penwidth") (NumeralID 1), Attr (ID "fillcolor") (ID "white"), Attr (ID "fontname") (ID "Courier New"), Attr (ID "shape") (ID "Mrecord"), Attr (ID "label") (HtmlID (Element "table" [ ( "border", "0" ), ( "cellborder", "0" ), ( "cellpadding", "3" ), ( "bgcolor", "white" ) ] [ Element "tr" [] [ Element "td" [ ( "bgcolor", "black" ), ( "align", "center" ), ( "colspan", "2" ) ] [ Element "font" [ ( "color", "white" ) ] [ Text "State #9" ] ] ], Element "tr" [] [ Element "td" [ ( "align", "left" ), ( "port", "r2" ) ] [ Text "(2) e -> r •" ], Element "td" [ ( "bgcolor", "grey" ), ( "align", "right" ) ] [ Text "$" ] ] ])) ]
                            , EdgeStmtNode (NodeId (ID "state0") Nothing) (EdgeNode (NodeId (ID "state5") Nothing)) [] [ Attr (ID "penwidth") (NumeralID 5), Attr (ID "fontsize") (NumeralID 28), Attr (ID "fontcolor") (ID "black"), Attr (ID "label") (ID "e") ]
                            , EdgeStmtNode (NodeId (ID "state0") Nothing) (EdgeNode (NodeId (ID "state6") Nothing)) [] [ Attr (ID "penwidth") (NumeralID 5), Attr (ID "fontsize") (NumeralID 28), Attr (ID "fontcolor") (ID "black"), Attr (ID "label") (ID "l") ]
                            , EdgeStmtNode (NodeId (ID "state0") Nothing) (EdgeNode (NodeId (ID "state9") Nothing)) [] [ Attr (ID "penwidth") (NumeralID 5), Attr (ID "fontsize") (NumeralID 28), Attr (ID "fontcolor") (ID "black"), Attr (ID "label") (ID "r") ]
                            , EdgeStmtNode (NodeId (ID "state0") Nothing) (EdgeNode (NodeId (ID "state1") Nothing)) [] [ Attr (ID "penwidth") (NumeralID 1), Attr (ID "fontsize") (NumeralID 14), Attr (ID "fontcolor") (ID "grey28"), Attr (ID "label") (ID "'*'") ]
                            , EdgeStmtNode (NodeId (ID "state0") Nothing) (EdgeNode (NodeId (ID "state2") Nothing)) [] [ Attr (ID "penwidth") (NumeralID 1), Attr (ID "fontsize") (NumeralID 14), Attr (ID "fontcolor") (ID "grey28"), Attr (ID "label") (ID "'n'") ]
                            , EdgeStmtNode (NodeId (ID "state1") Nothing) (EdgeNode (NodeId (ID "state1") Nothing)) [] [ Attr (ID "penwidth") (NumeralID 1), Attr (ID "fontsize") (NumeralID 14), Attr (ID "fontcolor") (ID "grey28"), Attr (ID "label") (ID "'*'") ]
                            , EdgeStmtNode (NodeId (ID "state1") Nothing) (EdgeNode (NodeId (ID "state4") Nothing)) [] [ Attr (ID "penwidth") (NumeralID 5), Attr (ID "fontsize") (NumeralID 28), Attr (ID "fontcolor") (ID "black"), Attr (ID "label") (ID "r") ]
                            , EdgeStmtNode (NodeId (ID "state1") Nothing) (EdgeNode (NodeId (ID "state2") Nothing)) [] [ Attr (ID "penwidth") (NumeralID 1), Attr (ID "fontsize") (NumeralID 14), Attr (ID "fontcolor") (ID "grey28"), Attr (ID "label") (ID "'n'") ]
                            , EdgeStmtNode (NodeId (ID "state1") Nothing) (EdgeNode (NodeId (ID "state3") Nothing)) [] [ Attr (ID "penwidth") (NumeralID 5), Attr (ID "fontsize") (NumeralID 28), Attr (ID "fontcolor") (ID "black"), Attr (ID "label") (ID "l") ]
                            , EdgeStmtNode (NodeId (ID "state6") Nothing) (EdgeNode (NodeId (ID "state7") Nothing)) [] [ Attr (ID "penwidth") (NumeralID 1), Attr (ID "fontsize") (NumeralID 14), Attr (ID "fontcolor") (ID "grey28"), Attr (ID "label") (ID "'='") ]
                            , EdgeStmtNode (NodeId (ID "state7") Nothing) (EdgeNode (NodeId (ID "state8") Nothing)) [] [ Attr (ID "penwidth") (NumeralID 5), Attr (ID "fontsize") (NumeralID 28), Attr (ID "fontcolor") (ID "black"), Attr (ID "label") (ID "r") ]
                            , EdgeStmtNode (NodeId (ID "state7") Nothing) (EdgeNode (NodeId (ID "state1") Nothing)) [] [ Attr (ID "penwidth") (NumeralID 1), Attr (ID "fontsize") (NumeralID 14), Attr (ID "fontcolor") (ID "grey28"), Attr (ID "label") (ID "'*'") ]
                            , EdgeStmtNode (NodeId (ID "state7") Nothing) (EdgeNode (NodeId (ID "state2") Nothing)) [] [ Attr (ID "penwidth") (NumeralID 1), Attr (ID "fontsize") (NumeralID 14), Attr (ID "fontcolor") (ID "grey28"), Attr (ID "label") (ID "'n'") ]
                            , EdgeStmtNode (NodeId (ID "state7") Nothing) (EdgeNode (NodeId (ID "state3") Nothing)) [] [ Attr (ID "penwidth") (NumeralID 5), Attr (ID "fontsize") (NumeralID 28), Attr (ID "fontcolor") (ID "black"), Attr (ID "label") (ID "l") ]
                            ]
                        )
                    )
        , test "graph with arrows is an Err" <|
            \_ ->
                Expect.equal
                    (fromString "graph { a -> b}")
                    (Err
                        [ { row = 1
                          , col = 13
                          , problem = Parser.Problem "Expected a graph, but this edge is for a digraph."
                          }
                        ]
                    )
        , test "digraph with lines is an Err" <|
            \_ ->
                Expect.equal
                    (fromString "digraph { a -- b}")
                    (Err
                        [ { row = 1
                          , col = 15
                          , problem = Parser.Problem "Expected a digraph, but this edge is for a graph."
                          }
                        ]
                    )
        ]


testToString : Test
testToString =
    describe "toString"
        [ test "basic" <|
            \_ ->
                Expect.equal
                    (toString (Dot Graph Nothing []))
                    "graph {}"
        , test "with id" <|
            \_ ->
                Expect.equal
                    (toString (Dot Graph (Just (ID "abc")) []))
                    "graph abc {}"
        , test "simple graph" <|
            \_ ->
                let
                    noSemis =
                        String.filter ((/=) ';') simpleGraph
                in
                Expect.equal (Result.map toString (fromString noSemis))
                    (Ok noSemis)
        , test "full graph" <|
            \_ ->
                let
                    g =
                        String.join "\n"
                            [ "digraph {"
                            , "    a -> b[label=0.2,weight=0.2]"
                            , "    a -> c[label=0.4,weight=0.4]"
                            , "    c -> b[label=0.6,weight=0.6]"
                            , "    c -> e[label=0.6,weight=0.6]"
                            , "    e -> e[label=0.1,weight=0.1]"
                            , "    e -> b[label=0.7,weight=0.7]"
                            , "}"
                            ]
                in
                Expect.equal
                    (Result.map toString (fromString g))
                    (Ok g)
        , test "subgraphs" <|
            \_ ->
                let
                    g =
                        String.join "\n"
                            [ "digraph {"
                            , "    subgraph cluster_0 {"
                            , "        label=\"Subgraph A\""
                            , "        a -> b"
                            , "        b -> c"
                            , "        c -> d"
                            , "    }"
                            , "    subgraph cluster_1 {"
                            , "        label=\"Subgraph B\""
                            , "        a -> f"
                            , "        f -> c"
                            , "    }"
                            , "}"
                            ]
                in
                Expect.equal
                    (Result.map toString (fromString g))
                    (Ok g)
        , test "can flatten" <|
            \_ ->
                let
                    g =
                        String.join "\n"
                            [ "digraph {"
                            , "  a -> b"
                            , "  b -> c"
                            , "  c -> d"
                            , "}"
                            ]
                in
                Expect.equal
                    (Result.map (toStringWithConfig OneLine) (fromString g))
                    (Ok "digraph { a -> b b -> c c -> d }")
        ]
