# dot-lang [![Build Status](https://travis-ci.org/brandly/elm-dot-lang.svg?branch=master)](https://travis-ci.org/brandly/elm-dot-lang)

```elm
import DotLang

DotLang.fromString "graph { a -- b }"
-- => Ok (Dot Graph Nothing [EdgeStmtNode (NodeId (ID "a") Nothing) (EdgeNode Graph (NodeId (ID "b") Nothing)) [] []])
```