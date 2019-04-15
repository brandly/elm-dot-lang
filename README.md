# dot-lang [![Build Status](https://travis-ci.org/brandly/elm-dot-lang.svg?branch=master)](https://travis-ci.org/brandly/elm-dot-lang)

```elm
import DotLang

DotLang.fromString "graph { a -- b }"
-- => Ok (Dot Graph Nothing [EdgeStmtNode (NodeId (ID "a") Nothing) (EdgeNode (NodeId (ID "b") Nothing)) [] []])
```

### development

```
$ npm install
$ npm test
```

the fuzz tests hang at times. to exclusively run unit tests on every save, do this:

```
$ npm test -- tests/Main.elm --watch
```
