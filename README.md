# WebGL with Elm
Some WebGL examples with the language [Elm](https://elm-lang.org/).

## Dependencies
The following modules must be installed in order to use the examples.
```
elm install elm-explorations/linear-algebra
elm install elm-explorations/webgl
elm install avh4/elm-color
```

## Make
Elm provides the CLI command `elm make` to compile an elm file to a javascript file which can be embedded into a
 custom html document.
```
elm make src/FourRectangles.elm --optimize --output=four-rectangles.js
```

## Run
Elm provides the CLI command `elm reactor` which starts a webserver and let you start each example in this project.
