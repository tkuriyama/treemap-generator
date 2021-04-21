# Treemap Generator

An interactive generator for squarified treemaps.

## Live Version

[https://tarokuriyama.com/treemap](https://tarokuriyama.com/treemap)

## Build

### Quick

All Elm projects can be built with [`elm make`](https://elmprogramming.com/elm-make.html), such as:

```
elm make examples/Main.elm --optimize --output=elm.js
```

... which builds the example specified in `examples/Main.elm` and compiles it to `elm.js`.


### Full

To follow the "full" build for this repo, run the commands in the `all.do` script (`redo all` if you have [`redo`](https://redo.readthedocs.io/en/latest/), or run it as a shell script like `sh all.do`).

The full build includes a number of Elm-ecosystem dependencies, which can be installed with [`npm`](https://nodejs.org/en/) like so (omit the `-g` flag to install locally for the repo):

```
npm install -g elm-format
npm install -g elm-test
npm install -g elm-optimize-level-2
npm install -g elm-minify
```

Note that `elm-minify` is deprecated. For an alternative minification, see the [`terser` command recommended by `elm-optimize-level-2`](https://github.com/mdgriffith/elm-optimize-level-2/blob/HEAD/notes/minification.md).
