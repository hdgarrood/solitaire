# solitaire

A basic Solitaire game, written in PureScript, including both a console and a
web UI.

## Module overview

- `Solitaire.Prelude` - a custom Prelude for this project.

### Game logic

The following modules define basic data types for the game, and operations on
them.

- `Solitaire.Card`
- `Solitaire.Tableaux`
- `Solitaire.Foundations`
- `Solitaire.Stock`
- `Solitaire.Stack`
- `Solitaire.Deck`
- `Solitaire.Game`

### Console UI

The console UI is uses node.js's `readline` module. For printing things, it
uses ANSI escape codes and unicode line-drawing characters to make the output
recognisable as a Solitaire game.

The console UI code is contained in the following modules:

- `Solitaire.Ansi` (this module is also useful for debugging in the PureScript
  repl)
- `Solitaire.Repl`

To play with the console UI, run

```
$ pulp build --main Solitaire.Repl --to game.js && node game.js
```

### Web UI

The web UI uses Halogen, and is contained in the following modules:

- `Solitaire.Web`
- `Solitaire.Web.Positioning`

To play with the web UI, run

```
$ pulp build --main Solitaire.Web --to web/app.js
```

and then open `web/index.html` in a browser window.
