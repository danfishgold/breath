# Breath

This is some very messy code I threw together in three hours. It is not great. I don't know if I'll be able to read it in a week, but it gets the job done (except it looks very ugly).

## How to run it

You need to compile the elm code into `elm.js`:

```
elm make src/Main.elm --output=elm.js
```

and then you can open `index.html`.

## How to develop it

Use elm-live:

```
elm-live src/Main.elm -- --output=elm.js
```

and then go to localhost:8000
