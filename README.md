## Editing

Run watcher for generator,

```
$ rlwrap sbcl --load watcher.lisp

* (watch)
```

Edit `generator.lisp`. The watcher will recompile `index.html` everytime
`generator.lisp` is saved.

Use any HTTP server to see it in browser, for e.g Node.js `http-server`.
