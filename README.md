# Haskheap

This is a simple Haskell library for easily interacting with the refheap API.

Currently WIP.

# Usage (reflects currently implemented functionality during WIP period)

```haskell
import Network.Haskheap

getPaste "1"
```

Output

```haskell
Paste {getLines = 1, getDate = Just 2012-01-04 01:44:22.964 UTC, getID = "1",
getLanguage = "Clojure", getPrivate = False, getURL = Just
https://www.refheap.com/paste/1, getUser = "raynes", getBody = "(begin)"}
```

Note that the reason the id argument is a string and not a number is because
private paste ids are strings.
