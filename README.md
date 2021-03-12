# terminusdb-client-haskell

Simple Haskell client, WIP and an experiment.

## Examples

First run:

```
cabal build
cabal exec ghci src/TerminusDBClient.hs
:set -XOverloadedStrings
```

And try some examples:

### Execute a built-in sample query

```haskell
let client = initClient "localhost" "root" "admin" "test"
executeQuery client queryAllDatabases
```

### Sample query

```haskell
queryAllDatabases = Query (Using "_system"
                            (Triple
                             (WOQLVar "X")
                             (WOQLNode "rdf:type")
                             (WOQLNode "system:Database")
                            )
                           )
```

Querying this can be done by using `executeClient`.


```haskell
executeQuery client queryAllDatabases
```

### Print JSON-LD

```haskell
printJSON queryAllDatabases -- prints the WOQL JSONLD
```

## Limitations

- Only supports HTTP
- Port is hardcoded to 6363
