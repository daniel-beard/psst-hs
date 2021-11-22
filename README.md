# psst-hs

PaSteboard String Tranform tool - Haskell edition.

There are two ways to get data into this tool, `cat` something to it, or just run it and it will take the value in your pasteboard.

This utility transforms strings with a typed dsl. 

E.g.

```shell
echo 'hello world' | stack run 'words |> uppercase |> take(2) |> base64 |> unbase64'
VStringList ["HE","WO"]
```

Most commands work the same across `VString` and `VStringList` types. The function used in the `VString` type is mapped.