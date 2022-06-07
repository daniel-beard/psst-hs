# psst-hs

PaSteboard String Tranform tool - Haskell edition.

There are two ways to get data into this tool, `cat` something to it, or just run it and it will take the value in your pasteboard.

There are a lot of tools for transforming strings on the CLI, but the most useful tool is the one you remember how to use.
This tool aims to be useful, but unsurprising in behavior.

## Concepts

- This utility transforms strings with a typed dsl.
- The syntax is inspired by Julialang's pipeline operator `|>`
- This utility was born as a reimplentation of [daniel-beard/psst](https://github.com/daniel-beard/psst) with the following:
  - Less syntax (for now), 'command' based
  - Better documentation
  - A typed representation
  - Fast start time (Julia takes a while to warm up)
  - Easy to add new commands
  - Better error messages
  - Interpreted, with ability to output partial results in case of errors

## Examples

```shell
echo 'hello world' | psst-hs 'words |> uppercase |> take(2) |> base64 |> unbase64'
["HE","WO"]
```

## Types and Examples

There are currently only a few types in `psst-hs`:

```
- VString
- VStringList
- VInt
- VBool
- VError
```

Many commands work the same across `VString` and `VStringList` types. The function used in the `VString` type is just mapped across the list.

## Commands

Commands are chained by the pipeline operator `|>`. Each command opts in to a particular input type.

### VString

<details><summary>base64 -> VString</summary>

- Description: Base64 encoding of a VString
- Example:

```
echo 'hello world' | psst-hs 'base64'
> aGVsbG8gd29ybGQK
```
</details>

<details><summary>head -> VString</summary>

- Description: Take the first value
- Example:

```
echo 'hello world' | psst-hs 'head'
> h
```
</details>

<details><summary>length -> VInt</summary>

- Description: Return a `VInt` representing the length of a `VString`
- Example:

```
echo 'hi' | psst-hs 'length'
> 2
```
</details>

<details><summary>lowercase -> VString</summary>

- Description: Return a `VString` with all characters set to lowercase
- Example:

```
echo 'HELLO WORLD' | psst-hs 'lowercase'
> "hello world"
```
</details>

<details><summary>match -> VStringList</summary>

- Description: Return all matches for a given regex
- Example:

```
echo 'hello world' | psst-hs 'match("[a-z]+")'
["hello", "world"]
```
</details>

<details><summary>matches -> VBool</summary>

- Description: Return a `VBool` indicating if input matches a regex
- Example:

```
echo "hello world" | psst-hs 'matches("\\w+")'
True
```
</details>

<details><summary>reverse -> VStringList</summary>

- Description: Return a `VString` with the characters reversed from their original order
- Example:

```
echo 'hello world' | psst-hs 'reverse'
> dlrow olleh
```
</details>

<details><summary>tail -> VString</summary>

- Description: All characters except the first one. Returns a `VString`
- Example:

```
echo 'hello world' | psst-hs 'tail'
> ello world
```
</details>

<details><summary>take(Int) -> VString</summary>

- Description: Output only the prefix number of characters. Returns a `VString`
- Example:

```
echo 'hello world' | psst-hs 'take(4)'
> hell
```
</details>

<details><summary>unbase64 -> VString</summary>

- Description: Decode a base64 representation. Returns a `VString`.
- Example:

```
echo 'aGVsbG8gd29ybGQK' | psst-hs 'unbase64'
> hello world
```
</details>

<details><summary>uppercase -> VString</summary>

- Description: Uppercase the string. Returns a `VString`
- Example:

```
echo 'hello world' | psst-hs 'uppercase'
> HELLO WORLD
```
</details>

<details><summary>words -> VStringList</summary>

- Description: Split a `VString` into components separated by whitespace. Returns a `VStringList`
- Example:

```
echo 'hello world' | psst-hs 'words'
> ["hello", "world"]
```
</details>

-------

### VStringList

<details><summary>base64 -> VStringList</summary>

- Description: Maps the `VString` method across a `VStringList`
- Example:

```
echo 'hello world' | psst-hs 'words |> base64'
> ["aGVsbG8=","d29ybGQ="]
```
</details>

<details><summary>length -> VInt</summary>

- Description: Returns the length of the `VStringList` as `VInt`
- Example:

```
echo 'hello world' | psst-hs 'words |> length'
> 2
```
</details>

<details><summary>lowercase -> VStringList</summary>

- Description: Maps the `VString` method across a `VStringList`
- Example:

```
echo 'HELLO WORLD' | psst-hs 'words |> lowercase'
> ["hello", "world"]
```
</details>

<details><summary>reverse -> VStringList</summary>

- Description: Reverse the order of elements within the `VStringList`
- Example:

```
echo 'hello world' | psst-hs 'words |> reverse'
> ["world", "hello"]
```
</details>

<details><summary>unbase64 -> VStringList</summary>

- Description: Maps the `VString` method across a `VStringList`
- Example:

```
echo 'aGVsbG8= d29ybGQ=' | psst-hs 'words |> unbase64'
> ["hello", "world"]
```
</details>

<details><summary>uppercase -> VStringList</summary>

- Description: Maps the `VString` method across a `VStringList`
- Example:

```
echo 'hello world' | psst-hs 'words |> uppercase'
> ["HELLO", "WORLD"]
```
</details>

<details><summary>tail -> VString</summary>

- Description: Returns the last `VString` from a `VStringList`
- Example:

```
echo 'hello world' | psst-hs 'words |> tail'
> ["WORLD"]
```
</details>

<details><summary>take(VInt) -> VStringList</summary>

- Description: Returns the first n `VString` values from a `VStringList`
- Example:

```
echo '1 2 3 4 5' | psst-hs 'words |> take(4)'
> ["1", "2", "3", "4"]
```
</details>

-------

## Building on macOS

```shell
brew install pcre

# For M1s
brew list pcre | grep 'pcre\.h$'

/opt/homebrew/include

ln -s /usr/local/pcre-8.45 /usr/sbin/pcre
ln -s /usr/local/pcre-8.45/include/pcre.h /usr/include/pcre.h
```