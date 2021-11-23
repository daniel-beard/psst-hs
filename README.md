# psst-hs

PaSteboard String Tranform tool - Haskell edition.

There are two ways to get data into this tool, `cat` something to it, or just run it and it will take the value in your pasteboard.

There are a lot of tools for transforming strings on the CLI, but the most useful tool is the one you remember how to use.
This tool aims to be useful, but unsurprising in behavior.

## Concepts

- This utility transforms strings with a typed dsl. 
- The syntax is inspired by Julialang's pipeline operator `|>`
- This utility was born as a reimplentation of https://github.com/daniel-beard/psst but with the following:
    - Less syntax (for now), initially 'command' based
    - Better documentation
    - A typed representation
    - Fast start time (Julia takes a while to warm up)
    - Easy to add new commands
    - Better error messages
    - Interpreted, with ability to output partial results in case of errors

## Examples

```shell
echo 'hello world' | stack run 'words |> uppercase |> take(2) |> base64 |> unbase64'
VStringList ["HE","WO"]
```


## Types and Examples

There are currently only 4 types in `psst-hs`:

```
- VString
- VStringList
- VInt
- VError
```

Many commands work the same across `VString` and `VStringList` types. The function used in the `VString` type is just mapped across the list.

## Commands

Commands are chained by the pipeline operator `|>`. Each command opts in to a particular input type.

### VString

*base64*
- Description: Base64 encoding of a VString
- Example Input: `hello world'
- Example Output: `aGVsbG8gd29ybGQK`

*head*
- Description: Take the first value
- Example Input: `hello world`
- Example Ouput `h`

*length*
- Description: Return a `VInt` representing the length of a `VString`
- Example Input: `hi`
- Example Output: `2`

*lowercase* 
- Description: Return a `VString` with all characters set to lowercase
- Example Input: `HELLO WORLD`
- Example Output: `hello world`

*reverse*
- Description: Return a `VString` with the characters reversed from their original order
- Example Input: `hello world`
- Example Output: `dlrow olleh`

*tail*
- Description: All characters except the first one. Returns a `VString`
- Example Input: `hello world`
- Example Output: `ello world`

*take(Int)*
- Description: Output only the prefix number of characters. Returns a `VString`
- Example Input: `hello world` 4
- Example Output: `hell`

*unbase64*
- Description: Decode a base64 representation. Returns a `VString`.
- Example Input: `aGVsbG8gd29ybGQK`
- Example Output: `hello world`

*uppercase*
- Description: Uppercase the string. Returns a `VString`
- Example Input: `hello world`
- Example Output: `HELLO WORLD`

*words*
- Description: Split a `VString` into components separated by whitespace. Returns a `VStringList`
- Example Input: `hello world`
- Example Output: `["hello", "world"]`

### VStringList

TODO: