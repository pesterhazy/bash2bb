# bash2bb

## About

Translates bash scripts into [babashka](https://babashka.org/) scripts.

This can be useful in two ways:

- Conversion: You are running into limitations with a bash script, so you'd like to convert it to babashka. `bash2bb` gives you a rough translation of the existing code. Don't blindly trust the output – always review the generated script.

- Learning: You know how to perform a certain action in bash and you'd like to know how to do the same in babashka. With `bash2bb`, you can translate your bash knowledge into the beginning of a babashka program.

*This is an early alpha. Lots of bash language constructs aren't working*

## Installation

bash2bb depends on the [shfmt](https://github.com/mvdan/sh) command-line tool. On macOS this can be installed like so:

```
brew install shfmt
```

You will also need to install [bbin](https://github.com/babashka/bbin).

With that out of the way, you can install bash2bb:

```
bbin install io.github.pesterhazy/bash2bb
```

## Usage

Pass the script you'd like to translate as an argument:

```
bash2bb myscript.bb
```

if no argument is specified, bash2bb reads from stdin.
