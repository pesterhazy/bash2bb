# bash2bb

## About

Translates bash scripts into [babashka](https://babashka.org/) scripts.

This can be useful in two ways:

- Learning: You already know how to perform a certain task in bash and you'd like to know how to do the same thing in babashka. With `bash2bb`, you can translate your bash knowledge into the beginning of a babashka program.

- Upgrading: You are running up against limitations with a bash script, so you'd like to convert it to babashka. `bash2bb` gives you a rough translation of the existing code. But don't blindly trust the output – always review the generated script!

*This is an early alpha release. Many bash language constructs aren't implemented yet.*

## Installation

bash2bb depends on the [shfmt](https://github.com/mvdan/sh) command-line tool. On macOS this dependency can be installed via homebrew:

```
brew install shfmt
```

You will also need to install [bbin](https://github.com/babashka/bbin):

```
brew install babashka/brew/bbin
```

With that out of the way, you can now use bbin to install bash2bb:

```
bbin install io.github.pesterhazy/bash2bb
```

## Usage

Pass the script you'd like to translate as an argument:

```
bash2bb myscript.bb
```

If no argument is specified, bash2bb reads from stdin.

## Implementation status

- [x] Redirection
- [x] Command substitution
- [x] If statements
- [x] Quoting rules
- [ ] Arrays, in particular `$@`
- [ ] Functions
- [ ] for loops
- [ ] while loops
