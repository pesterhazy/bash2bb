# bash2bb

**Translates bash scripts into babashka scripts.**

```
% cat myscript.bash
#!/bin/bash
OF=myhome_directory_$(date +%Y%m%d).tar.gz
tar -czf $OF /home/linuxconfig 

% bash2bb myscript.bash
(require (quote [babashka.process :refer [shell pipeline pb]]))
(def OF (System/getenv "OF"))
(def OF (str "myhome_directory_" (:out (shell {:out :string} "date" "+%Y%m%d")) ".tar.gz"))
(shell "tar" "-czf" OF "/home/linuxconfig")
```

## About

`bash2bb` generates a [babashka](https://babashka.org/) program that's roughly equivalent to an input bash script. This can help in a few ways:

- _Learning_: If you already know how to perform a certain task in bash, you may wanto to learn how to do the same thing in babashka. With `bash2bb`, you can translate your bash knowledge into (the beginning of) a babashka program.

- _Upgrading_: When you run up against limitations with bash as a scripting language, you may want to convert an existing bash script to babashka. `bash2bb` gives you a rough translation of the code.

> **Note**
> While `bash2bb` makes an effort to emulate various bash features, the result is likely to contain inaccuracies. Don't blindly trust the output – always review the generated script!

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

and

```
echo 'export PATH="$PATH:$HOME/.babashka/bbin/bin"' >> ~/.$(basename $SHELL)rc && exec $SHELL
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
