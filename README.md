# bash2bb

## About

Translates bash scripts into [babashka](https://babashka.org/) scripts.

Instead of aiming for 100% faithfulness, bash2bb makes a best-effort attempt at preserving the intent of the script. Bash is a large language and only a subset of features is supported.

Don't blindly trust the generated program! Use the output as a basis for writing a new script.

## Usage

bash2bb depends on [shfmt](https://github.com/mvdan/sh). On macOS this can be installed like so:

```
brew install shfmt
```

Now you're ready to translate scripts:

```
% bb -m bash2bb.core <<< 'echo hi'
(require (quote [babashka.process :refer [shell pipeline pb]]))
(shell "echo" "hi")
```
