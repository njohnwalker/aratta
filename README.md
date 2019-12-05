# arrata

`aratta` is a library intended to implement many different languages in different semantic frameworks using Haskell as a meta-language.

## Getting Started

This library is built using the Haskell `stack` build tool. Instructions to install `stack` are [here](https://docs.haskellstack.org/en/stable/README/#how-to-install

The `houdini-verify` tool requires that the `cvc4` executable is in your PATH.

Currently there are two executables in this project:

- `semantic-run` executes a program given the interpretter defined for a given language.
- `houdini-verify` searches a list of candidate invariants for a valid invariant of a given program.

You may install them with the command

```
stack install
```

## Examples
### `semantic-run`
To execute the program `collatz.imp` as an IMP program using the Small-Step SOS (Transition) semantic framework:

```bash
semantic-run --imp --transition res/imp/collatz.imp
```

To execute the IMP++ program `incUp.imp` with input from the file `oneUpTo5.in`:

```
semantic-run --imp++ -t res/imp++/incUp.imp --input res/imp/oneUpTo5.in
```

### `houdini-verify`
`houdini-verify -h` will give you a more detailed explanation of the options of the program.

```
houdini-verify --src res/gcl/product1.gcl --inv res/gcl/product1.gcl.inv
```

will produce a valid invariant for a simple multiplication program
