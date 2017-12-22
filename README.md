# Ivy compiler (PureScript)

This is a work-in-progress reimplementation in PureScript of the [Ivy-to-Bitcoin-Script](https://github.com/ivy-lang/ivy-bitcoin) compiler.

Currently the parser and typechecker are implemented. The reference checker, desugaring, stack tracking and code generation still need to be completed.

```
> npm install -g purescript pulp bower
> bower install
> pulp test
```
