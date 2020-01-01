# Naproche-CIC

A prototype for a [controlled natural language](https://en.wikipedia.org/wiki/Controlled_natural_language)
for mathematics with type-theoretical semantics.

## Building

This project uses [stack](http://haskellstack.org/).
A brief tutorial can be found [here](https://seanhess.github.io/2015/08/04/practical-haskell-getting-started.html).

You can start GHCi with `stack repl` and build the project using `stack build`.

## Running

After building, use `stack exec napcic` to run the program on all example files in `work/in/`.
