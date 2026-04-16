# Haskell Scrapbook

![Haskell workflow](https://github.com/frankhjung/haskell-scrapbook/actions/workflows/haskell.yml/badge.svg)

A collection of short Haskell examples exploring functions, folds, recursion,
state, parsing, concurrency, and related techniques.

Package metadata lives in [Cabal](https://www.haskell.org/cabal/), while
day-to-day workflows are driven by
[GNU Make](https://www.gnu.org/software/make/) and
[Stack](https://docs.haskellstack.org/).

## Links

[Haddock](https://www.haskell.org/haddock/doc/html/index.html) API documentation
is available on:

* [GitHub](https://frankhjung.github.io/haskell-scrapbook/)
* [GitLab](https://frankhjung1.gitlab.io/haskell-scrapbook/)

(See also benchmark reports below.)

## Quick Start

The repository contains both library modules under `src/` and runnable example
programs under `app/`. Use `make help` to list the available development
targets.

### Build

Install the required tools first: `stack`, `cabal`, `stylish-haskell`,
`cabal-fmt`, `hlint`, `hasktags`, and `yamllint`.

For an overview of the available automation targets, run:

```bash
make help
```

To initialise local Cabal configuration and update dependencies, run:

```bash
make setup
```

To build the project, run:

```bash
make build
```

To run the default local workflow, run:

```bash
make default
```

This performs formatting, checks, and tests.

For the full workflow, including documentation, benchmarks, and executable
examples, run:

```bash
make all
```

### Format

Format and style code using:

```bash
make format
```

This runs:

```bash
cabal-fmt --inplace Scrapbook.cabal
stylish-haskell --inplace <all Haskell sources>
```

### Check

To only perform code checks, run:

```bash
make check
```

This runs `tags` and `lint`:

```bash
hasktags --ctags --extendedctag <all Haskell sources>
hlint --cross --color --show <all Haskell sources>
cabal check
yamllint --strict <tracked YAML files>
```

### Repl

To start a REPL with the project loaded, use:

```bash
stack repl
```

Using Cabal also works for the REPL:

```bash
cabal repl
```

### Test

Test using GNU Make:

```bash
make test
```

This runs:

```bash
stack test --fast
```

### Performance

The
[Criterion](https://hackage.haskell.org/package/criterion/docs/Criterion.html)
benchmark HTML reports can be generated using
[stack](https://docs.haskellstack.org/en/stable/README/). They are available
from GitHub, here:

* Criterion benchmarks:
  * [MyFilter](https://frankhjung.github.io/haskell-scrapbook/benchmark-myfilter.html)
  * [MyReverse](https://frankhjung.github.io/haskell-scrapbook/benchmark-myreverse.html)
  * [MySum](https://frankhjung.github.io/haskell-scrapbook/benchmark-mysum.html)
  * [PolyDivisors](https://frankhjung.github.io/haskell-scrapbook/benchmark-polydivisors.html)
  * [RecursionSchemes](https://frankhjung.github.io/haskell-scrapbook/benchmark-recursionschemes.html)
  * [RepMax](https://frankhjung.github.io/haskell-scrapbook/benchmark-repmax.html)
  * [SubSeqs](https://frankhjung.github.io/haskell-scrapbook/benchmark-subseqs.html)
  * [TermFoldBench](https://frankhjung.github.io/haskell-scrapbook/benchmark-termFoldBench.html)
  * [ZipFold](https://frankhjung.github.io/haskell-scrapbook/benchmark-zipfold.html)

To run individual benchmark:

```bash
stack bench Scrapbook:bench:polydivisorsBench
```

The `make bench` target writes HTML reports to `.stack-work/`.

### Documentation

To generate [Haddock](https://www.haskell.org/haddock/doc/html/) for source:

```bash
make doc
```

This runs `stack haddock`.

## Code Examples

### Executables

* `counter`: count up and down from a natural number.
* `fpcomplete`: functor and contravariant-style examples based on function
  mapping.
* `json`: decode JSON with a variable top-level key.
* `numberlines`: number the lines of a file, similar to `nl(1)`.
* `polydivs`: search for poly-divisible numbers.
* `quine`: print the program's own source.
* `readfile`: read a file using explicit handle management.
* `skips`: generate every nth element from an input list.
* `stategame`: small stateful game with score tracking.
* `threads`: simple STM and `forkIO` example.
* `vocab`: extract and count distinct words from a file.
* `while`: implement a `while` loop as a higher-order function.
* `wordcount`: count words in a file.
* `wordcountarrow`: count words using arrow-based composition.

### Library Modules

* `ApplyToTuple`: rank-N type example applying one function to two list types.
* `BinarySearch`: binary search over an ordered list.
* `Caesar`: Caesar cipher over a printable ASCII subset.
* `CFold`: continuation-passing fold implementation.
* `Colours`: semigroup example for colour mixing.
* `CountEntries`: count directory entries with several monadic approaches.
* `Cps`: continuation-passing style examples using `Cont` and `callCC`.
* `Expr`: small typed expression evaluator built with GADTs.
* `Fractions`: add fractions using `Data.Ratio`.
* `HarmonicOscillation`: simulate harmonic motion with the `State` monad.
* `Lower`: validate and construct lowercase alphabetic characters.
* `Mod35`: test whether a number is divisible by 3 or 5.
* `MyFilter`: implement `filter` with `foldr`.
* `MyFreeMonad`: arithmetic DSL built with a free monad.
* `MyJson`: decode JSON containing special characters.
* `MyPenultimate`: return the second-to-last list element.
* `MyReverse`: reverse a list with `foldl`, `foldr`, and recursion.
* `MyState`: small custom state type with `get`, `put`, and `modify`.
* `MySum`: sum integers using local mutable state in `ST`.
* `MyTake`: simple `take` reimplementation.
* `MyType`: examples using type applications and `Typeable`.
* `Permutation`: generate permutations with several algorithms.
* `PolyDivisors`: poly-divisible number search used by the executable and
  benchmarks.
* `Qsort`: quicksort implementation.
* `Random`: random values and dice-roll style examples.
* `RecursionSchemes`: examples of anamorphisms, catamorphisms, and related
  folds.
* `RepMax`: replace every list element with the maximum value.
* `SplitList`: split lists and derive related helper functions.
* `Stack`: stack operations layered over `MyState`.
* `STy`: internal singleton-type example for `Bool`, `Int`, and `Maybe`.
* `SubSeqs`: generate subsequences with multiple algorithms.
* `TermFold`: fold with early termination.
* `Trim`: trim whitespace with fold-based implementations.
* `Weekday`: weekday enumeration and string conversion helpers.
* `Yahtzee`: enumerate Yahtzee roll choices.
* `ZipFold`: zip two lists using a fold-based representation.

## ghcid

Notes on using [ghcid](https://github.com/ndmitchell/ghcid).

### Installing ghcid

```bash
cabal update
cabal install ghcid
```

Then copy executable to `$HOME/.local/bin/`.

### Using ghcid

For example to monitor changes for one file [app/Threads.hs](app/Threads.hs)
call:

```bash
ghcid -l -c 'ghci -package stm app/Threads.hs'
```

If no errors, then the screen will report something like:

> All good (1 module, at 21:28:27)

Alternatively, to monitor a couple of files:

```bash
ghcid -l src/Weekday.hs test/WeekdaySpec.hs
```

To monitor the entire project, use:

```bash
ghcid -l
```

### Using ghcid in Vim

To monitor a couple of files (as per above):

```text
:let g:ghcid_args="--lint src/Weekday.hs test/WeekdaySpec.hs"
:GhcidStart
```

To stop, call:

```text
:GhcidStop
```

See also [vim-ghcid](https://github.com/alx741/vim-ghcid) plugin.

## ghcup

When editing using [Visual Studio Code](https://code.visualstudio.com/), use
[GHCup](https://www.haskell.org/ghcup/install/).

To show available software versions:

```bash
ghcup tui
```

To show current installation:

```bash
ghcup list
```

Example output:

```text
$ ghcup list -c installed
   Tool  Version  Tags                      Notes
✔✔ ghc   9.6.7    recommended,base-4.18.3.0 hls-powered
✔✔ cabal 3.12.1.0 recommended
✔✔ hls   2.10.0.0 latest,recommended
✔✔ stack 3.5.1    latest
✔✔ ghcup 0.1.50.2 latest,recommended
```
