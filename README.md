# Haskell Scrapbook

![Haskell workflow](https://github.com/frankhjung/haskell-scrapbook/actions/workflows/haskell.yml/badge.svg)

A collection of short scripts testing functions and techniques.

The project is built using [Cabal](https://www.haskell.org/cabal/).

To coordinate various build tasks I use
[GNU Make](https://www.gnu.org/software/make/).

## Links

[Haddock](https://www.haskell.org/haddock/doc/html/index.html) API
documentation is available on:

* [GitHub](https://frankhjung.github.io/haskell-scrapbook/)
* [GitLab](https://frankhjung1.gitlab.io/haskell-scrapbook/)

(See also benchmark reports below.)

## Haskell

These examples are meant to be run using
[runhaskell(1)](https://manpages.debian.org/buster/ghc/runhaskell.1.html) or
[runghc](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/runghc.html).

The reason they won't link is because I've added them to their own module, and
have not yet gotten around to build an über main yet. The idea was really to
test an run code snippets quickly without having to bother with compiling and
linking a main module.

The build uses GNU make to check source files.

### Build

Use a local Cabal project profile, `cabal.project` with:

```config
packages: scrapbook.cabal
with-compiler: /home/frank/.ghcup/bin/ghc-8.10.7
```

This will use the specified GHC, which can be different from the system version.

Build using GNU Make:

```bash
make setup default
```

### Check

To only perform code checks, run:

```bash
make check
```

This runs `tags`, `style` and `lint`:

```bash
SRC=$(find * -name '*.hs')
hasktags --ctags --extendedctag ${SRC}
stylish-haskell --config=.stylish-haskell.yaml --inplace ${SRC}
cabal check --verbose
hlint --cross --color --show ${SRC}
```

### Unit Tests

Test using GNU Make:

```bash
make test
```

This runs:

```bash
cabal test --test-show-details=always
```

To re-run a failed test, call:

```bash
cabal test --test-show-details=direct --test-option=--match --test-option='/Weekday/test weekday type/capitised head of string/'
```

### Performance Benchmarks

The
[Criterion](https://hackage.haskell.org/package/criterion/docs/Criterion.html)
benchmark HTML reports can be generated using
[stack](https://docs.haskellstack.org/en/stable/README/). They are available
from GitHub, here:

* Criterion benchmarks:
  * [MonTransBench](https://frankhjung.github.io/haskell-scrapbook/benchmark-monTransBench.html)
  * [MyFilter](https://frankhjung.github.io/haskell-scrapbook/benchmark-myfilter.html)
  * [MyReverse](https://frankhjung.github.io/haskell-scrapbook/benchmark-myreverse.html)
  * [PolyDivisors](https://frankhjung.github.io/haskell-scrapbook/benchmark-polydivisors.html)
  * [RecursionSchemes](https://frankhjung.github.io/haskell-scrapbook/benchmark-recursionschemes.html)
  * [RepMax](https://frankhjung.github.io/haskell-scrapbook/benchmark-repmax.html)
  * [SubSeqs](https://frankhjung.github.io/haskell-scrapbook/benchmark-subseqs.html)
  * [ZipFold](https://frankhjung.github.io/haskell-scrapbook/benchmark-zipfold.html)

To run individual benchmark:

```bash
cabal bench PolyDivisorsBench
```

Individual benchmarks can be reported as well by calling the benchmark
executable and providing an output file. For example:

```bash
dist-newstyle/build/x86_64-linux/ghc-8.8.4/scrapbook-0.1.0/b/myfilterBench/build/myfilterBench/myfilterBench --output myfilter.html
```

### API Documentation

To generate [Haddock](https://www.haskell.org/haddock/doc/html/) for source:

```bash
cabal haddock --haddock-quickjump --haddock-hyperlink-source
```

## ghcid

Notes on using [ghcid](https://github.com/ndmitchell/ghcid).

### Installing ghcid

```bash
cabal update
cabal install ghcid
```

Then copy executable to `$HOME/.loca/bin/`.

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
[ Warn  ] New ghc version available. If you want to install this latest version, run 'ghcup install ghc 9.6.2'
[ Warn  ] New cabal version available. If you want to install this latest version, run 'ghcup install cabal 3.10.1.0'
[ Warn  ] New stack version available. If you want to install this latest version, run 'ghcup install stack 2.11.1'
   Tool  Version  Tags               Notes
✔✔ ghc   9.0.2    base-4.15.1.0      hls-powered
✔✔ cabal 3.6.2.0  recommended
✓  hls   1.10.0.0 recommended
✔✔ hls   2.0.0.0  latest
✓  stack 2.9.1
✔✔ stack 2.9.3    recommended
✔✔ ghcup 0.1.19.2 latest,recommended
```
