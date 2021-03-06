# Haskell Scrapbook

A collection of short scripts testing functions and techniques.

The project is built using [Cabal](https://www.haskell.org/cabal/). To
coordinate build the various task I use [GNU
Make](https://www.gnu.org/software/make/).

## Links

[Haddock](https://www.haskell.org/haddock/doc/html/index.html) API
documentation is available on:

* [GitHub](https://frankhjung.github.io/haskell-scrapbook/)
* [GitLab](https://frankhjung1.gitlab.io/haskell-scrapbook/)

## Haskell

These examples are meant to be run using
[runhaskell(1)](https://manpages.debian.org/buster/ghc/runhaskell.1.html) or
[runghc](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/runghc.html).

The reason they won't link is because I've added them to their own module, and
have not yet gotten around to build an über main yet. The idea was really to
test an run code snippets quickly without having to bother with compiling and
linking a main module.

The compilation using make is to validate the source files.

### Build

Build using GNU Make:

```bash
make build
```

### Check

To only perform code checks, run:

```bash
make check
```

This runs `tags`, `style` and `lint`:

```bash
SRC=$(find * -name "*.hs")
hasktags --ctags --extendedctag $SRC
stylish-haskell --config=.stylish-haskell.yaml --inplace $SRC
cabal check
hlint --cross --color --show $SRC
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

### Performance Benchmarks

The
[Criterion](https://hackage.haskell.org/package/criterion/docs/Criterion.html)
benchmark HTML reports can be generated using
[stack](https://docs.haskellstack.org/en/stable/README/). They are available
from GitHub, here:

* [Criterion benchmarks
  * [MyReverse](https://frankhjung.github.io/haskell-scrapbook/benchmark-myreverse.html)
  * [PolyDivisors](https://frankhjung.github.io/haskell-scrapbook/benchmark-polydivisors.html)
  * [RepMax](https://frankhjung.github.io/haskell-scrapbook/benchmark-repmax.html)
  * [SubSeqs](https://frankhjung.github.io/haskell-scrapbook/benchmark-subseqs.html)
  * [ZipFold](https://frankhjung.github.io/haskell-scrapbook/benchmark-zipfold.html)

### API Documentation

To generate [Haddock](https://www.haskell.org/haddock/doc/html/) for source:

```bash
cabal haddock --haddock-quickjump --haddock-hyperlink-source
```

## Word Count in Python

I've also included a Python equivalent to word count program.

Get help [PyDoc](https://docs.python.org/3/library/pydoc.html) with:

```bash
pydoc3 wordcount.py
pydoc3 wordcount.word_count
```

The later produces a short function summary:

```text
$ pydoc3 wordcount.word_count
Help on function word_count in wordcount:

wordcount.word_count = word_count(stream: 'file') -> 'int'
    Count words in stream.
```

### Python Build

To format source code:

```bash
yapf --style google --parallel -i *.py
```

To lint:

```bash
pylint *.py
```

## Python Test

To test program run:

```bash
cat wordcount.hs | python3 ./wordcount.py
```

This should return `38`. That is, this should return `PASS`:

```bash
export f=wordcount.hs
cat $f | python3 ./wordcount.py | (read count ; test $count -eq $(wc -w $f | cut -d ' ' -f1 -) && echo "PASS")
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

For example to monitor changes to a file [app/Threads.hs](app/Threads.hs) call:

```bash
ghcid --lint --command 'ghci app/Threads.hs'
ghcid -lc 'ghci app/Threads.hs'
```

If no errors, then the screen will report something like:

<span style="color:green">All good</span> (1 module, at 21:28:27)

