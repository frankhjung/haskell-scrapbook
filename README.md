# Haskell Scrapbook

A collection of short scripts testing functions and techniques.

## Documentation

[Haddock](https://www.haskell.org/haddock/doc/html/index.html) API
documentation is available on:

* [GitHub](https://frankhjung.github.io/haskell-scrapbook/)
  * Criterion benchmarks
    * [Multiply](https://frankhjung.github.io/haskell-scrapbook/benchmark-multiply.html)
    * [MyReverse](https://frankhjung.github.io/haskell-scrapbook/benchmark-myreverse.html)
    * [PolyDivisors](https://frankhjung.github.io/haskell-scrapbook/benchmark-polydivisors.html)
    * [RepMax](https://frankhjung.github.io/haskell-scrapbook/benchmark-repmax.html)
    * [SubSeqs](https://frankhjung.github.io/haskell-scrapbook/benchmark-subseqs.html)
    * [ZipFold](https://frankhjung.github.io/haskell-scrapbook/benchmark-zipfold.html)
* [GitLab](https://frankhjung1.gitlab.io/haskell-scrapbook/)
  * Criterion benchmarks
    * [Multiply](https://frankhjung1.gitlab.io/haskell-scrapbook/benchmark-multiply.html)
    * [MyReverse](https://frankhjung1.gitlab.io/haskell-scrapbook/benchmark-myreverse.html)
    * [PolyDivisors](https://frankhjung1.gitlab.io/haskell-scrapbook/benchmark-polydivisors.html)
    * [RepMax](https://frankhjung1.gitlab.io/haskell-scrapbook/benchmark-repmax.html)
    * [SubSeqs](https://frankhjung1.gitlab.io/haskell-scrapbook/benchmark-subseqs.html)
    * [ZipFold](https://frankhjung1.gitlab.io/haskell-scrapbook/benchmark-zipfold.html)

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

This is also the default goal.

### Test

Test using GNU Make:

```bash
make test
```

To generate test coverage use:

```bash
stack test --coverage
```

### Documentation

To generate [Haddock](https://www.haskell.org/haddock/doc/html/) for source:

```bash
stack haddock --no-rerun-tests --no-reconfigure
```

Which is almost the same as:

```bash
stack exec -- haddock src/*.hs --odir=public --html
```

Where the later does not produce a full function cross-reference.

## Literate Haskell

To render [bird.lhs](./bird.lhs) into a PDF, use:

```bash
pandoc -r markdown+lhs -s bird.lhs --css haskell.css -o bird.pdf
```

Or

```bash
make doc/bird.pdf
```

To render to HTML use:

```bash
make doc/bird.html
```

## Haskell Notebook

Included is a Jupyter Notebook with a
[Haskell](https://github.com/gibiansky/IHaskell) runtime.

To start run this:

```bash
stack exec jupyter -- notebook
```

For more details see [IHaskell](https://github.com/gibiansky/IHaskell).

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

### Build

To format source code:

```bash
yapf --style google --parallel -i *.py
```

To lint:

```bash
pylint *.py
```

## Test

To test program run:

```bash
cat wordcount.hs | python3 ./wordcount.py
```

This should return `38`. That is, this should return `PASS`:

```bash
export f=wordcount.hs
cat $f | python3 ./wordcount.py | (read count ; test $count -eq $(wc -w $f | cut -d ' ' -f1 -) && echo "PASS")
```
