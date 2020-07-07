# Haskell Scrapbook

A collection of short scripts testing functions and techniques.

## Documentation

[Haddock](https://www.haskell.org/haddock/doc/html/index.html) API
documentation is available on:

* [GitHub](https://frankhjung.github.io/haskell-scrapbook/)
* [GitLab](https://frankhjung1.gitlab.io/haskell-scrapbook)

## Haskell

These examples are meant to be run using
[runhaskell(1)](https://www.commandlinux.com/man-page/man1/runhaskell.1.html) or
[runghc(1)](https://manpages.debian.org/stable/ghc/runghc.1.en.html).

The reason they won't link is because I've added them to their own module, and
have not yet gotten around to build an über main yet. The idea was really to
test an run code snippets quickly without having to bother with compiling and
linking a main module.

The compilation using make is to validate the source files.

### Check

Build using GNU Make:

```bash
make [target]
```

To only perform code checks, run:

```bash
make check
```

This is the default goal.

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

This should return `38`. i.e. This should return `PASS`:

```bash
export f=wordcount.hs
cat $f | python3 ./wordcount.py | (read count ; test $count -eq $(wc -w $f | cut -d ' ' -f1 -) && echo "PASS")
```

## Literate Haskell

To print [bird.lhs](./bird.lhs) to PDF, use:

```bash
pandoc -r markdown+lhs -s bird.lhs --css haskell.css -o bird.pdf
```

To render to HTML use:

```bash
make bird.html
```

## Haskell Notebook

Included is a Jupyter Notebook with a
[Haskell](https://github.com/gibiansky/IHaskell) runtime.

To start run this:

```bash
stack exec jupyter -- notebook
```

For more details see [IHaskell](https://github.com/gibiansky/IHaskell).
