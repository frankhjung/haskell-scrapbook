# Haskell Scrapbook

A collection of short scripts testing functions and techniques.

## Documentation

The [Haddock](https://www.haskell.org/haddock/doc/html/index.html) documentation
on [GitHub](https://github.com/frankhjung/haskell-scrapbook) is
[here](https://frankhjung.github.io/haskell-scrapbook/).

## Haskell

Notes on Haskell programs.

### Build

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

Get help PyDoc with:

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
