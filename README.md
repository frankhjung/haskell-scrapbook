# Haskell Scrapbook

A collection of short scripts testing functions and techniques.

## CONTENTS

* [mod35](mod35.hs) - is input modulus 3 or 5?
* [qsort](qsort.hs) - näive implementation of [qsort](https://en.wikipedia.org/wiki/Quicksort)
* [wordcount (haskell)](wordcount.hs) - simple implementation of [wc(1)](https://linux.die.net/man/1/wc) for words only
* [wordcount (python)](wordcount.py) - simple implementation of [wc(1)](https://linux.die.net/man/1/wc) for words only

## Python

I've also included a Python equivalent to word count program.

Get help PyDoc with:

```bash
pydoc3 wordcount.py
pydoc3 wordcount.word_count
```

The later produces a short function summary:

```
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

