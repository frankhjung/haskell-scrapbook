# Haskell Scrapbook

A collection of short scripts testing functions and techniques.

## CONTENTS

* [mod35](mod35.hs) - is input modulus 3 or 5?
* [qsort](qsort.hs) - näive implementation of [qsort](https://en.wikipedia.org/wiki/Quicksort)
* [wordcount (haskell)](wordcount.hs) - simple implementation of [wc(1)](https://linux.die.net/man/1/wc) for words only
* [wordcount (python)](wordcount.py) - simple implementation of [wc(1)](https://linux.die.net/man/1/wc) for words only

### Build Python Example

To format source code:

```bash
yapf --style google --parallel -i *.py
```

To lint:

```bash
pylint *.py
```
