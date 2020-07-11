#!/usr/bin/env python3

"""
Word count as per wc(1).

Count words from file read from stdin.

To run:

    $ cat wordcount.hs | python3 ./wordcount.py
    38
"""

import sys
from functools import reduce


def word_count(stream: 'file') -> 'int':
    """
    Count words from STDIN IO stream.
    """
    return reduce(lambda x, y: x + y, list(map(lambda x: len(x.split()),
                                               stream)))


if __name__ == '__main__':
    print(word_count(sys.stdin.readlines()))

