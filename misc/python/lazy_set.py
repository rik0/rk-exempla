# Copyright (C) 2011 by Enrico Franchi
#
# This file is released under the terms of the MIT license
# http://www.opensource.org/licenses/mit-license.php

import itertools as it


class lazy_set(object):
    """
    Trace of implementation of lazy sets in Python.


    >>> s = lazy_set(xrange(1, 4))

    >>> sorted(list(s))
    [1, 2, 3]

    >>> sorted(list(s))
    [1, 2, 3]

    >>> s.add(4)
    >>> sorted(list(s))
    [1, 2, 3, 4]

    >>> s.update(2*x for x in range(10))
    >>> sorted(list(s))
    [0, 1, 2, 3, 4, 6, 8, 10, 12, 14, 16, 18]
    """
    def __init__(self, input_=[]):
        self.input_ = iter(input_)
        self.seen = set()

    def _iter(self):
        for element in self.input_:
            if element in self.seen:
                continue
            else:
                yield element
                self.seen.add(element)

    def add(self, obj):
        self.seen.add(obj)

    def update(self, iterator):
        self.input_ = it.chain(self.input_, iterator)

    def __iter__(self):
        return it.chain(self.seen, self._iter())

if __name__ == "__main__":
    import doctest
    doctest.testmod()
