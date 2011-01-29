import re
import sys
import collections

def yield_words(iterable):
    for line in iterable:
        for word in re.finditer('\w+', line):
            yield word.group(0)

import itertools as it
import functools as ft

def yield_words(iterable):
    return (word.group(0) for word in
            ft.reduce(it.chain, 
                      (re.finditer('\w+', line) for line in iterable)))
    
count = collections.defaultdict(lambda: 0)
for word in yield_words(sys.stdin):
    count[word] += 1
 
for word, occurrencies in count.iteritems():
    print word, ':', occurrencies
        