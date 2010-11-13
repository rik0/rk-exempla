import os
import sys
import itertools as it

import wordlists
import timeit
import time
import math

from anagrams import words_iterable
    
def yield_anagrams(dictionary, word):
    return [word for word in 
                (''.join(candidate) for candidate in it.permutations(word)) 
                if word in dictionary]
            
def time_yield(min_, max_):
	for i in xrange(min_, max_):
		repetitions = sx
            
            
def main():
    WORDLIST_URL = 'ftp://ftp.ox.ac.uk/pub/wordlists/american/dic-0294.tar.gz'
    try:
        DICT_FILE = sys.argv[1]
    except IndexError:
        DICT_FILE = '2of12.txt'
    start = time.clock()
    with file(DICT_FILE) as fh:
        wordlist = set(words_iterable(fh))
    print 'Read file in: ', time.clock() - start
    print len(wordlist), 'words available.'

    print yield_anagrams(wordlist, 'fighter')
    return wordlist

if __name__ == '__main__':
    print timeit.timeit(main, number=1) 
