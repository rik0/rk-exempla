import os
import sys
import time 

import numpy as np

from os import path
import itertools as it


import wordlists

def functional_lev(a, b):
    if not a: return len(b)
    if not b: return len(a)
    return min(functional_lev(a[1:], b[1:])+(a[0] != b[0]), 
               functional_lev(a[1:], b)+1, 
               functional_lev(a, b[1 :])+1)


def fast_lev(a, b):
    m = len(a) 
    n = len(b)
    distance = np.zeros((m+1, n+1), dtype=int)
    distance[0,::] = np.arange(n+1)
    distance[::,0] = np.arange(m+1)
    
    for i, ca in enumerate(a, start=1):
        for j, cb in enumerate(b, start=1):
            if ca == cb:
                distance[i,j] = distance[i-1,j-1]
            else:
                distance[i,j] = min(
                    distance[i-1,j] + 1,
                    distance[i,j-1] + 1,
                    distance[i-1,j-1] + 1
                )
    return distance[m,n]
               
    
def user_loop():
    while 1:
        try:
            line = raw_input('> ')
            w1, w2 = line.split(',')
        except KeyboardInterrupt:
            print 'QUIT'
        else:
            w1 = w1.strip()
            w2 = w2.strip()
            print "'%s'-'%s'" % (w1, w2),
            print lev(w1, w2), 
            print fast_lev(w1, w2)
                        
if __name__ == '__main__':
    DICT_COMPONENT = 'dictionaries'
    try:
        DICTIONARY = sys.argv[1]
    except IndexError:
        DICTIONARY = path.join(os.getcwd(), DICT_COMPONENT)
    WORDLIST_URL = 'ftp://ftp.ox.ac.uk/pub/wordlists/american/dic-0294.tar.gz'
    
    start = time.clock()
    wordlist = wordlists.get_wordlist(DICTIONARY, WORDLIST_URL, 5, 
                            lengths=[3, 5, 7, 8, 10, 11, 13, 14, 20])
    print 'Got wordlist in %s seconds.' % (time.clock() - start)
    print '%d words available.' % len(wordlist) 
    start = time.clock()
    for w1, w2 in it.product(wordlist, wordlist):
        fast_lev(w1, w2)
    print 'Computed %d fast_lev in %s seconds.' % (len(wordlist)**2, 
                                                   time.clock() - start)
        
    start = time.clock()
    for w1, w2 in it.product(wordlist, wordlist):
        functional_lev(w1, w2)
    print 'Computed %d functional_lev in %s seconds.' % (len(wordlist)**2, 
                                                         time.clock() - start)
