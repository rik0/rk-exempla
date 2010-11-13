import sys
import itertools as it

import wordlists
import timeit

    
def yield_anagrams(dictionary, word):
    return [candidate for candidate in it.permutations(word) 
            if candidate in dictionary]
def main():
    WORDLIST_URL = 'ftp://ftp.ox.ac.uk/pub/wordlists/american/dic-0294.tar.gz'
    try:
        word = sys.argv[1]
    except IndexError:
        word = 'fighter'
    #dictionary = wordlists.get_wordlist('dictionaries', WORDLIST_URL, None
                                        #predicate=str.isalpha)
    #
    dictionary = {}
    print yield_anagrams(dictionary, word)
    
if __name__ == '__main__':
    print timeit.timeit(main, number=1) 
