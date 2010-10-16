import sys
import iterators as it

import wordlists


    
def yield_anagrams(dictionary, word):
    return [candidate for candidate in it.permutations(word) 
            if candidate in dictionary]
def main():
    try:
        word = sys.args[1]
    except IndexError:
        word = 'fighter'
    dictionary = wordlists.get_wordlist('dictionaries', wordlists.WORDLIST_URL,
                                        predicate=str.isalpha)
    
