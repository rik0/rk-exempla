import itertools as it
import wordlists
import lev

WORDS = wordlists.get_wordlist('dictionaries', wordlists.WORDLIST_URL, 
                            lengths=(6, 7, 8), predicate=str.isalpha)

WORD = 'fighter'
assert WORD in WORDS
print len(WORDS)

for w in WORDS:
    if lev.fast_lev(WORD, w) <= 1:
        print w
