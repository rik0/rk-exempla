import os, time
from os import path

import wordlists

def create_key(word):
    return frozenset(word)


class Anagrams(object):
    def __init__(self, dictionary):
        self.dictionary = dictionary
        self.indexed_words = {}
        self.preprocess()

    def make_key(self, word):
        sorted_word = list(word)
        sorted_word.sort()
        return tuple(sorted_word)

    def preprocess(self):
        for word in self.dictionary:
            key = self.make_key(word)
            self.indexed_words.setdefault(key, []).append(word)

    def anagrams(self, word):
        key = self.make_key(word)
        return tuple(self.indexed_words[key])


def main(lengths):
    DICT_COMPONENT = 'dictionaries'
    DICTIONARY = path.join(os.getcwd(), DICT_COMPONENT)
    WORDLIST_URL = 'ftp://ftp.ox.ac.uk/pub/wordlists/american/dic-0294.tar.gz'

    wordlist = wordlists.get_wordlist(DICTIONARY, WORDLIST_URL, lengths=lengths,
                                      predicate=str.isalpha)

    start = time.clock()
    anagr = Anagrams(wordlist)
    print anagr.anagrams('fighter')
    print time.clock() - start