from __future__ import with_statement

import os
import sys
import time

import itertools as it

from os import path

def words_iterable(seq, predicate = lambda x: True):
    return (word for word in
            (line.strip() for line in seq) if predicate(word))

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

    def anagram(self, word):
        key = self.make_key(word)
        return tuple(self.indexed_words[key])

    def anagrams(self):
        return [anagrams for key, anagrams in self.indexed_words.iteritems()
                    if len(anagrams) > 1]

    def stats(self, out=sys.stdout):
        def word_length(al):
            return len(al[0])
        anagrams_list = self.anagrams()
        longest_anagram = max(anagrams_list, key=word_length)
        word_with_max_anagrams = max(anagrams_list, key=len)

        print >>out, 'Number of anagrams:', len(anagrams_list)
        print >>out, 'Greatest number of anagrams:', word_with_max_anagrams,
        print >>out, len(word_with_max_anagrams)
        print >>out, 'Longest anagram:', longest_anagram, len(longest_anagram[0])

        print >>out
        print >>out,  'Anagrams by number of anagrams'

        for key, group_it in it.groupby(sorted(anagrams_list, key=len), len):
            print key, len(list(group_it))

        print >>out
        print >>out,  'Anagrams by anagram length'

        for key, group_it in it.groupby(sorted(anagrams_list, key=word_length),
                                        word_length):
            print key, len(list(group_it))

def main():
    try:
        DICT_FILE = sys.argv[1]
    except IndexError:
        DICT_FILE = '2of12.txt'
    start = time.time()
    with file(DICT_FILE) as fh:
        wordlist = set(words_iterable(fh))
    print 'Read file in: ', time.time() - start
    print len(wordlist), 'words available.'

    start = time.time()
    anagr = Anagrams(wordlist)
    print anagr.anagram('fighter')
    print time.time() - start

    anagr.stats()
    return anagr

if __name__ == '__main__':
    main()



