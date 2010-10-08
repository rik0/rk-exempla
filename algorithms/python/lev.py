import os
import sys
import time 

import urllib
import tarfile

import random

import itertools as it
import numpy as np

from os import path

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
    
class DownloadReport(object):
    def __init__(self):
        self.downloaded = 0
    def __call__(self, blocks, block_size, total_size):
        self.downloaded += block_size
        if blocks % 10 == 0:
            print r"\r%d/%d" % (self.downloaded, total_size)
    def __del__(self):
        print

def get_dictionary_file(url, pathname):
    filename = path.basename(url)
    if not path.isdir(pathname):
        os.mkdir(pathname)
    filepath = path.join(pathname, filename)
    urllib.urlretrieve(url, filename=filepath,
                       reporthook=DownloadReport())
    tararchive = tarfile.open(filepath)
    tararchive.extractall(pathname)
    
def check_dictionary_file(pathname):
    return (path.isdir(pathname) and
            path.isfile(path.join(pathname, 'length02.txt')))
           
    
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
            
def read_all(dict_dir, no_words, min_length, max_length, lengths):
    if not lengths:
        lengths = set(range(min_length, max_length+1))
    else:
        lengths = set(lengths)
    words_per_lengths, has_rest = divmod(no_words, len(lengths))
    words = []
    for n in lengths:
        filename = "length%0.2d.txt" % n
        full_path = path.join(dict_dir, filename)
        if has_rest:
            no_requested_words = words_per_lengths + 1 
            has_rest -= 1
        else: 
            no_requested_words = words_per_lengths
        more_words = read_file(full_path, no_requested_words)
        words.extend(more_words)
    return words
        
def read_file(filename, no_words):
    with open(filename) as fh:
        all_words = fh.read().split('\n')
        selected_words = random.sample(all_words, no_words)
        return [w.strip() for w in selected_words]
    
def get_wordlist(pathname, backup_url, no_words, min_length=2, max_length=32, lengths=None):
    if not check_dictionary_file(pathname):
        get_dictionary_file(backup_url, pathname)
    return read_all(pathname, no_words, min_length, max_length, lengths)
            
if __name__ == '__main__':
    DICT_COMPONENT = 'dictionaries'
    try:
        DICTIONARY = sys.argv[1]
    except IndexError:
        DICTIONARY = path.join(os.getcwd(), DICT_COMPONENT)
    WORDLIST_URL = 'ftp://ftp.ox.ac.uk/pub/wordlists/american/dic-0294.tar.gz'
    
    start = time.clock()
    wordlist = get_wordlist(DICTIONARY, WORDLIST_URL, 5, 
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
