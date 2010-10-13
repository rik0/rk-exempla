import os
import sys
import time

import urllib
import tarfile

import random

import itertools as it

from os import path

WORDLIST_URL = 'ftp://ftp.ox.ac.uk/pub/wordlists/american/dic-0294.tar.gz'

class DownloadReport(object):
    '''Helper class to be used as urlretrieve hook.'''
    def __init__(self):
        self.downloaded = 0
    def __call__(self, blocks, block_size, total_size):
        self.downloaded += block_size
        if blocks % 10 == 0:
            sys.stdout.write("Downloading: %d/%d\r" % (self.downloaded, total_size))
            sys.stdout.flush()
    def __del__(self):
        print

def get_dictionary_file(url, pathname):
    '''Download tarfile from url into a file named according to url basename
    and expands the file into pathname.'''
    filename = path.basename(url)
    if not path.isdir(pathname):
        os.mkdir(pathname)
    filepath = path.join(pathname, filename)
    urllib.urlretrieve(url, filename=filepath,
                       reporthook=DownloadReport())
    tararchive = tarfile.open(filepath)
    tararchive.extractall(pathname)

def check_dictionary_file(pathname):
    '''Check if the dictionary file has been expanded into pathname'''
    return (path.isdir(pathname) and
            path.isfile(path.join(pathname, 'length02.txt')))

def parts_array(max_sum, no_parts):
    """Return an array A such that:
       * sum(A) == max_sum
       * len(A) == no_parts
       * max(A) - min(A) <= 1
    """
    parts = [None, ] * no_parts
    if max_sum:
        min_item, has_rest = divmod(max_sum, no_parts)
        for i in xrange(no_parts):
            if has_rest:
                parts[i] = min_item + 1
                has_rest -= 1
            else:
                parts[i] = min_item
    return parts

def read_all(dict_dir, no_words, min_length, max_length, lengths, predicate):
    '''Read words from a dictionary.

    dict_dir is the directory where the dictionary is located
    no_words is the maximum number of words (or None for all) to be read
    min_length/max_length specify the range of the lengths of words to be read
    alternatively, lengths is an iterable containing the lengths (3, 4, 7)
    only words which make predicate true are returned
    '''
    if not lengths:
        lengths = set(range(min_length, max_length+1))
    else:
        lengths = set(lengths)
    words = []
    requested_words = parts_array(no_words, len(lengths))
    for n, no_requested_words in it.izip(lengths, requested_words):
        filename = "length%0.2d.txt" % n
        full_path = path.join(dict_dir, filename)
        more_words = read_file(full_path, no_requested_words, predicate)
        words.extend(more_words)
    return words

def read_file(filename, no_words, predicate):
    '''Read at most no_words satisfying predicate from filename.

    If no_words > of the number of words in filename, all words are returned.
    If no_words is None, all words are returned.'''
    with open(filename) as fh:
        all_words = fh.read().split('\n')
        filtered_words = filter(predicate, (w.strip() for w in all_words))
        if no_words and no_words < len(filtered_words):
            selected_words = random.sample(filtered_words, no_words)
        else:
            selected_words = filtered_words
        return selected_words

def get_wordlist(pathname, backup_url, no_words=None, min_length=2,
                 max_length=32, lengths=None, predicate=(lambda x: True)):
    '''Return a list of words.

    pathname is the place where the dictionary should be stored
    backup_url is used to download the dictinary to pathname if not present
    the other arguments have the same meaning than in read_all
    '''
    if not check_dictionary_file(pathname):
        get_dictionary_file(backup_url, pathname)
    return read_all(pathname, no_words, min_length,
                    max_length, lengths, predicate)
