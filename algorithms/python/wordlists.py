import os
import sys
import time 

import urllib
import tarfile

import random

import itertools as it

from os import path

class DownloadReport(object):
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
