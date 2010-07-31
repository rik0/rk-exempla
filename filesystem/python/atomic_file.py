import os
import sys
import tempfile




class AtomicFile(object):
    '''A file-like object where writes are committed only when closed.'''

    def __init__(self, name, mode=None, buffering=None, suffix='', prefix='', dir=None):
        mode = 'r+b' if mode is None else mode
        self.name = name
        self.tempfile = tempfile.NamedTemporaryFile(
            mode=mode, bufsize=-1, suffix=suffix, prefix=prefix,
            dir=dir, delete=False)

    def _swap(self):
        self.tempfile.close()
        os.rename(self.tempfile.name, self.name)

