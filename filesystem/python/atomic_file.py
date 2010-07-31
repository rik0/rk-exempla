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

    def __enter__(self):
        return self

    def __exit__(self, exc_type, exc_val, exc_tb):
        self._swap()
        return False


    def _swap(self):
        self.tempfile.close()
        os.rename(self.tempfile.name, self.name)



    def write(self, what):
        'Writes a string to the file'
        self.tempfile.write(what)


