import os
import sys
import tempfile




class AtomicFile(object):
    '''A file-like object where writes are committed only when closed.

    A temporary named file is used in order to store the results.
    On close, the temporary is renamed according to the name parameter.

    AtomicFiles are meant to be used with context managers.'''
    def __init__(self, name, mode=None, bufsize=-1, suffix='', prefix='', dir=None):
        mode = 'r+b' if mode is None else mode
        self.name = name
        self.tempfile = tempfile.NamedTemporaryFile(
            mode=mode, bufsize=bufsize, suffix=suffix, prefix=prefix,
            dir=dir, delete=False)

    def __enter__(self):
        return self

    def __exit__(self, exc_type, exc_val, exc_tb):
        self._swap()
        return False

    def swap(self):
        'Explicitly closes and renames the temporary file.'
        self.tempfile.close()
        os.rename(self.tempfile.name, self.name)
    close = swap


    def write(self, what):
        'Writes a string to the file'
        self.tempfile.write(what)


