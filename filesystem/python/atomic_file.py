# Enrico Franchi (c) 2010
# This module is released under MIT license.

'''Simple module containing a file-like object whose behavior is
to actually save the file only when closed. Before that, data is
stored in a temporary file: thus when creating a file, if another file
with the same name exists that is never substituted with a partial
file.'''

import os
import errno
import tempfile

def make_temp_copy(name, bufsize, suffix, prefix, dir_):
    'x'
    with tempfile.NamedTemporaryFile(mode='r+b', bufsize=bufsize,
                                     suffix=suffix, prefix=prefix,
                                     dir=dir_, delete=False) as tfile:
        try:
            with open(name, 'rb') as fobj:
                tfile.write(fobj.read())
        except IOError, exc:
            if exc.errno == errno.ENOENT:
                pass
    return tfile.name

class AtomicFile(object):
    '''A file-like object where writes are committed only when closed.

    A temporary named file is used in order to store the results.
    On close, the temporary is renamed according to the name parameter.

    AtomicFiles are meant to be used with context managers.'''

    # Unfortunately the TemporaryFile interface uses a dir parameter
    # pylint: disable-msg=R0913
    # pylint: disable-msg=W0622
    def __init__(self, name, mode=None, bufsize=-1,
                 suffix='', prefix='', dir=None):
        self.mode = 'r+b' if mode is None else mode
        self.name = name
        self.tempfile = tempfile.NamedTemporaryFile(
            mode=mode, bufsize=bufsize, suffix=suffix, prefix=prefix,
            dir=dir, delete=False)

    def __enter__(self):
        return self

    def __exit__(self, _exc_type, _exc_val, _exc_tb):
        self.swap()
        return False

    def _create_temporary(self, name, mode, bufsize, suffix, prefix, dir):
        if mode[1] == '+':
            name = make_temp_copy(name, bufsize, suffix, prefix, dir)
            tf = tempfile.NamedTemporaryFile(
                mode=mode, bufsize=bufsize, suffix=suffix, prefix=prefix,
                dir=dir, delete=False)



    def swap(self):
        'Explicitly closes and renames the temporary file.'
        self.tempfile.close()
        os.rename(self.tempfile.name, self.name)
    close = swap

    def write(self, what):
        'Writes a string to the file'
        self.tempfile.write(what)


