# Enrico Franchi (c) 2010
# This module is released under MIT license.

'''Simple module containing a file-like object whose behavior is
to actually save the file only when closed. Before that, data is
stored in a temporary file: thus when creating a file, if another file
with the same name exists that is never substituted with a partial
file.'''

import os
import tempfile

class AtomicFile(object):
    '''A file-like object where writes are committed only when closed.

    A temporary named file is used in order to store the results.
    On close, the temporary is renamed according to the name parameter.

    AtomicFiles are meant to be used with context managers.'''

    # Unfortunately the TemporaryFile interface uses a dir parameter
    # pylint: disable-msg=R0913
    # pylint: disable-msg=W0622
    def __init__(self, name, bufsize=-1, suffix='', prefix='', dir=None):
        mode = 'r+b'
        self.name = name
        self.tempfile = tempfile.NamedTemporaryFile(
            mode=mode, bufsize=bufsize, suffix=suffix, prefix=prefix,
            dir=dir, delete=False)

    def __enter__(self):
        return self

    def __exit__(self, _exc_type, _exc_val, _exc_tb):
        self.swap()
        return False

    def swap(self):
        'Explicitly closes and renames the temporary file.'
        self.tempfile.close()
        os.rename(self.tempfile.name, self.name)
    close = swap


    def write(self, what):
        'Writes a string to the file'
        self.tempfile.write(what)


