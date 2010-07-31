import unittest
import tempfile
import atomic_file

import os
import time

from os import path

class TestAtomicFile(unittest.TestCase):
    def setUp(self):
        self.directory = path.dirname(__file__)
        self.name = tempfile.mktemp()

    def tearDown(self):
        try:
            os.remove(self.name)
        except OSError:
            pass

    def testCreate(self):
        af = atomic_file.AtomicFile(name=self.name, dir=self.directory)
        self.assert_(not path.exists(self.name))
        self.assert_(path.exists(path.join(self.directory,
                                           af.tempfile.name)))
        af.swap()
        self.assert_(path.exists(self.name))
        self.assert_(not path.exists(path.join(self.directory,
                                           af.tempfile.name)))
    def testClose(self):
        af = atomic_file.AtomicFile(name=self.name, dir=self.directory)
        self.assert_(not path.exists(self.name))
        self.assert_(path.exists(path.join(self.directory,
                                           af.tempfile.name)))
        af.close()
        self.assert_(path.exists(self.name))
        self.assert_(not path.exists(path.join(self.directory,
                                           af.tempfile.name)))

    def testContext(self):
        with atomic_file.AtomicFile(name=self.name, dir=self.directory) as af:
            self.assert_(not path.exists(self.name))
            self.assert_(path.exists(path.join(self.directory,                                               af.tempfile.name)))
        self.assert_(path.exists(self.name))
        self.assert_(not path.exists(path.join(self.directory,
                                           af.tempfile.name)))

    def testWrite(self):
        with atomic_file.AtomicFile(name=self.name, dir=self.directory) as af:
            text = 'THE TEXT\n'
            af.write(text)
        self.assertEqual(file(self.name).read(), text)

    def testMoreWrite(self):
        with atomic_file.AtomicFile(name=self.name, dir=self.directory) as af:
            lines = ['THE TEXT', 'MORE TEXT', 'AGAIN!']
            for line in lines:
                print >> af, line
        self.assertEqual(file(self.name).read(), '\n'.join(lines) + '\n')

    def hasExplosion(self):
        with atomic_file.AtomicFile(name=self.name, dir=self.directory) as af:
            raise RuntimeError()
        self.assert_(not path.exists(self.name))
        self.assert_(not path.exists(path.join(self.directory,
                                               af.tempfile.name)))
    def testBoom(self):
        self.assertRaises(RuntimeError, self.hasExplosion)

    def testDifferentDirectory(self):
        tmpdir = tempfile.gettempdir()
        af = atomic_file.AtomicFile(name=self.name, dir=tmpdir)
        self.assert_(not path.exists(self.name))
        self.assert_(path.exists(path.join(tmpdir,
                                           af.tempfile.name)))
        af.swap()
        self.assert_(path.exists(self.name))
        self.assert_(not path.exists(path.join(tmpdir,
                                           af.tempfile.name)))

    def testDifferentDirectory2(self):
        self.name = path.join('..', 'here' + str(time.time))
        af = atomic_file.AtomicFile(name=self.name, dir=self.directory)
        self.assert_(not path.exists(self.name))
        self.assert_(path.exists(path.join(self.directory,
                                           af.tempfile.name)))
        af.swap()
        self.assert_(path.exists(self.name))
        self.assert_(not path.exists(path.join(self.directory,
                                           af.tempfile.name)))

