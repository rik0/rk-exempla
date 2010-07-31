import unittest
import atomic_file

import os
from os import path

class TestAtomicFile(unittest.TestCase):
    def setUp(self):
        self.directory = path.dirname(__file__)

    def testFalse(self):
        self.assert_(False)