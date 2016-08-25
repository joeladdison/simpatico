from __future__ import absolute_import
import unittest

from .naming import *
from .indents import *
from .length import *
from .braces import *
from .whitespace import *
from .overall import *

from simpatico import Styler

class TestSimpatico(unittest.TestCase):
    def run_simpatico(self, f):
        """
        Run simpatico over file, supressing output and not outputting to a file
        """
        s = Styler(f, True, False)
        return s

unittest.main()
