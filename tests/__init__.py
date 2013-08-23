"""Unit tests

Usage: From project's root directory:
$ python tests

or

$ python -m unittest tests

"""
import unittest
import TestSimpatico

from simpatico import Styler

class TestSimpatico(unittest.TestCase):
    def run_simpatico(self, f):
        """
        Run simpatico over file, supressing output and not outputting to a file
        """
        s = Styler(f, True, False)
        return s
