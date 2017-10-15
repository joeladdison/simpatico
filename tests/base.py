import unittest
from simpatico import Styler

class TestSimpatico(unittest.TestCase):

    def run_simpatico(self, f, include_paths=()):
        """
        Run simpatico over file, supressing output and not outputting to a file
        """
        s = Styler(f, quiet=True, output_file=False,
                   include_paths=include_paths)
        return s
