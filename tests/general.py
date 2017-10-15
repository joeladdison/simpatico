import unittest
from base import TestSimpatico

class TestGeneral(TestSimpatico):

    def test_library_include(self):
        includes = ['tests/files/libinclude']
        f = 'tests/files/libraryDependency.c'
        s = self.run_simpatico(f, includes)


if __name__ == "__main__":
    unittest.main()
