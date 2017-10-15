import unittest
from base import TestSimpatico

class TestWhitespace(TestSimpatico):

    def test_whitespace(self):
        expected_error_lines = [1,4,6,7,9,12,19,21,24,25,26,28,31,33,40,41,
                42,43,46,48,50,52,54,56,58,60,67,69,73,75,77,84,85,87,89,90,
                91,94,97,101,102,104,110,115,119,120,125,127,133,137,138,143]

        f = 'tests/files/whitespace.c'
        s = self.run_simpatico(f)
        whitespace_errors = len(s.errors.whitespace_d.keys())
        found_error_lines = set(s.errors.whitespace_d.keys())

        self.assertEqual(whitespace_errors, len(expected_error_lines))
        self.assertSetEqual(found_error_lines, set(expected_error_lines))


if __name__ == "__main__":
    unittest.main()
