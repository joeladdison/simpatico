import unittest
from base import TestSimpatico

class TestLineLength(TestSimpatico):

    def test_lines(self):
        expected_error_lines = [2, 3, 16, 21]

        f = 'tests/files/length.c'
        s = self.run_simpatico(f)
        error_count = len(s.errors.line_length_d.keys())
        found_error_lines = set(s.errors.line_length_d.keys())

        self.assertEqual(error_count, len(expected_error_lines))
        self.assertSetEqual(found_error_lines, set(expected_error_lines))


if __name__ == "__main__":
    unittest.main()
