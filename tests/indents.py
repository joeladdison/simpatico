import unittest
from base import TestSimpatico

class TestIndents(TestSimpatico):

    def test_indents(self):
        expected_error_lines = [5, 8, 12, 16, 21, 26, 37, 38, 40, 50, 53, 58,
                72,75]

        f = 'tests/files/indents.c'
        s = self.run_simpatico(f)
        indent_errors = len(s.errors.indent_d.keys())
        found_error_lines = set(s.errors.indent_d.keys())

        self.assertEqual(indent_errors, len(expected_error_lines))
        self.assertSetEqual(found_error_lines, set(expected_error_lines))


if __name__ == "__main__":
    unittest.main()
