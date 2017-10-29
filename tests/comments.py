import unittest
from base import TestSimpatico

class TestComments(TestSimpatico):

    def test_comments(self):
        expected_error_lines = [23]

        f = 'tests/files/comments.c'
        s = self.run_simpatico(f)
        error_count = len(s.errors.comments_d.keys())
        found_error_lines = set(s.errors.comments_d.keys())

        self.assertEqual(error_count, len(expected_error_lines))
        self.assertSetEqual(found_error_lines, set(expected_error_lines))


if __name__ == "__main__":
    unittest.main()
