from tests import TestSimpatico


class TestWhitespace(TestSimpatico):

    def test_whitespace(self):
        expected_error_lines = [1, 3, 8, 10, 12, 19, 20, 23,
                26, 29, 35, 40, 44, 45]

        f = 'tests/files/whitespace.c'
        s = self.run_simpatico(f)
        indent_errors = len(s.errors.whitespace_d.keys())
        found_error_lines = set(s.errors.whitespace_d.keys())

        self.assertEqual(indent_errors, len(expected_error_lines))
        self.assertSetEqual(found_error_lines, set(expected_error_lines))
