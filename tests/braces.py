from tests import TestSimpatico

class TestBraces(TestSimpatico):
    def test_good(self):
        """ No brace errors of any kind, but does contain other errors """
        f = 'tests/files/good_braces.c'
        s = self.run_simpatico(f)
        braces_errors = len(s.errors.braces_d.keys())
        self.assertEqual(braces_errors, 0)

    def test_bad(self):
        # Line numbers could be arguably different for errors that
        # span multiple lines

        expected_error_lines = [16, 21, 23, 25, 27, 29, 38, 41,
                44, 54, 56, 58, 65, 68, 70, 74, 76, 85, 93, 96]

        f = 'tests/files/bad_braces.c'
        s = self.run_simpatico(f)

        braces_errors = len(s.errors.braces_d.keys())
        found_error_lines = set(s.errors.braces_d.keys())

        self.assertEqual(braces_errors, len(expected_error_lines))
        self.assertSetEqual(found_error_lines, set(expected_error_lines))
