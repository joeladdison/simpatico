from tests import TestSimpatico

class TestNaming(TestSimpatico):

    def test_lines(self):
        expected_error_lines = [3, 4, 7, 11, 18, 19, 20, 21, 22, 23, 25, 28, 36,
                37]

        f = 'tests/files/names.c'
        s = self.run_simpatico(f)
        error_count = len(s.errors.naming_d.keys())
        found_error_lines = set(s.errors.naming_d.keys())

        self.assertEqual(error_count, len(expected_error_lines))
        self.assertSetEqual(found_error_lines, set(expected_error_lines))

    def test_good(self):
        expected_error_lines = []

        f = 'tests/files/goodNaming.c'
        s = self.run_simpatico(f)
        error_count = len(s.errors.naming_d.keys())
        found_error_lines = set(s.errors.naming_d.keys())

        self.assertEqual(error_count, len(expected_error_lines))
        self.assertSetEqual(found_error_lines, set(expected_error_lines))

    def test_bad(self):
        expected_error_lines = [1, 6, 8, 10, 12, 18, 20, 21, 23, 26, 29, 32,
                36,38]

        f = 'tests/files/bad_naming.c'
        s = self.run_simpatico(f)
        error_count = len(s.errors.naming_d.keys())
        found_error_lines = set(s.errors.naming_d.keys())

        self.assertEqual(error_count, len(expected_error_lines))
        self.assertSetEqual(found_error_lines, set(expected_error_lines))
