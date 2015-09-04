from tests import TestSimpatico


class TestOverall(TestSimpatico):
    def test_overall(self):
        expected_error_lines = []

        f = 'tests/files/testerino.c'
        s = self.run_simpatico(f)

        found_error_lines = set()
        for error_type in [s.errors.braces_d, s.errors.whitespace_d,
                s.errors.line_length_d, s.errors.naming_d, s.errors.comments_d,
                s.errors.indent_d]:
            for key in sorted(error_type.keys()):
                found_error_lines.add(key)

        self.assertEqual(len(found_error_lines), len(expected_error_lines))
        self.assertSetEqual(found_error_lines, set(expected_error_lines))
