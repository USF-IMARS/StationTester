#!/usr/bin/env python
"""
tests test_helper methods
"""

import unittest
from unittest.mock import MagicMock

from StationTester import test_helper

class Test_util_basic_tests(unittest.TestCase):

    def _get_mock_result(self):
        result = MagicMock()
        result.stdout = b"""
            abc123\n\n\t\n  test test\ntest\ttest \napple\nban\tcan\tdan
        """
        return result

    # tests:
    #########################
    def test_expect_4_substr_in_outstr(self):
        """ expects 4/4 substr """
        test_helper.expect_substr_in_outstr(
            self,
            "test",
            self._get_mock_result(),
            n=4,
            _not=False
        )

    def test_expect_less_substr_in_outstr(self):
        """ expects exception when expecting 3/4 substr """
        with self.assertRaises(AssertionError):
            test_helper.expect_substr_in_outstr(
                self,
                "test",
                self._get_mock_result(),
                n=3,
                _not=False
            )

    def test_expect_substr_in_outstr_using_defaults(self):
        """ expects 1/1 substr _not=False by default """
        test_helper.expect_substr_in_outstr(
            self,
            "apple",
            self._get_mock_result()
        )

    def test_expect_substr_in_outstr_defaults(self):
        """ expects substr with _not=True """
        test_helper.expect_substr_in_outstr(
            self,
            "this is a long substring that should not be in mock stdout",
            self._get_mock_result(),
            _not=True
        )
