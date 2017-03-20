#!/usr/bin/env python
"""
This template contains useful boilerplate for setting up a new test.

After copy-pasting this, you should replace this description, TEST_CASE_NAME,
TEST_NAME, and TEST_DESCRIPTION.
"""

# std modules:
import unittest

# dependencies:
from StationTester import test_helper

class Test_TEST_CASE_NAME(unittest.TestCase):

    def setUp(self):
        test_helper.SPATestSetUp()

    def tearDown(self):
        test_helper.SPATestTearDown()

    # tests:
    #########################
    def test_TEST_NAME(self):
        """ TEST_DESCRIPTION """

        pass  # test code here.
