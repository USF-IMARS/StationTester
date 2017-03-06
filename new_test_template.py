#!/usr/bin/env python
"""
This template contains useful boilerplate for setting up a new test.

After copy-pasting this, you should replace this description, TEST_CASE_NAME,
TEST_NAME, and TEST_DESCRIPTION.
"""

# std modules:
import unittest

# dependencies:
from StationTester.TestHelper import TestHelper

class Test_TEST_CASE_NAME(unittest.TestCase):

    def setUp(self):
        TestHelper.mySetup()

    def tearDown(self):
        TestHelper.myTeardown()

    # tests:
    #########################
    def test_TEST_NAME(self):
        """ TEST_DESCRIPTION """

        pass  # test code here.
