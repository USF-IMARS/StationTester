#!/usr/bin/env python
"""
tests extra tools
"""

# std modules:
import unittest

# dependencies:
from StationTester import ExtraTools

class Test_TEST_CASE_NAME(unittest.TestCase):

    # tests:
    #########################
    def test_list_deps_does_runs(self):
        """ check for no exceptions in list_dependencies("imars")"""
        ExtraTools.list_dependencies("imars")
