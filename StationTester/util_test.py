#!/usr/bin/env python
"""
tests extra tools
"""

# std modules:
import unittest

# dependencies:
from StationTester import util

class Test_util_basic_tests(unittest.TestCase):

    # tests:
    #########################
    def test_list_deps_runs(self):
        """
        check for no exceptions in list_dependencies() for imars & seadas7.3
        """
        util.list_dependencies("imars", verbose=True)
        util.list_dependencies("seadas7.3", verbose=True)
