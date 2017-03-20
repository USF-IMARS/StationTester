#!/usr/bin/env python
"""
tests extra tools
"""

# std modules:
import os
import unittest

# dependencies:
from StationTester import util, path_helper, test_helper
import configparser

class Test_util_basic_tests(unittest.TestCase):

    # tests:
    #########################
    def test_list_deps_runs(self):
        """
        check for no exceptions in list_dependencies() for imars & seadas7.3
        """
        util.list_dependencies("imars", verbose=True)
        util.list_dependencies("seadas7.3", verbose=True)

    def test_get_deps_imars_modis_sst_png(self):
        """ get_deps reports modis_sst_png has alg generate_thumbnail """
        result = util.get_dependencies("imars", "modis_sst_png")
        self.assertTrue("generate_thumbnail" in result)

    def test_get_deps_imars_modis_sst_png_PngGenerator(self):
        """ get_deps reports alg generate_thumbnail is PngGenerator.py """
        result = util.get_dependencies("imars", "modis_sst_png")
        self.assertTrue("PngGenerator" in result["generate_thumbnail"])

    def test_read_params(self):
        params = util.read_params(
            os.path.join(path_helper._indir, 'test_config.ini')
        )
        print(params)
        self.assertEqual(params['test1'], 'apple')
        self.assertEqual(params['test2'], 'bin')

    def test_read_paramstring(self):
        res = util.read_paramstring(
            "needed_files=~/level2/terra.OC.17047153500.hdf"
        )
        self.assertDictContainsSubset(
            {"needed_files":"~/level2/terra.OC.17047153500.hdf"},
            res
        )
