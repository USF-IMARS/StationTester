#!/usr/bin/env python
"""
tests CFGFileReader
"""

# std modules:
import unittest

# dependencies:
from StationTester import path_helper
from StationTester.CFGFileReader import CFGFileReader

class Test_CFGFileReader(unittest.TestCase):

    # tests:
    #########################
    def test_get_inflows_on_imars(self):
        """
        get_inflows(imars, img_publisher) returns expected inflow products
        """
        cfg = CFGFileReader(path_helper.cfg_path("imars", "img_publisher"))
        actual = cfg.get_inflows()
        expected = ['imars.%.mapped.png', 'imars.%.mapped']

        self.assertEqual(expected, actual)

    def test_graph_modisl1db_outflows(self):
        """ modisl1db l0l1aqua outflows are drl.aqua.modis.mxd01 """
        cfg = CFGFileReader(path_helper.cfg_path("modisl1db", "l0l1aqua"))
        actual = cfg.get_outflows()
        expected = ['drl.aqua.modis.mxd01', 'drl.aqua.modis.mxd03']

        self.assertEqual(expected, actual)

    def test_graph_modisl1db_l0l1aqua_inflows(self):
        """
        modisl1db l0l1aqua inflows = ['drl.aqua.modis.pds','{otherInputTypes}']
        """
        cfg = CFGFileReader(path_helper.cfg_path("modisl1db", "l0l1aqua"))
        actual = cfg.get_inflows()
        expected = ['drl.aqua.modis.pds','{otherInputTypes}']

        self.assertEqual(expected, actual)

    def test_graph_modisl1db_l0l1aqua_inflows_w_formatter(self):
        """
        modisl1db l0l1aqua inflows = ['drl.aqua.modis.pds']
        """
        cfg = CFGFileReader(path_helper.cfg_path("modisl1db", "l0l1aqua"))
        cfg.set_varsub(True)
        actual = cfg.get_inflows()
        expected = ['drl.aqua.modis.pds']

        self.assertEqual(expected, actual)


    def test_get_var_value_basic(self):
        """ tests get_var_value for simple Ncs_set var """
        cfg = CFGFileReader(path_helper.cfg_path("modisl1db", "l0l1aqua"))
        actual = cfg._get_var_value("otherInputTypes", 32)
        expected = ""

        self.assertEqual(expected, actual)

    def test_var_sub_basic(self):
        """ tests var_sub for simple Ncs_set var """
        cfg = CFGFileReader(path_helper.cfg_path("modisl1db", "l0l1aqua"))
        actual = cfg._var_sub("test{otherInputTypes} test", 32)
        expected = "test test"

        self.assertEqual(expected, actual)
