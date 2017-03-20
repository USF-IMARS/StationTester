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
