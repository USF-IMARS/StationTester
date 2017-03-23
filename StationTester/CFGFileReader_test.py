#!/usr/bin/env python
"""
tests CFGFileReader
"""

# std modules:
import unittest

# dependencies:
from StationTester import path_helper
from StationTester.CFGFileReader import CFGFileReader, CFGFileValueError

class Test_CFGFileReader(unittest.TestCase):

    # tests:
    #########################
    def test_get_inflows_on_imars(self):
        """
        get_inflows(imars, img_publisher) returns expected inflow products
        """
        cfg = CFGFileReader(path_helper.cfg_path("imars", "img_publisher"))
        actual = cfg.get_inflows()
        expected = ['imars.%.mapped.png']

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

    def test_oc_png_has_no_None_inflows_w_formatter(self):
        """
        imars oc_png inflows should not include "None" values
        """
        cfg = CFGFileReader(path_helper.cfg_path("imars", "oc_png"), verbose=True)
        cfg.set_varsub(True)
        actual = cfg.get_inflows()
        expected = ['imars.getProductType().modis.oc.mapped']

        self.assertEqual(expected, actual)

    def test_var_sub_on_empty_string_raises(self):
        """ var_sub should raise err when string formats to "" """
        cfg = CFGFileReader(path_helper.cfg_path("imars", "oc_png"), verbose=True)
        self.assertRaises(CFGFileValueError, cfg._var_sub, "{otherInputTypes}")

    def test_var_sub_on_crefl(self):
        """h2g modis_tcolor-2 w/ undefined product_group"""
        cfg = CFGFileReader(
            path_helper.cfg_path("h2g", "modis_tcolor-2"),
            verbose=True
        )
        cfg.set_varsub(True)
        actual = cfg.get_inflows()
        expected = ['imars.getProductType().modis.?product_group?.filtered']

        self.assertEqual(expected, actual)

    def test_brackets_in_varnames(self):
        """ test using CVIIRS/CVIIRS which has brackets in varnames """
        cfg = CFGFileReader(
            path_helper.cfg_path("CVIIRS", "CVIIRS"),
            verbose=True
        )
        cfg.set_varsub(True)
        actual = cfg.get_inflows()
        expected = [
            'drl.npp.viirs.svm03',
            'drl.npp.viirs.svm04 drl.npp.viirs.svm05 drl.npp.viirs.svm07 '
            'drl.npp.viirs.svm08 drl.npp.viirs.svm10 drl.npp.viirs.svm11 '
            'drl.npp.viirs.svi01 drl.npp.viirs.svi02 drl.npp.viirs.svi03 '
            'drl.npp.viirs.gmtco'
        ]

        self.assertEqual(expected, actual)

    def test_getDate_wo_name(self):
        """ test using CVIIRS/CVIIRS which uses getDate() w/o name attrib """
