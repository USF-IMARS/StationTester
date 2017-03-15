#!/usr/bin/env python3
"""
class to help run unit/integration tests across multiple SPAs

basic usage:
    tester = TestRunner()
    tester.test_all_SPAs()
"""

from nose.tools import nottest
import nose
import configparser
import argparse
import os

from StationTester import util
from StationTester.TestHelper import TestHelper

class TestRunner:

    def __init__(self, arguments):
        self.args = arguments

    @nottest
    def test_all_SPAs(self, verbose=None):
        if (verbose is None): verbose = self.args.verbose

        test_list = self.get_all_tests(verbose=verbose)
        # print ("test_list: ", test_list)

        test_str = ""
        for tst in test_list:
            test_str += tst + ','
        test_str = test_str[:-1]  # chop off last comma
        # print ("\n\ntest_str: ", test_str)

        nose.run(defaultTest=test_str)

    @nottest
    def get_all_tests(self, verbose=None):
        if (verbose is None): verbose = self.args.verbose

        # create list of all tests:
        tests = [TestHelper.stationTestDir]  # starting w/ self tests
        for SPA in util.get_packages():
            if verbose: print("| ", SPA)
            tests.append(os.path.join(util.SPA_DIR, SPA))  # root of SPA

            # SPA/algorithm/ root
            alg_dir = os.path.join(util.SPA_DIR, SPA, 'algorithm')
            if(os.path.isdir(alg_dir)):
                tests.append(alg_dir)

            # SPA/station/*/
            try:
                for station in util.get_stations(SPA):
                    if verbose: print("|---- ", os.path.basename(station))
                    tests.append(util.get_station_path(SPA, station))
            except FileNotFoundError as fnf_err:
                print(
                    "\nWARN: no stations found for SPA \"",
                    os.path.basename(SPA)
                )

            # dirs listed in SPA/setup.cfg [nosetests] tests=dir1, dir2
            try:
                cfg = configparser.ConfigParser()
                filename = os.path.join(SPA, "setup.cfg")
                cfg.read(filename)
                testlist = cfg['nosetests']['tests'].split(',')
                tests.extend([ test.strip() for test in testlist ])
                if verbose:
                    print("|- setup.cfg")
                    [print("|---- ", test) for test in testlist ]
            except KeyError as k_err:
                print("\nWARN: no setup.cfg for \"", os.path.basename(SPA), "\".\n")

        return tests

if __name__ == "__main__":
    #parser = argparse.ArgumentParser(description='run tests across all SPAs')
    parser = argparse.ArgumentParser()
    parser.add_argument("-v", "--verbose", help="increase output verbosity",
                        action="store_true")
    args = parser.parse_args()
    tester = TestRunner(args)
    tester.test_all_SPAs()
