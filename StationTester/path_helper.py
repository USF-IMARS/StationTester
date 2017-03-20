#!/usr/bin/env python3
"""
static helpers for working with StationTester and IPOPP directories
"""
import os

stationTestDir = os.path.expanduser("~/drl/StationTester")
wrapper_home = os.path.join(stationTestDir, "wrapper/lib")
testoutdir   = os.path.join(stationTestDir, "test_data/output")
testindir    = os.path.join(stationTestDir, "test_data/input")
sandbox      = os.path.join(stationTestDir, "test_data/sandbox")
err_file_box = os.path.join(stationTestDir, "test_data/err_files")

# Currently unused & deprecated:
testscriptdir = "./"
FNULL = open(os.devnull, 'w')
