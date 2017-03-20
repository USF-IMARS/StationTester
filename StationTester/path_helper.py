#!/usr/bin/env python3
"""
static helpers for working with StationTester and IPOPP directories
"""
import os

### static attribs: ###
stationTestDir = os.path.expanduser("~/drl/StationTester")
wrapper_home = os.path.join(stationTestDir, "wrapper/lib")
_outdir   = os.path.join(stationTestDir, "test_data/output")
_indir    = os.path.join(stationTestDir, "test_data/input")
_sandbox      = os.path.join(stationTestDir, "test_data/sandbox")
err_file_box = os.path.join(stationTestDir, "test_data/err_files")
SPA_DIR=os.path.expanduser("~/drl/SPA")


# Currently unused & deprecated:
testscriptdir = "./"
FNULL = open(os.devnull, 'w')


### static methods: ###
def sandbox_file(filename):
    """returns full path to file in sandbox"""
    return os.path.join(_sandbox, filename)

def input_file(filename):
    """returns full path to input file in test_data"""
    return os.path.join(_indir, filename)

def output_file(filename):
    """returns full path to output file in test_data"""
    return os.path.join(_outdir, filename)

def cfg_path(packageName, stationName):
    """returns full path to cfgfile of given package and station"""
    return os.path.join(station_path(packageName, stationName), "station.cfgfile")

def station_path(package, station_name=""):
    """ returns path for given station name and package"""
    return os.path.join(
        SPA_DIR, package, 'station', station_name
    )
