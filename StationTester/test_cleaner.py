#!/usr/bin/env python3
"""
static helpers for cleaning up before/after SPA tests
"""
import os
import shutil

from StationTester import path_helper

# set _should_clean=False to *NOT* clean output files after each test.
# Useful for manual file checking, but also dangerous b/c not cleaning up
# may cause failing code to pass tests. So just remember to set it back to
# True when you're done.
_should_clean = True  # should (usually) be True!


def _del_testdata_out():
    print("rm test output...")
    filelist = [ f for f in os.listdir(path_helper._outdir) ]
    for f in filelist:
        os.remove(os.path.join(path_helper._outdir, f))

def _clean_sandbox():
    for f in [ fi for fi in os.listdir(path_helper._sandbox)]:
        path = os.path.join(path_helper._sandbox, f)
        try :
            os.remove( path )
        except IsADirectoryError:
            shutil.rmtree( path )

def clean():
    if (_should_clean):
        _del_testdata_out()
        _clean_sandbox()
    else:
        print("WARN: Not cleaning can cause tests to pass erroneously!")
