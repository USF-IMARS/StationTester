"""
static helper class (common w/ other test classes) with useful methods like
file cleanup and errfile checking.
"""

import os
import shutil

class TestHelper:
    wrapper_home = '/home/ipopp/drl/StationTester/wrapper/lib'
    testoutdir = "../testdata/output"
    testindir = "../testdata/input"
    testscriptdir = "./"
    FNULL = open(os.devnull, 'w')

    @staticmethod
    def _del_testdata_out():
        print("rm test output...")
        filelist = [ f for f in os.listdir(TestHelper.testoutdir) ]
        for f in filelist:
            os.remove(os.path.join(TestHelper.testoutdir, f))

    @staticmethod
    def _del_errfiles():
        print("rm errfile*...")
        filelist = [ f for f in os.listdir(TestHelper.testscriptdir) if f.startswith('errfile')]
        for f in filelist:
            os.remove(os.path.join(TestHelper.testscriptdir, f))

    @staticmethod
    def _del_stdfiles():
        print("rm stdfile*...")
        filelist = [ f for f in os.listdir(TestHelper.testscriptdir) if f.startswith('stdfile')]
        for f in filelist:
            os.remove(os.path.join(TestHelper.testscriptdir, f))

    @staticmethod
    def _cleanup_l1atob():
        print("rm {*hdf, *pcf}...")
        filelist = ([
            f for f in os.listdir(TestHelper.testscriptdir)
            if (
                f.endswith('.hdf') or
                f.endswith('.pcf')
            )
        ])
        for f in filelist:
            os.remove(os.path.join(TestHelper.testscriptdir, f))

        print("rm *_logs-pcf/...")
        folderlist = ([f for f in os.listdir(TestHelper.testscriptdir) if f.endswith('_logs-pcf')])
        for f in folderlist:
            shutil.rmtree(os.path.join(TestHelper.testscriptdir, f))

    @staticmethod
    def file_is_empty(filename):
        return os.stat(filename).st_size == 0

    @staticmethod
    def clean():
        TestHelper._del_testdata_out()
        TestHelper._del_errfiles()
        TestHelper._del_stdfiles()
        TestHelper._cleanup_l1atob()

    @staticmethod
    def mySetup():
        print ("test setup.")
        TestHelper.clean()

    @staticmethod
    def myTeardown():
        print ("test clean up.")
        TestHelper.clean()

    @staticmethod
    def _test_products_and_errfiles(testClass, products, errfiles):
        # assert expected product exists
        for expected_product in products:
            # print(expected_product, '?')
            testClass.assertTrue(
                os.path.exists(os.path.join(TestHelper.testoutdir, expected_product)),
                'expected product: "' + expected_product + '" not found.'
            )
        # assert no errs in errfiles
        for errfile in errfiles:
            # print(errfile, '?')
            testClass.assertTrue(
                TestHelper.file_is_empty(errfile),
                'errfile "' + errfile + '" not empty.'
            )
