"""
static helper class (common w/ other test classes) with useful methods like
file cleanup and errfile checking.
"""

import os
import shutil
import subprocess

class TestHelper:
    wrapper_home = os.path.expanduser("~/drl/StationTester/wrapper/lib")
    testoutdir   = os.path.expanduser("~/drl/StationTester/test_data/output")
    testindir    = os.path.expanduser("~/drl/StationTester/test_data/input")
    testscriptdir = "./"
    FNULL = open(os.devnull, 'w')

    # set _should_clean=False to *NOT* clean output files after each test.
    # Useful for manual file checking, but also dangerous b/c not cleaning up
    # may cause failing code to pass tests. So just remember to set it back to
    # True when you're done.
    _should_clean = True  # should (usually) be True!

    @staticmethod
    def SPA_command(
            testClass,
            command,
            products=[],
            errfiles=[],
            expected_return_value=None
        ):
        """
        tests the given spa command
        products : list of files expected to be produced in the testoutdir
        errfiles : list of files expected to be empty after running command
        expected_return_value : value command is expected to return
        """
        print(command)
        return_value = subprocess.call(
            command, shell=True, stdout=TestHelper.FNULL, stderr=subprocess.STDOUT
        )

        TestHelper._test_products_and_errfiles(testClass, products, errfiles)
        if (expected_return_value is not None):
            testClass.assertEquals(return_value, expected_return_value)

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
        if (TestHelper._should_clean):
            TestHelper._del_testdata_out()
            TestHelper._del_errfiles()
            TestHelper._del_stdfiles()
            TestHelper._cleanup_l1atob()
        else:
            print("WARN: Not cleaning can cause tests to pass erroneously!")

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
            expected_file=os.path.join(TestHelper.testoutdir, expected_product)
            testClass.assertTrue(
                os.path.exists(expected_file),
                'expected product: "' + expected_product + '" not found at '
                + expected_file
            )
        # assert no errs in errfiles
        for errfile in errfiles:
            # print(errfile, '?')
            testClass.assertTrue(
                TestHelper.file_is_empty(errfile),
                'errfile "' + errfile + '" not empty.'
            )
