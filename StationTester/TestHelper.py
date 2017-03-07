"""
static helper class (common w/ other test classes) with useful methods like
file cleanup and errfile checking.
"""

import os
import shutil
import subprocess

class TestHelper:
    stationTestDir = os.path.expanduser("~/drl/StationTester")
    wrapper_home = os.path.join(stationTestDir, "wrapper/lib")
    testoutdir   = os.path.join(stationTestDir, "test_data/output")
    testindir    = os.path.join(stationTestDir, "test_data/input")
    sandbox      = os.path.join(stationTestDir, "test_data/sandbox")
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
            expected_files=[],
            expected_return_value=None
        ):
        """
        Tests the given spa command using the NCS wrapper with
        cwd=test_data/sandbox. Commands are expected to return 0.

        Auto-substitues $INPUT and $OUTPUT with location of
        test_data/{in|out}put directories so you can `$INPUT/testfile.txt`
        in your commands.

        products : list of files expected to be produced in the testoutdir.
        errfiles : list of files expected to be empty after running command.

        (DEPRECATED) expected_return_value : value command is expected to return
        """
        # prep command:
        command = TestHelper.wrapper_home+'/run ' + command
        command = command.replace('$INPUT', TestHelper.testindir)
        command = command.replace('$OUTPUT', TestHelper.testoutdir)

        # run command:
        print(command)
        try:
            subprocess.check_output(
                command, shell=True, cwd=TestHelper.sandbox
            )
        except subprocess.CalledProcessError as err:
            raise AssertionError(
                'Command did not return 0. \nCaptured command output below (WARN: may be incomplete):\n\n'
                + err.output.decode("ascii")+'\n\n'
            )

        # perform checks:
        TestHelper._test_products_and_errfiles(testClass, products, errfiles)
        Testhelper._expect_files(testClass, expected_files)

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
    def _clean_sandbox():
        for f in [ fi for fi in os.listdir(TestHelper.sandbox)]:
            path = os.path.join(TestHelper.sandbox, f)
            try :
                os.remove( path )
            except os.IsADirectoryError:
                shutil.rmtree( path )


    @staticmethod
    def file_is_empty(filename):
        return os.stat(filename).st_size == 0

    @staticmethod
    def file_not_empty(filename):
        return os.stat(filename).st_size > 0

    @staticmethod
    def clean():
        if (TestHelper._should_clean):
            TestHelper._del_testdata_out()
            TestHelper._del_errfiles()
            TestHelper._del_stdfiles()
            TestHelper._cleanup_l1atob()
            TestHelper._clean_sandbox()
        else:
            print("WARN: Not cleaning can cause tests to pass erroneously!")

    @staticmethod
    def mySetup():
        TestHelper.clean()
        print ("test setup complete.")

    @staticmethod
    def myTeardown():
        print ("clean up after test...")
        TestHelper.clean()

    @staticmethod
    def _expect_files(testClass, files):
        # assert non-empty files exist
        for fff in files:
            # print(errfile, '?')
            testClass.assertTrue(
                TestHelper.file_not_empty(
                    os.path.join(TestHelper.sandbox, fff)
                ),
                'expected file "' + fff + '" is empty.'
            )

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
                TestHelper.file_is_empty(
                    os.path.join(TestHelper.sandbox, errfile)
                ),
                'errfile "' + errfile + '" not empty.'
            )
