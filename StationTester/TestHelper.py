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
    err_file_box = os.path.join(stationTestDir, "test_data/err_files")
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
        ):
        """
        Tests the given spa command using the NCS wrapper with
        cwd=test_data/sandbox. Commands are expected to return 0.

        Auto-substitues $INPUT and $OUTPUT with location of
        test_data/{in|out}put directories so you can `$INPUT/testfile.txt`
        in your commands.

        products : list of files expected to be produced in the testoutdir.
        errfiles : list of files expected to be empty after running command.
        """
        # prep command:
        command = TestHelper.wrapper_home+'/run ' + command
        command = command.replace('$INPUT', TestHelper.testindir)
        command = command.replace('$OUTPUT', TestHelper.testoutdir)
        command = command.replace(' ~/', ' '+os.path.expanduser('~/'))

        # run command:
        TestHelper.check_cmd(command)

        # perform checks:
        TestHelper._expect_empty_errfiles(testClass, errfiles)
        TestHelper._expect_files(testClass, expected_files, TestHelper.sandbox)
        TestHelper._expect_files(testClass, products, TestHelper.testoutdir)

    @staticmethod
    def check_cmd(cmd):
        """ runs given command, expects command to return 0 """
        print(cmd)
        try:
            subprocess.check_output(
                cmd, shell=True, cwd=TestHelper.sandbox
            )
        except subprocess.CalledProcessError as err:
            raise AssertionError(
                'Command did not return 0. \nCaptured command output below (WARN: may be incomplete):\n\n'
                + err.output.decode("ascii")+'\n\n'
            )

    @staticmethod
    def _del_testdata_out():
        print("rm test output...")
        filelist = [ f for f in os.listdir(TestHelper.testoutdir) ]
        for f in filelist:
            os.remove(os.path.join(TestHelper.testoutdir, f))

    @staticmethod
    def _clean_sandbox():
        for f in [ fi for fi in os.listdir(TestHelper.sandbox)]:
            path = os.path.join(TestHelper.sandbox, f)
            try :
                os.remove( path )
            except IsADirectoryError:
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
    def _expect_files(testClass, files, directory):
        # assert non-empty files exist at given directory
        for fff in files:
            # print(errfile, '?')
            path=os.path.join(directory, fff)
            testClass.assertTrue(
                os.path.exists(path),
                'expected product: "' + fff + '" not found at '
                + path
            )
            testClass.assertTrue(
                TestHelper.file_not_empty(path),
                'expected file "' + fff + '" is empty.'
            )

    @staticmethod
    def expect_params_in_file(testClass, filename, paramDict):
        """
        assert given filename contains key-val paired params matching paramDict
        """
        actual = util.read_params(filename)
        testClass.assertDictContainsSubset(paramDict, actual)

    @staticmethod
    def sandbox_file(filename):
        """returns full path to file in sandbox"""
        return os.path.join(TestHelper.sandbox, filename)

    @staticmethod
    def _expect_empty_errfiles(testClass, errfiles, directory=None):
        # assert no errs in errfiles
        if (directory is None):
            directory = TestHelper.sandbox
        for errfile in errfiles:
            # print(errfile, '?')
            path = os.path.join(directory, errfile)
            is_empty = TestHelper.file_is_empty(path)
            if (not is_empty):
                os.rename(path, os.path.join(TestHelper.err_file_box, errfile))

            testClass.assertTrue(
                is_empty,
                'errfile "' + errfile + '" not empty. \n\t\t'
                    'Moved to ' + TestHelper.err_file_box + ' for manual inspection.'
            )
