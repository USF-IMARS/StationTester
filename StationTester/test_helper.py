#!/usr/bin/env python3
"""
static helper class (common w/ other test classes) with useful methods like
file cleanup and errfile checking.
"""

import os
import shutil
import subprocess

from StationTester import util, path_helper, test_cleaner

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
    command = path_helper.wrapper_home+'/run ' + command
    command = command.replace('$INPUT', path_helper._indir)
    command = command.replace('$OUTPUT', path_helper._outdir)

    # run command:
    result = check_cmd(command)

    # perform checks:
    _expect_empty_errfiles(testClass, errfiles)
    _expect_files(testClass, expected_files, path_helper._sandbox)
    _expect_files(testClass, products, path_helper._outdir)

    return result

def check_cmd(cmd, env=None, bufsize=-1, stdbuf=False):
    """
    runs given command, expects command to return 0.
    cmd is expected to be a string.

    set bufsize=1 or bufsize=0 or stdbuf=True if stdout is not capturing.
    """
    cmd = cmd.replace('~/', os.path.expanduser('~/'))

    if (stdbuf):
        cmd = "stdbuf -o0 -e0 " + cmd

    print(cmd)
    try:
        res = subprocess.run(
            cmd,
            check=True, stdout=subprocess.PIPE, stderr=subprocess.STDOUT,
            shell=True, cwd=path_helper._sandbox, env=env, bufsize=bufsize,
        )
        return res
    except subprocess.CalledProcessError as err:
        raise AssertionError(
            'Command did not return 0. Returned '+str(err.returncode)+
            '\nCaptured command output below (WARN: may be incomplete):\n\n'
            + err.output.decode("ascii")+'\n\n'
        )

def file_is_empty(filename):
    return os.stat(filename).st_size == 0

def file_not_empty(filename):
    return os.stat(filename).st_size > 0

def SPATestSetUp():
    test_cleaner.clean()
    print ("test setup complete.")

def SPATestTearDown():
    print ("clean up after test...")
    test_cleaner.clean()

def _expect_files(testClass, files, directory):
    """ assert non-empty files exist at given directory """
    for fff in files:
        # print(errfile, '?')
        path=os.path.join(directory, fff)
        testClass.assertTrue(
            os.path.exists(path),
            'expected product: "' + fff + '" not found at '
            + path
        )
        testClass.assertTrue(
            file_not_empty(path),
            'expected file "' + fff + '" is empty.'
        )

def expect_params_in_file(testClass, filename, paramDict):
    """
    assert given filename contains key-val paired params matching paramDict
    """
    actual = util.read_params(filename)
    testClass.assertDictContainsSubset(paramDict, actual)


def _expect_empty_errfiles(testClass, errfiles, directory=None):
    """ assert no errs in errfiles """
    if (directory is None):
        directory = path_helper._sandbox
    for errfile in errfiles:
        # print(errfile, '?')
        path = os.path.join(directory, errfile)
        is_empty = file_is_empty(path)
        if (not is_empty):
            os.rename(path, os.path.join(path_helper.err_file_box, errfile))

        testClass.assertTrue(
            is_empty,
            'errfile "' + errfile + '" not empty. \n\t\t'
                'Moved to ' + path_helper.err_file_box + ' for manual inspection.'
        )

def expect_substr_in_outstr(testClass, substr, result, n=1, _not=False):
    """
    asserts given substr is in given result.stdout

    n: number of times cmdstr should be expected
    _not: expect substr is NOT in result.stdout
    """

    outstr = result.stdout.decode("ascii")
    n_actual = outstr.count(substr)

    if _not:
        testClass.assertFalse(
            n_actual > 0,
            outstr + "\n\nunwanted substr \"" + substr + "\" found in stdout (above)"
        )
    else:
        testClass.assertEqual(
            n, n_actual,
            outstr + "\n\nsubstr " + substr + " found in stdout (above) " + str(n_actual) + "/" + str(n) + " times."
        )
