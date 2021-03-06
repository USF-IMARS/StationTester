# Installation:

`python setup.py install`
or
`python setup.py develop`

# Usage

## Test Runner
The TestRunner module encapsulates the running of many SPA tests via nosetest.
 Basic usage looks like:

```bash
$ ./StationTester/TestRunner.py
```

```python
tester = TestRunner()
tester.test_all_SPAs()
```

## Test Helper
The test_helper module is intended to help with writing of SPA station tests.

For example usage also see `new_test_template.py`.

```python
from StationTester import path_helper, test_helper

def test_using_SPA_command(self):
    """ runs station via IPOPP wrapper, checks that `products` exist, and `errfiles` are empty """
    test_helper.SPA_command( self,
        (
            ' mySPAName/wrapper/myStationName'
            ' -1 args -a blah.blah'
            ' --myprograminfile  $OUTPUT/myProgramOutput.txt'
            ' --myprogramoutfile $INPUT/myProgramInput.txt'
        ),
        products=['myProgramOutput.txt', 'myProgramOutput2.csv'],
        errfiles=['myProgram_errfile', 'myProgramErrlog.txt'],
    )

def test_paramstring_from_stdout(self):
    """ tests param string coming from stdout (or stderr) """

    result = test_helper.check_cmd(
        "blah/blah/command_blah.sh -1 args -a blah.blah"
    )

    # expects output to be `param_1_key=expected_param_1_value`
    self.assertDictContainsSubset(
        util.read_paramstring(result.stdout.decode('ascii')),
        {"param_1_key":"expected_param_1_value")}
    )


def test_stdout_for_substrings(self):
    """
    tests stdout (or stderr) for specific substrings
    """
    result = test_helper.check_cmd(
        "blah/blah/command_blah.sh -1 args -a blah.blah"
    )

    # assert expected_substr in stdout/stderr:
    test_helper.expect_substr_in_outstr(
        self,
        "my expected\tsub/`~'\"'`string",
        result
    )

    # assert substring not in stdout/stderr:
    test_helper.expect_substr_in_outstr(
        self,
        "my unexpected\tERROR/`~'\"'`string",
        result,
        _not=True
    )

    # assert expected_substr in stdout/stderr 7 times:
    test_helper.expect_substr_in_outstr(
        self,
        "my expected\tsub/`~'\"'`string",
        result,
        n=7
    )

def test_output_paramfile(self):
    """ tests paramfile contents created by test """
    # run test cmd
    test_helper.check_cmd(
        "blah/blah/command_blah.sh -1 args -a blah.blah"
    )

    # check MyProgramFile.txt has been created and contains "param_1=abc123"
    test_helper.expect_params_in_file(
        self,
        path_helper.sandbox_file("MyProgramFile.txt"),
        {"param_1":"abc123"}
    )
```

### Test Attributes
Supported attributes are:

```python
@attr(length='100')  # approx length of test in seconds
@attr('slow')       # test is slow to run ( length is > 30 )
```


## Tips on Making Tests for extant SPAs

### At the SPA Station level:
Check the SPA's `generic.xml` for something like this example from `modisl1db/l0tol1`:

```xml
<Parameters>
 <sat/>
 <geocheck_threshold optional="true"/>
</Parameters>
<Inputs>
 <modis.pds/>
 <gbad_att optional="true"/>
 <gbad_eph optional="true"/>
 <leapsec optional="true"/>
 <utcpole optional="true"/>
</Inputs>
<Outputs>
 <modis.mxd01/>
 <modis.mxd03/>
</Outputs>
```

These are the values you may want to pass using `test_helper.SPA_command`.
Example usage for `modisl1db/l0tol1`:

```python
test_helper.SPA_command( self,
            (
                ' ../wrapper/l0tol1'
                ' modis.pds $INPUT/P0420064AAAAAAAAAAAAAA12249171145001.PDS'
                ' modis.mxd01 $OUTPUT/L1ATerra.hdf'
                ' modis.mxd03 $OUTPUT/GEOTerra.hdf'
                ' sat '         + 'TERRA'
                ' leapsec $INPUT/leapsec.dat'
                ' utcpole $INPUT/utcpole.dat'
                ' geocheck_threshold 50'
            )
)
```

### At the algorithm level:
1. Run the IPOPP station in the console, like this (for station `l0l1aqua`):
`./drl/ncs/stations/l0l1aqua/jsw/bin/wrapper.sh console`

2. Run the station (by deleting a marker or otherwise creating an unprocessed pre-
  product of this station).

3. Look for `cmd = *`. You may copy and modify these commands to create tests
  for the specific algorithm command at this step.

More hints about the usage of commands can be deciphered from an `Ncs_run` command in the
`<commands>` block of stations' `generic.xml` files:

```
<!-- Run L0 to L1 -->
<Ncs_run debug="true"
runFlag="true" standardFile="stdfileL1A" errorFile="errfileL1A"
  cmd="{ML1Acmd} {modis.pds} -m {sat} -o L1A.hdf --log"
>
  <env name="DBHOME" value="{modis_L1_home}"/>
  <env name="SEADAS" value="{modis_L1_home}"/>
  <env name="OCSSWROOT" value="{modis_L1_home}"/>
  <env name="MODIS_GEO" value="."/>
  <env name="MODIS_L1A" value="."/>
  <env name="MODIS_L1B" value="."/>
  <env name="LIB3_BIN" value="{modis_L1_home}{/}run{/}bin"/>
  <env name="OCDATAROOT" value="{modis_L1_home}{/}run{/}data"/>
  <env name="MODIS_ATTEPH" value="{modis_L1_home}{/}var{/}modis{/}atteph"/>
  <env name="AQUA_REFL_LUT" value="."/>
  <env name="AQUA_EMIS_LUT" value="."/>
  <env name="AQUA_QA_LUT" value="."/>
  <env name="TERRA_REFL_LUT" value="."/>
  <env name="TERRA_EMIS_LUT" value="."/>
  <env name="TERRA_QA_LUT" value="."/>
  <env name="OCSSW_BIN" value="{modis_L1_home}{/}run{/}bin"/>
  <env name="PATH" value="{modis_L1_home}{/}run{/}scripts:{modis_L1_home}{/}run{/}bin:{PATH}"/>
</Ncs_run>
```

# Theoretically planned implementation
NOTE: this section contains information about a planned (not yet complete) implementation.

There are three levels of tests within the IPOPP setup. 

1. Unit/Module/Script Level (multiple sets per station)
    1. tests a single "unit" of code 
    2. triggered whenever a change is committed 
    3. uses Travis CI 
2. Algorithm Level (one set per station algorithm (multiple algorithms per station))
    1. tests an entire IPOPP station, starting with test input data and checking against expected output. (eg L0->L1). 
    2. run on an *ad hoc* basis
    3. uses python3's "nose" module.
    4. (see also each station's pdf & testing package. eg: modisl1db*.pdf/"Software Package Testing and Validation"
3. Stack/Integration Level (one set for all stations)
    1. tests the full stack of stations, starting from test L0 inputs and checking the final products. 
    2. uses python3's "nose" module.
    3. Triggered automatically on a periodic basis (nightly?).

