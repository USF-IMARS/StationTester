Installation:

`python setup.py install`
or
`python setup.py develop`

For example usage see `new_test_template.py`.


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

These are the values you may want to pass using `TestHelper.SPA_command`.
Example usage for `modisl1db/l0tol1`:

```python
TestHelper.SPA_command( self,
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
