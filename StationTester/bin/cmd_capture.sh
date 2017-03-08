# Attempt at automating test-building process for extant stations.
# In its current form this will NOT work. Instead copy the commands individually
# for now.

PACKAGE_NAME=imars
STATION_NAME=img_publisher

FULL_OUT_FILE=output.txt
CMD_LIST_FILE=commands.txt

# capture output for station run in console
stdbuf -o0 ~/drl/ncs/stations/$STATION_NAME/jsw/bin/wrapper.sh console 2>&1 | tee $FULL_OUT_FILE

# TODO: delete marker

# TODO: wait for it to stop at something like:
      # jvm 1    | ---Debug---> Dsm_command
      # jvm 1    | class	 = 	DSM
      # jvm 1    | method	 = 	releaseProduct
      # jvm 1    | result	 =
      # jvm 1    | resultUnboundOK	 = 	false
      # jvm 1    | value	 = 	imars.terra.modis.chlor_a.mapped.png.OBJ
      # jvm 1    | imars.terra.modis.chlor_a.mapped.png.OBJ	 = 	Product 480 imars.terra.modis.chlor_a.mapped.png 2017-02-16 15:30:00 2017-02-16 15:34:59
      # jvm 1    | DSM Method Arguments:
      # jvm 1    | arg[0] = gov.nasa.gsfc.nisgs.dsm.Product	 value = Product 480 imars.terra.modis.chlor_a.mapped.png 2017-02-16 15:30:00 2017-02-16 15:34:59
      # jvm 1    | Object found in pool: DSM = gov.nasa.gsfc.nisgs.dsm.DSM@4d101530
      # jvm 1    |
      # jvm 1    | return type = void
      # jvm 1    | ---command complete
      # jvm 1    | runFlag	 = 	true
      # jvm 1    |
      # jvm 1    | ---Debug---> Dsm_command
      # jvm 1    | class	 = 	DSM
      # jvm 1    | method	 = 	reserveProductLikeProductType
      # jvm 1    | result	 = 	cfg_wild.OBJ
      # jvm 1    | resultUnboundOK	 = 	false
      # jvm 1    | value	 = 	imars.%.mapped.png
      # jvm 1    | value	 =
      # jvm 1    | DSM Method Arguments:
      # jvm 1    | arg[0] = java.lang.String	 value = imars.%.mapped.png
      # jvm 1    | arg[1] = java.lang.String	 value =
      # jvm 1    | Object found in pool: DSM = gov.nasa.gsfc.nisgs.dsm.DSM@4088f226
# TODO: then send ctrl+c to stop station

# get cmd list out of full output.
grep cmd $FULL_OUT_FILE > $CMD_LIST_FILE

# TODO: do something with commands?
mv $CMD_LIST_FILE ~/drl/SPA/$PACKAGE_NAME/station/$STATION_NAME/.
# Something like make $STATION_NAME_commands_test.py from new_test_template.py
# and put it in package/station/$STATION_NAME/ ?

# cleanup
rm $FULL_OUT_FILE $CMD_LIST_FILE
