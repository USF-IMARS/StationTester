#!/usr/bin/env python3
"""
this module contains extra snippets which should NOT be used as-is,
but may be useful in the future.
"""

# TODO: check for extant input products and temporarily remove them
# JAVA FOR DELETE PRODUCT (from drl/dsm/lib/passmanager.jar/MarkersMain.DeleteButton.actionPerformed):
# //("About to delete " + arrayOfInt.length + " marker" + (arrayOfInt.length == 1 ? "" : "s") + ":");
# for (int n : arrayOfInt)
# {
#   MarkersMain.XMarker localXMarker2 = MarkersMain.MarkersJTableModel.access$300(MarkersMain.this.markersModel, n);
#   if (localXMarker2 != null) {
#     MarkersMain.this.dsm.update("DELETE FROM Markers WHERE product=" + localXMarker2.productId + " AND gopherColony=" + Utility.quote(localXMarker2.gopherColony));
#   }
# }

import os
import json  # for pretty printing
import configparser, itertools  # for param reading

from StationTester.CFGFileReader import CFGFileReader

SPA_DIR=os.path.expanduser("~/drl/SPA")

def get_packages(SPA_dir=SPA_DIR):
    """returns SPAs dir names in (optional) SPA directory"""
    SPA_dir = os.path.expanduser(SPA_dir)
    return os.listdir(SPA_dir)

def get_stations(package):
    """returns stations dir names in given package"""
    try:
        stations_dir = get_station_path(package, "")
        return os.listdir(stations_dir)
    except FileNotFoundError as fnf_err:
        raise FileNotFoundError(
            "\n\n/station/ dir not found for SPA \""
                + os.path.basename(package) + "\"\n\n",
            fnf_err
        )

def get_station_path(package, station_name=""):
    """ returns path for given station name and package"""
    return os.path.join(
        SPA_DIR, package, 'station', station_name
    )

def list_dependencies(package, station=None, verbose=False):
    """
    Lists the executables ultimately needed by a station.

    package: name of SPA station package directory in SPA_DIR
    station: name of the station in SPA_PACKAGE/stations/ dir
                if not given, lists deps for all station in the package.

    Example usage:
    ```python
    from StationTester.ExtraTools import list_dependencies
    list_dependencies("imars")
    ```
    """
    deps = dict()
    if (station is None):
        for stat in get_stations(package):
            deps[stat] = get_dependencies(package, stat, verbose, skip_errors=True)
    else:
        deps[station] = get_dependencies(package, station, verbose, skip_errors=True)

    print(json.dumps(deps, indent=2))

def get_dependencies(package, station, verbose=False, skip_errors=False):
    station_path=os.path.join(
        get_station_path(package, station), 'station.cfgfile'
    )

    if(verbose): print('loading ', station_path, '...')
    cfgfile = CFGFileReader(station_path)

    return cfgfile.get_station_programs()

def read_params(filename):
    """
    reads in "="-separated key-val pairs like <Ncs_read> or
    configparser without [section headers]

    returns configparser section-head dict
    """
    cfg = configparser.ConfigParser()
    with open(filename) as fp:
        cfg.read_file(itertools.chain(['[global]'], fp), source=filename)
    return cfg['global']

def read_paramstring(paramstring):
    """ reads param key-val pairs from given string """
    cfg = configparser.ConfigParser()
    paramstring = "[global]\n" + paramstring

    cfg.read_string(paramstring)
    # if < py3:
    # buf = StringIO.StringIO(paramstring)
    # cfg.readfp(buf)

    return cfg['global']
