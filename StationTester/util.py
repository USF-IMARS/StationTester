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
import xml.etree.ElementTree

def get_stations(package):
    stations_dir = os.path.expanduser(
        '~/drl/SPA/{}/station/'.format(package)
    )
    return os.listdir(stations_dir)

def list_dependencies(package, station=None, verbose=False):
    """
    Lists the executables ultimately needed by a station.

    package: name of SPA station package directory in ~/drl/SPA/
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
            deps[stat] = get_dependencies(package, stat, verbose)
    else:
        deps[station] = get_dependencies(package, station, verbose)

    print(json.dumps(deps, indent=2))

def get_dependencies(package, station, verbose=False):
    station_path=os.path.expanduser(
        '~/drl/SPA/{}/station/{}/station.cfgfile'.format(package, station)
    )

    if(verbose): print('loading ', station_path, '...')
    station_tree = xml.etree.ElementTree.parse(station_path)
    station_root = station_tree.getroot()
    station_setup = station_root.find('SETUP')

    # get loaded algorithms
    sub_station_programs = []
    for alg in station_setup.findall('InitAlgorithm'):
        if(verbose): print('\treading InitAlg for ', alg.get('result'))
        filepath = alg.get('file')
        filepath = filepath.format(
            cfg_nisgs_home=os.path.expanduser('~/drl')
        )
        sub_station_programs.append(filepath)

    deps=dict()
    for sub_station_file in sub_station_programs:
        if(verbose): print('\t\tloading ', sub_station_file, '...')
        sub_station_tree = xml.etree.ElementTree.parse(sub_station_file)
        sub_station_root = sub_station_tree.getroot()
        for executable in sub_station_root.findall('Executables'):  # should only be one, but findall just in case
            for child in executable:
                if(verbose): print('\t\t\t', child.text)
                _addDepToDict(deps, child.tag, child.text)

    # get direct calls to Ncs_run
    station_exec = station_root.find('EXECUTE')
    for runblock in station_exec.findall('Ncs_run'):
        if (verbose): print('.\r')
        dep = runblock.get('cmd').split()[0]
        key = dep.split('{/}')[-1]
        _addDepToDict(deps, key, dep)
    if (verbose): print('\n')


    return deps

def _addDepToDict(deps, newdepkey, newdepval):
    # adds dependency to dict, checks to ensure it doesn't exist first
    try:
        test = deps[newdepkey]  # should throw exception
        raise AssertionError("duplicate key detected in dep list")
    except KeyError:
        deps[newdepkey] = newdepval
