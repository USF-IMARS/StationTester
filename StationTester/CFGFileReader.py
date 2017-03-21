#!/usr/bin/env python3
"""
class for reading IPOPP station `.cfgfile`s
"""

import os
from xml.etree import ElementTree

import sys
sys.modules['_elementtree'] = None
# That ^ forces python XML parser not faster C accelerators for
# bc we can't hook the C implementation.
from StationTester.LineNumberingParser import LineNumberingParser

class CFGFileReader(object):
    def __init__(self, cfgfile_path, verbose=False):
        self.path = cfgfile_path
        self.verbose = verbose

        self.tree = ElementTree.parse(self.path, parser=LineNumberingParser())
        self.root = self.tree.getroot()
        self.SETUP = self.root.find('SETUP')
        self.EXECUTE = self.root.find('EXECUTE')

        self.set_varsub(False)

    def get_station_algorithms(self, SPA_DIR=os.path.expanduser("~/drl/SPA")):
        """
        returns list of algorithms loaded via <InitAlgorithm> in the cfgfile

        SPA_DIR: location of SPA directory
        """
        sub_station_programs = []
        for alg in self.SETUP.findall('InitAlgorithm'):
            if(self.verbose): print('\treading InitAlg for ', alg.get('result'))
            filepath = alg.get('file')
            # if (__should_varsub):  # TODO: is this required?
            filepath = filepath.format(
                cfg_nisgs_home=os.path.expanduser('~/drl'),
                spa_dir=SPA_DIR
            )
            sub_station_programs.append(self._formatter(filepath))
        return sub_station_programs

    def get_station_programs(self, skip_errors=False):
        """
        returns: list of all script (sh, py, etc) files this station uses.
        """
        deps=dict()
        for sub_station_file in self.get_station_algorithms():
            if(self.verbose): print('\t\tloading ', sub_station_file, '...')
            try:
                sub_cfg = CFGFileReader(sub_station_file)

                for executable in sub_cfg.root.findall('Executables'):  # should only be one, but findall just in case
                    for child in executable:
                        if(self.verbose): print('\t\t\t', child.text)
                        CFGFileReader._addDepToDict(deps, child.tag, child.text)
            except FileNotFoundError as err:
                if(skip_errors):
                    print(
                        "\n\nERROR: required substation file not found:\n",
                        sub_station_file, "\n\n"
                    )
                else:
                    raise FileNotFoundError

        # get direct calls to Ncs_run
        for runblock in self.EXECUTE.findall('Ncs_run'):
            if (self.verbose): print('.\r')
            dep = runblock.get('cmd').split()[0]
            key = dep.split('{/}')[-1]
            CFGFileReader._addDepToDict(deps, key, dep)
        if (self.verbose): print('\n')

        return deps

    def get_inflows(self):
        """
        returns list of station inflow products by searching for xml of the
            following forms:

            ```xml
            <!-- form 1 : reserveProductLikeProductType -->
            <Dsm_command class="DSM" method="reserveProductLikeProductType">
                <String value="imars.%.mapped.png"/>
                <String value="imars.%.mapped"/>
            </Dsm_command>

            <!-- form 2 : reserveProduct -->
            <Dsm_command class="DSM" method="reserveProduct">
                <String value="drl.aqua.modis.pds"/>
            </Dsm_command>
            ```

        There _should_ be only 1 inflow product per station. This method does
        not assume that, but will print a warning if it finds more than one.
        """
        inflows = []
        xpath_queries = [
        ".//Dsm_command/[@method='reserveProductLikeProductType']",
        ".//Dsm_command/[@method='reserveProduct']"
        ]

        for query in xpath_queries:
            for reservation in self.EXECUTE.findall(query):
                for product in reservation:
                    try:
                        inflows.append(
                            self._formatter(product.get("value"))
                        )
                    except ValueError as verr:
                        if (str(verr) == "value is empty string"):
                            pass
                        else:
                            raise verr

        if len(inflows) > 1:
            print("\n\tWARN: cfgfile has multiple inflow products:", self.path, "\n")

        return inflows

    def get_outflows(self, varsub=False):
        """
        returns list of station outflow products by searching for xml of the
            following form:

            ```xml
            <Dsm_command class="Product" debug="{cfg_debug}" method="new"
                result="drl.aqua.modis.mxd01.OBJ" runFlag="true">
                <Object value="drl.aqua.modis.pds.OBJ"/>
                <String value="drl.aqua.modis.mxd01"/>
            </Dsm_command>
            ```

            varsub:
        """
        outflows = []

        xpath_query = ".//Dsm_command/[@method='new']/[@class='Product']"
        for product in self.EXECUTE.findall(xpath_query):
            try:
                outflows.append(
                    self._formatter(product.find("String").get("value"))
                )
            except ValueError as verr:
                if (str(verr) == "value is empty string"):
                    pass
                else:
                    raise verr

        return outflows

    def set_varsub(self, newval):
        """
         If set True then will attempt to substitute variables in the cfgfile
            when {bracket}s are encountered by reading variable definitions from
            the cfgfile.
        """
        if (newval):
            self._formatter = self._var_sub
        else:
            self._formatter = lambda s: s # formatter does nothing

    def _var_sub(self, string, line=float("inf")):  # TODO: line should be req?
        """
        substitutes variables from file read through given line number
        into given string.

        returns: formatted string with substitutions
        """
        vardict = self._get_var_dict(line)
        try:
            return self._check_val(string.format(vardict))
        except (KeyError) as kerr:
            try:  # it might be an empty string (.format throws KeyError on "")
                varname = kerr.args[0]
                if (vardict[varname] == ""):
                    print("\nWARN: var is empty string.")
                    # remove offending var and try again
                    new_str = string.replace("{" + varname + "}", "")
                    return self._var_sub(new_str, line)
            except KeyError as kerr2:  # it's actually not defined
                print(vardict)
                raise KeyError(
                    "var not defined (see printed varlist)",
                    kerr
                )

    def _check_val(self, value):
        """throws error if value is no good, else pass it through"""
        if (value == ""):
            raise ValueError("value is empty string")
        else:
            return value

    def _get_var_dict(self, line, verbose=None):
        """ returns dict of variable values set through given line # """
        if (verbose is None): verbose = self.verbose

        vardict = dict()
        for elem in self.EXECUTE.iter():
            if True:#elem._end_line_number < line:  # TODO
                if (elem.tag == "Ncs_set"):
                    vardict[elem.attrib["name"]] = elem.attrib["value"]
                else:
                    if verbose:
                        print("ERR: unknown NCS cmd \"", elem.tag, "\"")
                    else:
                        pass
        return vardict

    def _get_var_value(self, key, line):
        """
        reads cfgfile and attempts to get the value of given key at or before
        given line number.
        """

        return self._get_var_dict(line)[key]



    @staticmethod
    def _addDepToDict(deps, newdepkey, newdepval):
        # adds dependency to dict, checks to ensure it doesn't exist first
        try:
            test = deps[newdepkey]  # should throw exception
            raise AssertionError("duplicate key detected in dep list")
        except KeyError:
            deps[newdepkey] = newdepval
