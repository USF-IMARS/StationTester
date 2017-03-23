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

class CFGFileValueError(ValueError):
    """bad value read from cfgfile"""

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
            except FileNotFoundError as fnferr:
                if(skip_errors):
                    print(
                        "\n\nERROR: required substation file not found:\n",
                        sub_station_file, "\n\n"
                    )
                else:
                    raise fnferr

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
                    self._try_append_str(product.get("value"), inflows)

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
            self._try_append_str(product.find("String").get("value"), outflows)

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
            self._formatter = self._check_val

    def _var_sub(self, string, line=float("inf")):  # TODO: line should be req?
        """
        substitutes variables from file read through given line number
        into given string.

        returns: formatted string with substitutions
        """
        if self.verbose: print("var_sub(", string, ")")
        vardict = self._get_var_dict(line)
        try:
            _ret = self._check_val(string.format(**vardict))
            if self.verbose: print("subbed:", _ret)
            return _ret
        except (KeyError) as kerr:
            try:  # it might be an empty string (.format throws KeyError on "")
                varname = kerr.args[0]
                if (len(vardict[varname]) < 1):
                    print("\nWARN: var \"", varname, "\" is empty string.")
                    # remove offending var and try again
                    new_str = string.replace("{" + varname + "}", "")
                    return self._var_sub(new_str, line)
                else:
                    raise kerr
            except (KeyError) as kerr2:  # it's actually not defined
                print("\n\n\tin cfgfile:", self.path)
                print("\n", vardict)
                print("\n\tERR:var \""+ varname+"\" undefined (varlist above)")
                raise kerr
        assert(False)  # should never reach here...

    def _try_append_str(self, newString, targetList):
        """
        Attempts to add given string to given list after passing through formatter.
        Excepts common issues by simply not appending to the list.
        """
        try:
            formatted_str = self._formatter(newString)
            assert(formatted_str is not None)  # this should have thrown err
            targetList.append(formatted_str)
        except CFGFileValueError as verr:
            if (str(verr) == "value == \"\""):
                pass
            elif (str(verr) == "value == None"):
                pass
            else:
                raise verr

    def _check_val(self, value):
        """throws CFGFileValueError if value is no good, else pass it through"""
        if (len(value) < 1):
            if self.verbose: print("value == \"\"")
            raise CFGFileValueError("value == \"\"")
        elif (value is None):
            if self.verbose: print("value == None")
            raise CFGFileValueError("value == None")
        else:
            if self.verbose: print("value ok")
            return value

    def _get_var_dict(self, line, verbose=None):
        """ returns dict of variable values set through given line # """
        if (verbose is None): verbose = self.verbose

        # starting vars here are defaults included ALWAYS
        vardict = {
            "product_group": "?product_group?",  # else undef in h2g modis_tcolor-2
            "tiff_product_type": "?tiff_product_type?", # else undef in imars img_publisher
            "place_name": "?place_name?", # else undef in imars oc_png

            # NCS-injected:
            "/":"/",
            "cfg_ncs_home": "<cfg_ncs_home>",
            "cfg_prodtype": "<cfg_prodtype>",
            "cfg_stationDirectory": "<cfg_stationDirectory>",

            # read from site.properties:
            "NISGS_SITE_NAME": "$NISGS_SITE_NAME$",
            "DSM_DATA_DIRECTORY": "$DSM_DATA_DIRECTORY$"
        }
        for elem in self.root.iter():
            if True:#elem._end_line_number < line:  # TODO
                if (elem.tag == "Ncs_set"):
                    key = elem.attrib["name"]
                    try:
                        val = elem.attrib["value"].format(**vardict)
                    except (KeyError, ValueError) as kerr:
                        val = elem.attrib["value"]
                        print("\n\tWARN: trouble parsing value")
                    if verbose: print(key, "=", val)
                    vardict[key] = val
                elif(elem.tag == "Ncs_log" or elem.tag == "Ncs_print"):
                    pass
                elif(elem.tag == "Ncs_getMatch"):
                    #    <Ncs_getMatch name="{drl_aqua_modis_pds}" pattern="[^{/}]*$" result="drl_aqua_modis_pds_filename"/>
                    key = elem.attrib["result"]
                    val = elem.attrib["name"]
                    if verbose: print(key, "=", val)
                    vardict[key] = val
                elif(elem.tag == "Dsm_command"):
                    # <Dsm_command class="DSM" debug="{cfg_debug}" method="getProductPath" result="infile_path">
                    #   <Object value="imars.{sat}.{sensor}.{product_family}.mapped.OBJ"/>
                    # </Dsm_command>
                    ####### OR ####################
                    # <Dsm_command class="imars.{sat}.{sensor}.{product_family}.mapped.OBJ"
                    # debug="{cfg_debug}" method="getAttribute" result="area">
                    #     <String value="place_name" />
                    # </Dsm_command>
                    try:
                        key = elem.attrib["result"]

                        val = elem.attrib["method"] + "("
                        for i, param in enumerate(elem.iter()):
                            if (i == 0): # skip first elem (Dsm_command itself)
                                pass
                            else:
                                try:
                                    val += param.attrib["value"] + ", "
                                except KeyError as val_key_err:
                                    # command param has no value???
                                    raise KeyError(param.tag ,valkeyerr)
                        val += ")"

                        if verbose: print(key, "=", val)
                        vardict[key] = val
                    except KeyError as kerr:
                        if kerr.args[0] == "result":
                            pass # call does not save return value anywhere
                        else:
                            raise kerr
                elif(elem.tag == "Ncs_date"):
                     #  	<Ncs_date getValue="start_date name="cfg_starttime" pattern="yyyyDDD"/>
                     if "getValue" in elem.attrib:
                         key = elem.attrib["getValue"]
                         if "name" in elem.attrib:
                             val = elem.attrib["name"]
                         else:  # no name given
                             val = "Ncs_date.getValue()"
                         if verbose: print(key, "=", val)
                         vardict[key] = val
                     elif "setValue" in elem.attrib:
                         pass
                     else:
                         if verbose:
                             print("WARN: unknown NCS usage Ncs_date(", elem.attrib, ")")
                else:
                    if verbose:
                        print("WARN: unknown NCS cmd \"", elem.tag, "\"")
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
