#!/usr/bin/env python3
"""
class for reading IPOPP station `.cfgfile`s
"""

import os

from xml.etree import ElementTree

class CFGFileReader(object):
    def __init__(self, cfgfile_path, verbose=False):
        self.tree = ElementTree.parse(cfgfile_path)
        self.root = self.tree.getroot()
        self.SETUP = self.root.find('SETUP')
        self.EXECUTE = self.root.find('EXECUTE')

        self.verbose = verbose

    def get_station_algorithms(self, SPA_DIR=os.path.expanduser("~/drl/SPA")):
        """
        returns list of algorithms loaded via <InitAlgorithm> in the cfgfile

        SPA_DIR: location of SPA directory
        """
        sub_station_programs = []
        for alg in self.SETUP.findall('InitAlgorithm'):
            if(self.verbose): print('\treading InitAlg for ', alg.get('result'))
            filepath = alg.get('file')
            filepath = filepath.format(
                cfg_nisgs_home=os.path.expanduser('~/drl'),
                spa_dir=SPA_DIR
            )
            sub_station_programs.append(filepath)
        return sub_station_programs

    def get_station_programs(self):
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

    @staticmethod
    def _addDepToDict(deps, newdepkey, newdepval):
        # adds dependency to dict, checks to ensure it doesn't exist first
        try:
            test = deps[newdepkey]  # should throw exception
            raise AssertionError("duplicate key detected in dep list")
        except KeyError:
            deps[newdepkey] = newdepval
