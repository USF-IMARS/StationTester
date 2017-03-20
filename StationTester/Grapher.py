#!/usr/bin/env python3
"""
tests station graphing functionality
"""

import networkx as netx

from StationTester.CFGFileReader import CFGFileReader
from StationTester import path_helper

class Grapher(object):
    def __init__(self):
        self.graph = netx.Graph()

    def graph_station(self, packageName, stationName):
        """
        produces graph of given station in/outflows
        """
        cfgfile = CFGFileReader(path_helper.cfg_path(packageName, stationName))

        self.graph.add_node(stationName)
        # TODO: add these attribs to node
        # <Ncs_set name="cfg_stationName" value="imars_img_publisher"/>
        # <Ncs_set name="cfg_groupTag" value="imars_img_publisher"/>
        # <Ncs_set name="version" value="ImgPublisher"/>
        # <Ncs_set name="cfg_stationLog" value="station.stationlog"/>

        # connect inflow product to station
        for inflow in cfgfile.get_inflows():
            self.graph.add_edge(inflow, stationName)

        # connect station to outflow products
        for outflow in cfgfile.get_outflows():
            self.graph.add_edge(stationName, outflow)

    def save(filepath):
        """ saves graph at given filepath """
        netx.write_gexf(self.graph, filepath)
