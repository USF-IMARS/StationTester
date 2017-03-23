#!/usr/bin/env python3
"""
tests station graphing functionality
"""

import argparse
from enum import Enum

import networkx as netx

from StationTester.CFGFileReader import CFGFileReader
from StationTester import path_helper, util

NodeType = Enum("NodeType", "STATION PRODUCT")

class Grapher(object):
    def __init__(self, verbocity=0):
        self.verbose = (verbocity > 0)
        self.verbocity = verbocity
        self.graph = netx.DiGraph()

    def graph_all(self, varsub=True):
        """ graphs all SPAs installed in the spa_dir """
        for packageName in util.get_packages():
            try:
                self.graph_SPA(packageName, varsub=varsub)
            except FileNotFoundError as err:
                print("\n\tWARN: no stations found for SPA \"", packageName, "\"")

    def graph_SPA(self, packageName, varsub=True):
        """
        produces graph of in/outfows for all stations in given SPA
        """
        for stationName in util.get_stations(packageName):
            self.graph_station(packageName, stationName, varsub=varsub)

    def graph_station(self, packageName, stationName, varsub=True):
        """
        produces graph of given station in/outflows
        """
        if self.verbose: print("graphing "+packageName+"/"+stationName+"...")
        cfgfile = CFGFileReader(
            path_helper.cfg_path(packageName, stationName),
            verbose=(self.verbocity > 1)
        )
        cfgfile.set_varsub(varsub)

        self._add_node(stationName, NodeType.STATION, cfgfile)

        # connect inflow product to station
        for inflow in cfgfile.get_inflows():
            if self.verbose: print(inflow, "=>", stationName)
            self._add_node(inflow, NodeType.PRODUCT, cfgfile)
            self._add_edge(inflow, stationName)

        # connect station to outflow products
        for outflow in cfgfile.get_outflows():
            if self.verbose: print(stationName, "=>", outflow)
            self._add_node(outflow, NodeType.PRODUCT, cfgfile)
            self._add_edge(stationName, outflow)

    def save(self, filepath):
        """ saves graph at given filepath """
        netx.write_gexf(self.graph, filepath, version="1.2draft")

    def _add_node(self, name, node_type, cfgfile):
        attribs = {"size":1}
        if node_type == NodeType.STATION:
            attribs["group"]=cfgfile.get_global_attrib("cfg_groupTag")
            attribs["version"]=cfgfile.get_global_attrib("version")
        elif node_type == NodeType.PRODUCT:
            attribs["group"]="product"
            attribs["version"]="0.0.0"
            # TODO: check for other products matching wld.%.card values here
            # TODO: maybe w/ self.graph.nodes()?
        else:
            raise ValueError("Unknown NodeType")

        if not self.graph.has_node(name):
            if self.verbocity > 1: print("newNode:" + name)
            self.graph.add_node(name, attribs)
        else:
            if self.verbocity > 1: print(name + "++")

    def _add_edge(self, from_node, to_node):
        self.graph.add_edge(
            from_node,
            to_node,
            {"Size":1}
        )


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description='graph SPA station(s) in/outflows')

    parser.add_argument("-p", "--package", help="name of SPA package to graph")
    parser.add_argument("-s", "--station", help="name of station to graph")
    parser.add_argument("-v", "--verbose", help="increase output verbosity",
                        action="count",
                        default=0
    )
    parser.add_argument("-n", "--nosub",
        help="do not use variable substitution when reading cfgfile(s)",
        action="store_true"
    )
    parser.add_argument("outfile", help="file to save output")

    args = parser.parse_args()

    graph = Grapher(verbocity=args.verbose)
    if (args.package is None):
        if (args.station is not None):
            print("\n\tERR: must provide package along with station name\n")
            parser.print_help()
            quit()
        else:
            graph.graph_all(varsub=(not args.nosub))
    else:
        if (args.station is None):
            graph.graph_SPA(args.package, varsub=(not args.nosub))
        else:
            graph.graph_station(args.package, args.station, varsub=(not args.nosub))
    graph.save(args.outfile)
    print(".gexf file saved. Open w/ gephi (or other) to view.")
