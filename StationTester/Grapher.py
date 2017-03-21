#!/usr/bin/env python3
"""
tests station graphing functionality
"""

import argparse
import networkx as netx

from StationTester.CFGFileReader import CFGFileReader
from StationTester import path_helper, util

class Grapher(object):
    def __init__(self, verbose=False):
        self.verbose = verbose
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
        cfgfile = CFGFileReader(path_helper.cfg_path(packageName, stationName))
        cfgfile.set_varsub(varsub)

        self.graph.add_node(stationName)
        # TODO: add these attribs to node
        # <Ncs_set name="cfg_stationName" value="imars_img_publisher"/>
        # <Ncs_set name="cfg_groupTag" value="imars_img_publisher"/>
        # <Ncs_set name="version" value="ImgPublisher"/>
        # <Ncs_set name="cfg_stationLog" value="station.stationlog"/>

        # connect inflow product to station
        for inflow in cfgfile.get_inflows():
            if self.verbose: print(inflow, "=>", stationName)
            self.graph.add_edge(inflow, stationName)

        # connect station to outflow products
        for outflow in cfgfile.get_outflows():
            if self.verbose: print(stationName, "=>", outflow)
            self.graph.add_edge(stationName, outflow)

    def save(self, filepath):
        """ saves graph at given filepath """
        netx.write_gexf(self.graph, filepath)


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description='graph SPA station(s) in/outflows')

    parser.add_argument("-p", "--package", help="name of SPA package to graph")
    parser.add_argument("-s", "--station", help="name of station to graph")
    parser.add_argument("-v", "--verbose", help="increase output verbosity",
                        action="store_true"
    )
    parser.add_argument("-n", "--nosub",
        help="do not use variable substitution when reading cfgfile(s)",
        action="store_true"
    )
    parser.add_argument("outfile", help="file to save output")

    args = parser.parse_args()

    graph = Grapher(verbose=args.verbose)
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
