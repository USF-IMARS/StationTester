#!/usr/bin/env python3
"""
tests station graphing functionality
"""

# std modules:
import unittest

# dependencies:
from StationTester.Grapher import Grapher

class Test_station_graph_basics(unittest.TestCase):

    # tests:
    #########################
    def test_graph_single_station(self):
        """ imars img_publisher station node is created """
        grapher = Grapher()
        grapher.graph_station('imars', 'img_publisher')

        self.assertTrue('img_publisher' in list(grapher.graph.nodes()))
