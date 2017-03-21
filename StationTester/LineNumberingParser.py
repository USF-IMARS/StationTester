"""
adds line numbers to ElementTree.
from http://stackoverflow.com/a/36430270
"""

import sys
sys.modules['_elementtree'] = None
# That ^ forces python XML parser not faster C accelerators for
# bc we can't hook the C implementation.
from xml.etree import ElementTree as ET

class LineNumberingParser(ET.XMLParser):
    def _start_list(self, *args, **kwargs):
        # Here we assume the default XML parser which is expat
        # and copy its element position attributes into output Elements
        element = super(self.__class__, self)._start_list(*args, **kwargs)
        element._start_line_number = self.parser.CurrentLineNumber
        element._start_column_number = self.parser.CurrentColumnNumber
        element._start_byte_index = self.parser.CurrentByteIndex
        return element

    def _end(self, *args, **kwargs):
        element = super(self.__class__, self)._end(*args, **kwargs)
        element._end_line_number = self.parser.CurrentLineNumber
        element._end_column_number = self.parser.CurrentColumnNumber
        element._end_byte_index = self.parser.CurrentByteIndex
        return element

# tree = ET.parse(
#     "/home/ipopp/drl/SPA/modisl1db/station/l0l1aqua/station.cfgfile",
#     parser=LineNumberingParser()
# )
# root = tree.getroot()
# print('rt: ', root)
# print('dir: ', dir(root), "\n\n")
# print('end: ', root._end_line_number)
# print('start: ', root._start_line_number)
