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
