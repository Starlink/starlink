#+
#  Name:
#     gaia_util

#  Purpose:
#     Utility procs.

#  Type of Module:
#     Tcl/Tk script.

#  Description:
#     Taken from gaiaMain.tcl by Allan Brighton (ESO)

#  Authors:
#     PDRAPER: Peter Draper (STARLINK)
#     {enter_new_authors_here}

#  History:
#     21-NOV-1996 (PDRAPER):
#        Original version
#     {enter_changes_here}

#-


#  Define a useful procedure for handling image names with slices.
proc fileName {image} {

   # return the proper filename of an image and any slice information.
   set i1 [string last {(} $image]
   set i2  [string last {)} $image]
   if { $i1 > -1 && $i2 > -1 } {
      set slice [string range $image $i1 $i2]
      incr i1 -1
      set image [string range $image 0 $i1]
   } else {
      set slice ""
   }
   set image2 "$image"
   if { [file extension $image] == "" } {
      set image "${image}.sdf"
   }
   return [list $image $slice]
}
