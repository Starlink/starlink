proc ccdGetGeometry {} {
#+
#  Name:
#    ccdGetGeometry

#  Purpose:
#    Gets an interactive estimate of the CCD geometries.

#  Language:
#     Tcl script.

#  Invocation:
#     ccdGetGeometry

#  Description:
#     This routine runs up a invocation of the geometry 

#  Authors:
#     PDRAPER: Peter Draper (STARLINK - Durham University)
#     {enter_new_authors_here}

#  History:
#     10-OCT-1995 (PDRAPER):
#        Original version.
#     {enter_changes_here}

#  Bugs:
#     {note_any_bugs_here}

#-

#  Global variables.
   global CCDglobalpars
   global TASK
#.

#  Initialise the CCD geometries.
   set CCDglobalpars(EXTENT) ""
   set CCDglobalpars(BOUNDS) ""
   set CCDglobalpars(DIRECTION) ""

#  Start the geometry application and keeping the output so that we
#  can parse it for the values.
   ccdRunTask geometry
   if { $TASK(geometry,error) == "" } { 

#  Parse output for relevant bits.
      foreach line [split "$TASK(geometry,output)" "\n"] {
         if { [regexp {Useful CCD area = ([^ ]*)} $line all match] } {
            set CCDglobalpars(EXTENT) "$match"
         } elseif { [regexp {Readout direction = ([^ ]*)} $line all match] } {
            set CCDglobalpars(DIRECTION) "$match"
         } elseif { [regexp {Bias strip bounds = ([^ ]*)} $line all match] } {
            set CCDglobalpars(BOUNDS) "$match"
         }
      }
   } else { 
      puts stderr "!! Failed to obtain CCD geometry"
   }

#  End of procedure.
}
# $Id$
