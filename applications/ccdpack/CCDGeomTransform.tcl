proc CCDGeomTransform { type args } {

#+
#  Name:
#     CCDGeomTransform

#  Type of Module:
#     Tcl/Tk procedure.

#  Purpose:
#     Transforms coordinates from canvas to world and vice versa.

#  Description:
#      This routine uses the transform parameters produced by
#      CCDGeomDrawCommand to transform the coordinate list from canvas
#      world or from world to canvas.

#  Arguments:
#      type = string (read)
#        The type of transform to apply. This is either world or Gwm.
#        if world then the transform is to world coordinates.
#      args = list (read)
#        The list of coordinate pairs to transform.

#  Return value
#      CCDGeomTransform = list
#         The transformed values.

#  Authors:
#     PDRAPER: Peter Draper (STARLINK - Durham University)
#     {enter_new_authors_here}

#  History:
#     28-SEP-1995 (PDRAPER):
#     	 Original version.
#     {enter_further_changes_here}

#-

#  Global variables.
   global DEVWorld

#.

   if { [info exists DEVWorld] } {
      set listlength [llength $args]
      if { "$type" == "world" } {
         set newc ""
         for { set i 0 } { $i < $listlength } { incr i } {
            set x [lindex $args $i]
            incr i
            set y [lindex $args $i]

#  Transform to Device from canvas.
	    set y [expr $DEVWorld(high) - $y]

            set x [expr $x*$DEVWorld(xscale) + $DEVWorld(xoffset)]
            set y [expr $y*$DEVWorld(yscale) + $DEVWorld(yoffset)]
            lappend newc $x
            lappend newc $y
         }
      } else {
         set newc ""
         for { set i 0 } { $i < $listlength } { incr i } {
            set x [lindex $args $i]
            incr i
            set y [lindex $args $i]
            set x [expr ($x-$DEVWorld(xoffset))/$DEVWorld(xscale)]
            set y [expr ($y-$DEVWorld(yoffset))/$DEVWorld(yscale)]

	    set y [expr $DEVWorld(high) - $y]

            lappend newc $x
            lappend newc $y
         }
      }
      return "$newc"
   } else { 
      CCDIssueError "CCDGeomTransform invoked before initialization of 
transforms (possible programming error)"
   }

#  End of procedure.
}
# $Id$
