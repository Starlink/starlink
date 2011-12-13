proc CCDGeomDrawCommand { Top Canvas } {
#+
#  Name:
#     CCDGeomDrawCommand

#  Purpose:
#     Draws/redraws the graphic content for CCDGeometry.

#  Language:
#     TCL

#  Type of Module:
#     Tcl/Tk procedure.

#  Description:
#     This routine redraws the graphics in the CCDGeometry canvas. It
#     does this by displaying the image, constructing a new
#     device-world coordinates transform and resizing and displaying
#     any rectangles that have already been drawn. The bounding boxes
#     of the rectangles should be stored in the BBOX global
#     variable. The BBOX coordinates should be world if they are
#     restored from a previous time and in Gwm coordinates otherwise.
#     If the BBOX coordinates are in world already then the lists
#     should contain an extra (unused) value. BBOX(image) is always in
#     world coordinates.

#  Arguments:
#     Top = window (read)
#       Name of the current top-level widget. This will be held
#       when drawing is taking place.
#     Canvas = window (read)
#       Name of the Ccd::gwm canvas in use.

#  Copyright:
#     Copyright (C) 1995-1996, 1999-2000 Central Laboratory of the
#     Research Councils. Copyright (C) 2006 Particle Physics &
#     Astronomy Research Council. All Rights Reserved.

#  Licence:
#     This program is free software; you can redistribute it and/or
#     modify it under the terms of the GNU General Public License as
#     published by the Free Software Foundation; either version 2 of
#     the License, or (at your option) any later version.
#
#     This program is distributed in the hope that it will be
#     useful, but WITHOUT ANY WARRANTY; without even the implied
#     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
#     PURPOSE. See the GNU General Public License for more details.
#
#     You should have received a copy of the GNU General Public License
#     along with this program; if not, write to the Free Software
#     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
#     02110-1301, USA

#  Authors:
#     PDRAPER: Peter Draper (STARLINK - Durham University)
#     MBT: Mark Taylor (STARLINK)
#     {enter_new_authors_here}

#  History:
#     27-SEP-1995 (PDRAPER):
#        Original version.
#     17-JAN-1996 (PDRAPER):
#        Fixed display to reset xmagn and ymagn parameters.
#     24-MAR-1999 (PDRAPER):
#        Removed the xmagn & ymagn changes, no longer needed. Stopped
#        KAPPA drawing axes.
#     16-MAY-2000 (MBT):
#        Upgraded for Tcl8.
#     01-FEB-2006 (PDRAPER):
#        Changed to use new meta-widget names (s/Ccd_/Ccd::/g).
#     {enter_further_changes_here}

#  Bugs:
#     {note_any_bugs_here}

#-

#  Global variables.
   global BBOX
   global CCDprefs
   global DEVWorld
   global DISPLAYED
   global NDF
   global TASK
   global XDEVICE
   global PERCENTILES
#.

#  Redraw the image.
   if { [info exists NDF] } {

#  Trap if we already have any regions drawn. If so transform these to
#  world coordinates (so we can transform back to the new Gwm system
#  later). If the length of these list is 5 then they are a
#  restoration from an old set up and are already in world coordinate.
      foreach region "bias1 bias2 extent" {
         if { [info exists BBOX($region)] } {
            if { [llength $BBOX($region)] != 5 } {
               set BBOX($region) [eval CCDGeomTransform world $BBOX($region)]
            } else {
               set BBOX($region) [lrange $BBOX($region) 0 3]
            }
         }
      }

#  (Re)display the image.
      set ndf [CCDFileToNDFName $NDF]
      set perc "\[$PERCENTILES(low),$PERCENTILES(high)\]"
      CCDRunTask display \
         "in=$ndf mode=per percentiles=$perc device=$XDEVICE axes=f" \
         1 $Top "Displaying image $NDF"
      set DISPLAYED $NDF
      $Canvas do raise Gwm

#  Define the transformations from world to Gwm coordinates and vice versa.
      CCDRunTask picinfo "device=$XDEVICE accept reset" 3 $Top
      set outputlist [split $TASK(picinfo,output) "\n"]
      set length [llength $outputlist]
      set i 0
      while { 1 } {
         set line [lindex $outputlist $i]
         switch $line {
            "  Device pixels:" {
               incr i
               set Xline [lindex $outputlist $i]
               scan $Xline "%s %f %s %f" d1 xdlow d2 xdhigh
               incr i
               set Yline [lindex $outputlist $i]
               scan $Yline "%s %f %s %f" d1 ydlow d2 ydhigh
            }
            "  World coordinates:" {
               incr i
               set Xline [lindex $outputlist $i]
               scan $Xline "%s %f %s %f" d1 xwlow d2 xwhigh
               incr i
               set Yline [lindex $outputlist $i]
               scan $Yline "%s %f %s %f" d1 ywlow d2 ywhigh
            }
         }
         incr i
         if { $i > $length } { break }
      }

#  Create the Gwm-world transform.
      set DEVWorld(xscale) [expr ($xwlow-$xwhigh)/($xdlow-$xdhigh)]
      set DEVWorld(xoffset) [expr $xwlow-($DEVWorld(xscale)*$xdlow)]
      set DEVWorld(yscale) [expr ($ywlow-$ywhigh)/($ydlow-$ydhigh)]
      set DEVWorld(yoffset) [expr $ywlow-($DEVWorld(yscale)*$ydlow)]

#  Height in pixels of Y.
      set DEVWorld(high) $ydhigh

#  Store size of image (not Gwm widget) in world coordinates.
      set BBOX(image) "$xwlow $ywlow $xwhigh $ywhigh"

#  Redraw the regions. These coordinate should be World so transform
#  them to Gwm
      foreach region "bias1 bias2 extent" {
         if { [info exists BBOX($region)] } {
            set BBOX($region) [eval CCDGeomTransform Gwm $BBOX($region)]
            $Canvas do delete $region
            eval $Canvas do create rectangle $BBOX($region) \
               -outline \$CCDprefs(outline) -fill \$CCDprefs(stipple) \
               -stipple gray25 -tags $region
            $Canvas do raise $region
         }
      }
   }

#  End of procedure.
}
#  $Id$
