proc CCDGeomCheckandExit {} {
#+
#  Name:
#     CCDGeomCheckandExit.

#  Purpose:
#     Checks the chosen CCD regions.

#  Language:
#     TCL

#  Type of Module:
#     Tcl/Tk procedure.

#  Description:
#     This routine checks that the chosen CCD regions comply with the
#     those expected for use in CCDPACK and also converts the values
#     to those usable by CCDPACK.
#
#     It checks that the bias strip long axes are parallel and do not
#     exceed the bounds of the CCD. It also checks that the chosen
#     useful area is within the CCD bounds. Warnings are issued for
#     non-serious problems and an error if the bias strips are not
#     parallel.

#  Arguments:
#     none

#  Return Value:
#     CCDGeomCheckandExit
#        Returns true if the regions are acceptable and false otherwise.
#        Suitable message are displayed if non-compliance is found.

#  Copyright:
#     Copyright (C) 1995, 1997, 2001 Central Laboratory of the Research
#     Councils. All Rights Reserved.

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
#     29-SEP-1995 (PDRAPER):
#        Original version.
#     16-APR-1997 (PDRAPER):
#        Added traps for bias strip extents set to 0.
#     4-JUL-2001 (MBT):
#        Upgraded for use with Sets.
#     {enter_further_changes_here}

#  Bugs:
#     {note_any_bugs_here}

#-

#  Global variables.
   global BBOX
   global CCDglobalpars
   global CCDgloprefix
#.

#  Check that BBOX exists.
   set ok 1
   if { [info exists BBOX] } {

#  Get size of image.
      set x1 [lindex $BBOX(image) 0]
      set y1 [lindex $BBOX(image) 1]
      set x2 [lindex $BBOX(image) 2]
      set y2 [lindex $BBOX(image) 3]
      set xmin [CCDMin $x1 $x2]
      set xmax [CCDMax $x1 $x2]
      set ymin [CCDMin $y1 $y2]
      set ymax [CCDMax $y1 $y2]

#  Check this against the extent.
      if { [info exists BBOX(extent)] } {
         set extent [eval CCDGeomTransform world $BBOX(extent)]
         set x1 [expr round([lindex $extent 0] +0.5)]
         set y1 [expr round([lindex $extent 1] +0.5)]
         set x2 [expr round([lindex $extent 2] +0.5)]
         set y2 [expr round([lindex $extent 3] +0.5)]
	 set exmin [CCDMin $x1 $x2]
	 set exmax [CCDMax $x1 $x2]
	 set eymin [CCDMin $y1 $y2]
	 set eymax [CCDMax $y1 $y2]
         if { $exmin < $xmin || $eymin < $ymin || \
                 $exmax > $xmax || $eymax > $ymax } {
            CCDIssueInfo \
               "Warning: useful CCD area extends outside of the image"
         }
         set CCDglobalpars(${CCDgloprefix}EXTENT) "$exmin,$exmax,$eymin,$eymax"
      }

#  Start to check bias strips.
      set havebias1 0
      if { [info exists BBOX(bias1)] } {
         set havebias1 1

#  Transform to world coordinates.
         set bias1 [eval CCDGeomTransform world $BBOX(bias1)]

#  Extract the bias strip regions as pixel indices.
         set x1 [expr round([lindex $bias1 0] +0.5)]
         set y1 [expr round([lindex $bias1 1] +0.5)]
         set x2 [expr round([lindex $bias1 2] +0.5)]
         set y2 [expr round([lindex $bias1 3] +0.5)]

#  Check sense of values.
	 set b1x1 [CCDMin $x1 $x2]
	 set b1x2 [CCDMax $x1 $x2]
	 set b1y1 [CCDMin $y1 $y2]
	 set b1y2 [CCDMax $y1 $y2]

#  Work out it aspect ratio.
         set failed [catch {set b1aspect [expr double($b1x2-$b1x1)/double($b1y2-$b1y1)] } ]
         if { $failed } {
            set b1aspect 1
         }
      }
      set havebias2 0
      if { [info exists BBOX(bias2)] } {

#  Transform to world coordinates.
         set bias2 [eval CCDGeomTransform world $BBOX(bias2)]

#  Extract the bias strip regions as pixel indices.
         if { $havebias1 } {
            set x1 [expr round([lindex $bias2 0] +0.5)]
            set y1 [expr round([lindex $bias2 1] +0.5)]
            set x2 [expr round([lindex $bias2 2] +0.5)]
            set y2 [expr round([lindex $bias2 3] +0.5)]
	    set b2x1 [CCDMin $x1 $x2]
	    set b2x2 [CCDMax $x1 $x2]
	    set b2y1 [CCDMin $y1 $y2]
	    set b2y2 [CCDMax $y1 $y2]
            set failed [catch  {set b2aspect [expr double($b2x2-$b2x1)/double($b2y2-$b2y1)]}]
            if { $failed } {
               set b2aspect 1
            }
            set havebias2 1
         } else {
            set x1 [expr round([lindex $bias2 0] +0.5)]
            set y1 [expr round([lindex $bias2 1] +0.5)]
            set x2 [expr round([lindex $bias2 2] +0.5)]
            set y2 [expr round([lindex $bias2 3] +0.5)]
	    set b1x1 [CCDMin $x1 $x2]
            set b1x2 [CCDMax $x1 $x2]
            set b1y1 [CCDMin $y1 $y2]
            set b1y2 [CCDMax $y1 $y2]
            set failed [catch  {set b1aspect [expr double($b1x2-$b1x1)/double($b1y2-$b1y1)]}]
            if { $failed } {
               set b1aspect 1
            }
            set havebias1 1
         }
      }

#  Check aspect ratios if appropriate.
      if { $havebias1 && $havebias2 } {
         if { $b1aspect > 1.0 && $b2aspect < 1.0 ||
              $b1aspect < 1.0 && $b2aspect >1.0 } {

#  Mismatch.
            CCDIssueInfo \
{  The bias strips as you have defined them do not run parallel (ie. along \
   opposite sides of the CCD). Redefine them to do so or remove one.}
            set ok 0
         }
      }

#  Proceed to process the strips if ok.
      if { $ok } {
         if { $havebias1 } {

#  What is the readout direction?
            if { $b1aspect < 1 } {
               set direction X
               set CCDglobalpars(${CCDgloprefix}DIRECTION) X

#  Find the maximum and minimum to check against image size.
               if { $havebias2 } {
                  set max [CCDMax $b1x1 $b1x2 $b2x1 $b2x2]
                  set min [CCDMin $b1x1 $b1x2 $b2x1 $b2x2]
                  set CCDglobalpars(${CCDgloprefix}BOUNDS) \
                      "$b1x1,$b1x2,$b2x1,$b2x2"
               } else {
                  set max [CCDMax $b1x1 $b1x2]
                  set min [CCDMin $b1x1 $b1x2]
                  set CCDglobalpars(${CCDgloprefix}BOUNDS) "$b1x1,$b1x2"
               }

#  Compare these with image. Allow failure as software will cope
               if { $max > $xmax } {
                  CCDIssueInfo "Warning: bias strips extend above X image limit"
               }
               if { $min < $xmin } {
                  CCDIssueInfo "Warning: bias strips extend below X image limit"
               }
            } else {
               set direction Y
               set CCDglobalpars(${CCDgloprefix}DIRECTION) Y
               if { $havebias2 } {
                  set max [CCDMax $b1y1 $b1y2 $b2y1 $b2y2]
                  set min [CCDMin $b1y1 $b1y2 $b2y1 $b2y2]
                  set CCDglobalpars(${CCDgloprefix}BOUNDS) \
                      "$b1y1,$b1y2,$b2y1,$b2y2"
               } else {
                  set max [CCDMax $b1y1 $b1y2]
                  set min [CCDMin $b1y1 $b1y2]
                  set CCDglobalpars(${CCDgloprefix}BOUNDS) "$b1y1,$b1y2"
               }

#  Compare these with image. Allow failure as software will cope
               if { $max > $ymax } {
                  CCDIssueInfo "Warning: bias strips extend above Y image limit"
               }
               if { $min < $ymin } {
                  CCDIssueInfo "Warning: bias strips extend below Y image limit"
               }
            }
         }
      }
   } else {
      CCDIssueInfo "No information about the CCD geometry is available"
      set ok 0
   }

#  End of procedure.
   return $ok
}
# $Id$
