proc CCDGeometry { Top {surekill 1} } {

#+
#  Name:
#     CCDGeometry

#  Purpose:
#     Aid to determining the geometric properties of a CCD frame.

#  Language:
#     TCL

#  Type of Module:
#     Tcl/Tk procedure.

#  Description:
#     This routine displays the given NDF in a GWM canvas item and
#     allows the user to define the positions of the bias strips
#     and the useful CCD area. On exit the CCDglobalpars entries are
#     set to the values chosen.
#
#     There are several additional options for controlling the NDF
#     display, it can be enlarged, or shrunk (this changes the size of
#     the canvas and Gwm widget so counts as real memory), regions can
#     be removed, a colour or grey look-up-table can be applied, the
#     colour of the stipple and outline can be changed as can the actual
#     NDF itself.

#  Arguments:
#     Top = window (read)
#        Name of the top-level widget for containing the canvas item etc.
#     surekill = boolean (read)
#        Whether exit item will also kill . Defaults to true.

#  Global Variables:
#     NDF = string (read and write)
#        Name of the NDF to be displayed.

#  Copyright:
#     Copyright (C) 1995, 1997, 2000-2001 Central Laboratory of the
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
#     25-SEP-1995 (PDRAPER):
#        Original version.
#     15-APR-1997 (PDRAPER):
#        Added CCDimagefilters to support foreign data formats.
#     16-MAY-2000 (MBT):
#        Upgraded for Tcl8.
#     4-JUL-2001 (MBT):
#        Upgraded for Sets (now only DISPLAYs one NDF from an NDG list).
#     01-FEB-2006 (PDRAPER):
#        Changed to use new meta-widget names (s/Ccd_/Ccd::/g).
#     {enter_further_changes_here}

#  Bugs:
#     {note_any_bugs_here}

#-

#  Global variables.
   global BBOX
   global CCDglobalpars
   global CCDgloprefix
   global CCDprefs
   global GWMDEVICE
   global ITEM
   global NDF
   global XDEVICE
   global PERCENTILES

#  Local parameters:
   set high 2e6
   set low -2e6

#.

#  Initialisations.
   if { ! [info exists CCDprefs(stipple)] } {
      set CCDprefs(stipple) red
   }
   if { ! [info exists CCDprefs(outline)] } {
      set CCDprefs(outline) blue
   }
   if { ![info exists PERCENTILES(low)] } {
      set PERCENTILES(low) 5
   }
   if { ![info exists PERCENTILES(high)] } {
      set PERCENTILES(high) 95
   }

#-----------------------------------------------------------------------------
#  Widget creation
#-----------------------------------------------------------------------------

#  Top level.
   CCDCcdWidget Topwin topwin \
      Ccd::toplevel $Top -title "Determine CCD Geometry"
   wm withdraw $topwin

#  Canvas for putting gwm item into add a graphic redrawn context.
   CCDCcdWidget Canvas canvas \
      Ccd::gwm $Topwin.canvas -tags Gwm -redraw 0 -gwmname $GWMDEVICE \
                  -drawcommand "CCDGeomDrawCommand $Topwin $Topwin.canvas"
   if { ! [winfo exists $canvas] } {

#  Creation failed (probably lack of colours).
      CCDIssueInfo "Cannot inspect CCD geometries. Release some display
colours (by closing any colour hogging applications) then try again."
      $Topwin kill $Topwin
      return
   }

#  Help menubar.
   CCDCcdWidget Menu menu Ccd::helpmenubar $Topwin.menubar

#  Display name of NDF that is being shown.
   CCDCcdWidget Label label \
      Ccd::labent $Topwin.label -textvariable NDF -text "Current image:"

#  Action to define bias strip1, strip2 or extent.
   CCDCcdWidget Action action \
      Ccd::radioarray $Topwin.action \
                  -label "Define:" -stack horizontal -variable ITEM

#  Options of various kinds.
   CCDCcdWidget Options options Ccd::choice $Topwin.options -standard false

#  Create rules
   CCDTkWidget Rule1 rule1 frame $topwin.rule1 -height 3
   CCDTkWidget Rule2 rule2 frame $topwin.rule2 -height 3

#  And get out choices.
   CCDCcdWidget Choice choice Ccd::choice $Topwin.choice

#-----------------------------------------------------------------------------
#  Configure widgets.
#-----------------------------------------------------------------------------

#  File items to cancel or accept window and exit interface (note
#  interface exit may nit actually kill . if surekill is false).
      $Menu addcommand File {Close Window} "$Choice invoke Cancel"
      $Menu addcommand File {Accept Window} "$Choice invoke OK"
      if { $surekill } {
         $Menu addcommand File {Exit} CCDExit
      } else {
         $Menu addcommand File {Exit} "$Choice invoke OK"
      }

#  Allow user to redefine the outline and fill colours.
   $Menu addcommand Options {Set outline colour...} \
      "global ITEM
       if { \[info exists ITEM\] } {
          CCDGetColour $Topwin.colour outline
       }
      "
   $Menu addcommand Options {Set stipple colour...} \
      "global ITEM
       if { \[info exists ITEM\] } {
          CCDGetColour $Topwin.colour stipple
       }
      "

#  Set interactive resize.
   $Menu addcheckbutton Options {Resize image when window changes} \
      -command "if { \[$Canvas cget -redraw\] } {
                   $Canvas configure -redraw 0
                } else {
                   $Canvas configure -redraw 1
                }
               "
#  Change the displayed NDF.
   $Menu addcommand Options {Change image...} \
     "global NDF
      global CCDimportfilter
      global CCDimportexists
      global CCDimagefilters
      global DISPLAYED
      if { \[info exists CCDimagefilters\] } {
         set CCDimportfilter \$CCDimagefilters
      } else {
         set CCDimportfilter {*.sdf}
      }
      CCDGetFileName $Topwin.restore {Select an image} 1
      if { \$CCDimportexists } {
         set NDF \"\$CCDimportfile\"
         CCDGeomDrawCommand $Topwin $Canvas
      } else {
         if { \[info exists DISPLAYED\] } {
            set NDF \$DISPLAYED
         } else {
            if {\[winfo exists $choice\] } {
               $Choice invoke Cancel
            }
         }
      }
     "

#  Change the display percentiles.
   $Menu addcommand Options {Change display range...} \
      "CCDGeomSetPercent $Topwin.percent CCDGeomDrawCommand $Topwin $Canvas"


#  <Return> in the NDF name entry changes the value of NDF and redisplays.
   $Label bind entry <Return> \
      "global NDF
       global DISPLAYED
       if { \[file readable \$NDF\] } {
          CCDGeomDrawCommand $Topwin $Canvas
       } else {
          set NDF \$DISPLAYED
       }
      "

#  Define commands to set control for certain actions.
   $Action addbutton "Bias strip 1" bias1
   $Action addbutton "Useful region" extent
   $Action addbutton "Bias strip 2" bias2

#  Token items (these stop problems with existence).
   $Canvas do create rectangle 0 0 0 0 -tags bias1
   $Canvas do create rectangle 0 0 0 0 -tags bias2
   $Canvas do create rectangle 0 0 0 0 -tags extent
   set ITEM bias1

#  Add options for control.
   $Options addbutton "Enlarge" "$Canvas zoom 2.0"
   $Options addbutton "Shrink" "$Canvas zoom 0.5"

#  Remove a region (this is the currently selected one, MB3 does this too).
   $Options addbutton "Remove Region" \
      "global ITEM
       global BBOX
       if { \[info exists ITEM\] } {
          set tags \[$Canvas do gettags \$ITEM\]
          set ITEM \[lindex \$tags 0\]
          if { \$ITEM != \"Gwm\" } {
             $Canvas do delete \$ITEM
             unset BBOX(\$ITEM)
             unset ITEM
          }
       }
      "

#  Colour map. Offer grey and colour.
   $Options addbutton "Grey" "CCDRunTask lutable \
          \"device=$XDEVICE mapping=linear coltab=grey\" 3 $Topwin"
   $Options addbutton "Colour" "CCDRunTask lutable \
          \"device=$XDEVICE mapping=linear coltab=colour\" 3 $Topwin"

#  Bind button press to a new region.
   $Canvas bind canvas <ButtonPress-1> \
      "global ITEM
       global BBOX
       global CCDprefs
       if { \[info exists ITEM\] } {
          if { \$ITEM != \"Gwm\" } {
             $Canvas do delete \$ITEM
          } else {
              set ITEM bias1
          }
       } else {
          set ITEM bias1
       }
       set newx \[$Canvas do canvasx %x\]
       set newy \[$Canvas do canvasy %y\]
       $Canvas do create rectangle \$newx \$newy \$newx \$newy \
          -outline \$CCDprefs(outline) -fill \$CCDprefs(stipple) \
          -stipple gray25 -tags \$ITEM

       set BBOX(\$ITEM) \"\$newx \$newy \$newx \$newy\"
      "

#  Button-1 motion defines a region.
   $Canvas bind canvas <B1-Motion> \
      "global ITEM
       global BBOX
       if { \[info exists ITEM\] } {
          if { \$ITEM != \"Gwm\" } {
             set coords \[$Canvas do coords \$ITEM\]
             if { \[llength \$coords\] > 3 } {
                set newx \[$Canvas do canvasx %x\]
                set newy \[$Canvas do canvasy %y\]
                set cor \[lreplace \$coords 2 3 \$newx \$newy\]
                eval $Canvas do coords \$ITEM \$cor
                set BBOX(\$ITEM) \"\$cor\"
             }
          }
       }
      "

#  Destroy an item (not the gwm one) using mouse button 2.
   $Canvas bind canvas <ButtonPress-3> \
      "global ITEM
       set newx \[$Canvas do canvasx %x\]
       set newy \[$Canvas do canvasy %y\]
       set ITEM \[$Canvas do find closest \$newx \$newy\]
       $Options invoke {Remove Region}
      "

#  Get out.
   $Choice addcommand Cancel "$Topwin kill $Topwin"

#  Accept the chosen regions. Want to be a bit clever about this and check
#  that the bias strips are parallel and that regions are within the bounds
#  of the image.
   $Choice addcommand OK \
      "if { \[CCDGeomCheckandExit\] } {
          $Topwin kill $Topwin
       }
      "

#----------------------------------------------------------------------------
#  Define help.
#----------------------------------------------------------------------------
   $Topwin sethelp ccdpack CCDGeometryWindow
   $Menu sethelpitem {On Window} ccdpack CCDGeometryWindow
   $Menu sethelp all ccdpack CCDGeometryMenu
   $Canvas sethelp ccdpack CCDGeometryGwm
   $Label sethelp ccdpack CCDGeometryNDF

#-----------------------------------------------------------------------------
#  Pack widgets.
#-----------------------------------------------------------------------------
   pack $menu -fill x
   pack $choice -side bottom -fill x
   pack $label -fill x
   pack $action -fill x
   pack $rule2 -fill x
   pack $options -fill x -side bottom
   pack $canvas -fill both -expand true
   pack $rule2 -fill x

#-----------------------------------------------------------------------------
#  Set interface to existing config.
#-----------------------------------------------------------------------------

#  Check CCDprefs defining bias strips and useful area already.
   if { [info exists CCDglobalpars(${CCDgloprefix}EXTENT)] } {
      if { $CCDglobalpars(${CCDgloprefix}EXTENT) != "UNKNOWN" &&
           $CCDglobalpars(${CCDgloprefix}EXTENT) != "" } {
         set l [split $CCDglobalpars(${CCDgloprefix}EXTENT) ","]
         set x1 [expr [lindex $l 0] -0.5]
         set x2 [expr [lindex $l 1] -0.5]
         set y1 [expr [lindex $l 2] -0.5]
         set y2 [expr [lindex $l 3] -0.5]
         set BBOX(extent) "$x1 $y1 $x2 $y2 world"
      }
   }
   if { [info exists CCDglobalpars(${CCDgloprefix}BOUNDS)] } {
      if { $CCDglobalpars(${CCDgloprefix}BOUNDS) != "UNKNOWN" &&
           $CCDglobalpars(${CCDgloprefix}BOUNDS) != "" } {
         set l [split $CCDglobalpars(${CCDgloprefix}BOUNDS) ","]
         set b1 [expr [lindex $l 0] -0.5]
         set b2 [expr [lindex $l 1] -0.5]
         if { [llength $l] > 2 } {
            set b3 [expr [lindex $l 2] -0.5]
            set b4 [expr [lindex $l 3] -0.5]
         }
         if { $CCDglobalpars(${CCDgloprefix}DIRECTION) == "X" } {
            set BBOX(bias1) "$b1 $low $b2 $high world"
            if { [llength $l] > 2 } {
               set BBOX(bias2) "$b3 $low $b4 $high world"
            }
         } else {
            set BBOX(bias1) "$low $b1 $high $b2 world"
            if { [llength $l] > 2 } {
               set BBOX(bias2) "$low $b3 $high $b4 world "
            }
         }
      }
   }

#  Default look-up-table to grey.
   $Options invoke Grey

#  And reveal window
   wm deiconify $topwin

#  If no NDF has been chosen then get one now.
   if { ! [info exists NDF] } {
      set NDF ""
   }
   if { ![ file readable $NDF] } {
      $Menu invoke Options 4
   } else {

#  Issue the draw command for this NDF.
      CCDGeomDrawCommand $Topwin $Canvas
   }

#  And wait for interaction to end.
   if { [winfo exists $topwin] } {
      CCDWindowWait $Topwin
   }

#  End of procedure.

}

# $Id$
