proc CCDGeometry { Top {surekill 1} } {

#+
#  Name:
#     CCDGeometry

#  Type of Module:
#     Tcl/Tk procedure.

#  Purpose:
#     Aid to determining the geometric properties of a CCD frame.

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

#  Global variables:
#     NDF = string (read and write)
#        Name of the NDF to be displayed.

#  Authors:
#     PDRAPER: Peter Draper (STARLINK - Durham University)
#     {enter_new_authors_here}

#  History:
#     25-SEP-1995 (PDRAPER):
#     	 Original version.
#     15-APR-1997 (PDRAPER):
#        Added CCDimagefilters to support foreign data formats.
#     {enter_further_changes_here}

#-

#  Global variables.
   global BBOX
   global CCDglobalpars
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
   Ccd_toplevel $Top -title {Determine CCD Geometry}
   wm withdraw $Top

#  Canvas for putting gwm item into add a graphic redrawn context.
   set Canvas [Ccd_gwm $Top.canvas -tags Gwm -redraw 0 -gwmname $GWMDEVICE \
                  -drawcommand "CCDGeomDrawCommand $Top $Top.canvas"]
   if { ! [winfo exists $Top.canvas] } { 

#  Creation failed (probably lack of colours).
      CCDIssueInfo "Cannot inspect CCD geometries. Release some display       
colours (by closing any colour hogging applications) then try again."
      $Top kill $Top
      return
   }

#  Help menubar.
   set Menu [Ccd_helpmenubar $Top.menubar]

#  Display name of NDF that is being shown.
   set Label [Ccd_labent $Top.label \
                 -textvariable NDF -text "Current image:"]

#  Action to define bias strip1, strip2 or extent.
   set Action [Ccd_radioarray $Top.action \
                  -label "Define:" -stack horizontal -variable ITEM]

#  Options of various kinds.
   set Options [Ccd_choice $Top.options -standard false]

#  Create rules
   set Rule1 [frame $Top.rule1 -height 3]
   set Rule2 [frame $Top.rule2 -height 3]

#  And get out choices.
   set Choice [Ccd_choice $Top.choice]

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
          CCDGetColour $Top.colour outline
       }
      "
   $Menu addcommand Options {Set stipple colour...} \
      "global ITEM
       if { \[info exists ITEM\] } {
          CCDGetColour $Top.colour stipple
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
      CCDGetFileName $Top.restore {Select an image}
      if { \$CCDimportexists } {
         set NDF \"\$CCDimportfile\"
         CCDGeomDrawCommand $Top $Canvas 
      } else {
         if { \[info exists DISPLAYED\] } {
            set NDF \$DISPLAYED
         } else {
            if {\[winfo exists $Choice\] } {
               $Choice invoke Cancel
            }
         }
      }
     "

#  Change the display percentiles.
   $Menu addcommand Options {Change display range...} \
      "CCDGeomSetPercent $Top.percent CCDGeomDrawCommand $Top $Canvas"


#  <Return> in the NDF name entry changes the value of NDF and redisplays.
   $Label bind entry <Return> \
      "global NDF
       global DISPLAYED
       if { \[file readable \$NDF\] } {
          CCDGeomDrawCommand $Top $Canvas
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
          \"device=$XDEVICE mapping=linear coltab=grey\" 3 $Top"
   $Options addbutton "Colour" "CCDRunTask lutable \
          \"device=$XDEVICE mapping=linear coltab=colour\" 3 $Top"

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
   $Choice addcommand Cancel "$Top kill $Top"

#  Accept the chosen regions. Want to be a bit clever about this and check
#  that the bias strips are parallel and that regions are within the bounds
#  of the image.
   $Choice addcommand OK \
      "if { \[CCDGeomCheckandExit\] } {
          $Top kill $Top
       }
      "

#----------------------------------------------------------------------------
#  Define help.
#----------------------------------------------------------------------------
   $Top sethelp ccdpack CCDGeometryWindow
   $Menu sethelpitem {On Window} ccdpack CCDGeometryWindow
   $Menu sethelp all ccdpack CCDGeometryMenu
   $Canvas sethelp ccdpack CCDGeometryGwm
   $Label sethelp ccdpack CCDGeometryNDF

#-----------------------------------------------------------------------------
#  Pack widgets.
#-----------------------------------------------------------------------------
   pack $Menu -fill x
   pack $Choice -side bottom -fill x
   pack $Label -fill x
   pack $Action -fill x
   pack $Rule2 -fill x
   pack $Options -fill x -side bottom
   pack $Canvas -fill both -expand true
   pack $Rule2 -fill x

#-----------------------------------------------------------------------------
#  Set interface to existing config.
#-----------------------------------------------------------------------------

#  Check CCDprefs defining bias strips and useful area already.
   if { [info exists CCDglobalpars(EXTENT)] } {
      if { $CCDglobalpars(EXTENT) != "UNKNOWN" && 
           $CCDglobalpars(EXTENT) != "" } { 
         set l [split $CCDglobalpars(EXTENT) ","]
         set x1 [expr [lindex $l 0] -0.5]
         set x2 [expr [lindex $l 1] -0.5]
         set y1 [expr [lindex $l 2] -0.5]
         set y2 [expr [lindex $l 3] -0.5]
         set BBOX(extent) "$x1 $y1 $x2 $y2 world"
      }
   }
   if { [info exists CCDglobalpars(BOUNDS)] } {
      if { $CCDglobalpars(BOUNDS) != "UNKNOWN" && 
           $CCDglobalpars(BOUNDS) != "" } { 
         set l [split $CCDglobalpars(BOUNDS) ","]
         set b1 [expr [lindex $l 0] -0.5]
         set b2 [expr [lindex $l 1] -0.5]
         if { [llength $l] > 2 } {
            set b3 [expr [lindex $l 2] -0.5]
            set b4 [expr [lindex $l 3] -0.5]
         }
         if { $CCDglobalpars(DIRECTION) == "X" } {
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
   wm deiconify $Top

#  If no NDF has been chosen then get one now.
   if { ! [info exists NDF] } {
      set NDF ""
   }
   if { ![ file readable $NDF] } {
      $Menu invoke Options 4
   } else {

#  Issue the draw command for this NDF.
      CCDGeomDrawCommand $Top $Canvas
   }

#  And wait for interaction to end.
   if { [winfo exists $Top] } {
      CCDWindowWait $Top
   }

#  End of procedure.

}

# $Id$
