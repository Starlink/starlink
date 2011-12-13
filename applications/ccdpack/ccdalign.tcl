#+
#  Name:
#     ccdalign.tcl

#  Purpose:
#     Script to do the work for the CCDALIGN application.

#  Language:
#     Tcl.

#  Description:
#     This script uses [incr Tcl] classes to present a GUI to the user
#     which allows selection of a matching set of points on each one.

#  External Variables:
#     ERROR = string (Returned)
#        If returned as a non-empty string, this should give the text
#        of an error to be presented to the user.  This indicates that
#        the routine did not complete successfully.
#     MARKSTYLE = string (Given and Returned)
#        A string, in the form of comma-separated att=value pairs,
#        indicating how markers should be plotted on the image.
#     MAXCANV = integer (Given and Returned)
#        The maximum X or Y dimension of the canvas in which the initial
#        NDF is to be displayed.  If zero, there is no limit.
#     NDFSETS = list of lists (Given)
#        Each element of this list represents a set of NDFs which is
#        to be presented to the user for aligning using this script.
#        The format of each element is {setname ndfname ?ndfname ...?}.
#        The setname is a text name of each set which is to be presented
#        to the user (it may be blank, in which case the name of the
#        first NDF will be used), and each ndfname is the name of one
#        of the NDFs which is to be a member of that set.
#     NPOINT = list of integers (Returned)
#        The number of points for each set.  The Nth entry in this
#        list contains the number of points selected for the Nth set
#        in the NDFSETS variable.
#     PERCHI = real (Given and Returned)
#        The percentile of the data above which all values should be
#        plotted as the same colour.  Must be between 0 and 100.
#     PERCLO = real (Given and Returned)
#        The percentile of the data below which all values should be
#        plotted as the same colour.  Must be between 0 and 100.
#     POINTS = list of lists of lists (Returned)
#        The points selected on the images.  The Nth entry in this list
#        contains data for the Nth set in the NDFSETS variable.
#        The entry for each NDF is a list containing one entry for each
#        selected point.  The entry for each point is a three-element
#        list containing the point index number, X coordinate and
#        Y coordinate respectively.  The X and Y coordinates are given
#        in Pixel coordinates if the corresponding element was a single
#        NDF (the ndfset had only one member) and in CCD_SET coordinates
#        if the corresponding ndfset had more than one member.
#     REFSET = integer (Given)
#        The position in the list NDFSETS of the set to be used as the
#        reference image (the first image in the list has index 0).
#     WINX = integer (Given and Returned)
#        X dimension of the window used for NDF display.
#     WINY = integer (Given and Returned)
#        Y dimension of the window used for NDF display.
#     ZOOM = real (Given and Returned)
#        The zoom factor for NDF display; may be limited by the value of
#        MAXCANV.

#  Copyright:
#     Copyright (C) 2000-2001 Central Laboratory of the Research
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
#     MBT: Mark Taylor (STARLINK)
#     {enter_new_authors_here}

#  History:
#     24-JUL-2000 (MBT):
#        Original version.
#     5-APR-2001 (MBT):
#        Upgraded for use with Sets.
#     {enter_further_changes_here}

#  Bugs:
#     {note_any_bugs_here}

#-

#  Sort out arguments.
      set title CCDALIGN
      set ref $REFSET
      set ndfsets $NDFSETS
      set nndfset [ llength $ndfsets ]
      set nother [ expr $nndfset - 1 ]

#  Set defaults for some arguments.
      foreach pair { { ZOOM 1 } { WINX 300 } { WINY 300 } { PERCLO 5 } \
                     { PERCHI 95 } { MAXCANV 0 } { MARKSTYLE "" } } {
         if { ! [ info exists [ lindex $pair 0 ] ] } {
            eval set $pair
         }
      }
      set ERROR ""

#  Initialise screen.
      wm withdraw .

#  Issue instructions about interacting with the GUI.
      ccdputs "  "
      ccdputs "  For each image that is displayed, mark centroidable" \
              " features on it."
      ccdputs "  Add a point by clicking on the image with mouse button 1" \
              " (left),"
      ccdputs "  remove a point by clicking on it with mouse button 3 (right)."
      ccdputs "  Corresponding features should be labelled by the same" \
              " number on all images;"
      ccdputs "  the number of the next point to be added can be changed" \
              " in the toolbar."
      ccdputs "  Click the 'Done' button when you have added all the points" \
              " to an image."
      ccdputs "  "

#  Construct and tweak the viewer for the reference NDF.
      set refndfset [ eval ndfset [ lindex $ndfsets $ref ] ]
      ndfview .vref \
                  -title "$title: %n (reference)" \
                  -watchstatus status$ref \
                  -percentiles [ list $PERCLO $PERCHI ] \
                  -zoom $ZOOM \
                  -markstyle $MARKSTYLE \
                  -geometry ${WINX}x${WINY}
      [ .vref component exit ] configure \
         -balloonstr "Start marking points on other images"
      .vref configure -markstyle showindex=1
      [ .vref component abort ] configure \
         -cmd [ code "set ERROR {User aborted GUI}; .vref deactivate" ]
      catch { unset basichelp }
      lappend basichelp \
   "Use this viewer to mark objects on the image." \
   "" \
   "   Click with mouse button 1 (left) to add a point" \
   "   Click with mouse button 3 (right) to remove a point" \
   "   Use the `Markers' control to change the index of the next point marked" \
   ""
      set helplines $basichelp
      lappend helplines \
   "When you press the `Done' button on the reference image window, you will" \
   "be asked to mark the corresponding objects on an image from each of the" \
   "other groups."
      .vref configure -helptext [ join $helplines "\n" ]

#  Load the reference NDF into the viewer.
      .vref loadndf $refndfset $MAXCANV
      ccdputs -log "  Mark points on the reference image, [ $refndfset name ]:"

#  Get the user to pick points.
      .vref activate
      tkwait variable status$ref
      if { $ERROR == "" } {
         set npref [ llength [ .vref points ] ]
         ccdputs -log "    $npref points marked initially."
         ccdputs -log "  "

#  Get display preferences from the reference viewer so that they can be
#  propagated to subsequent windows and passed back to the calling routine.
         set WINX [ winfo width .vref ]
         set WINY [ winfo height .vref ]
         set ZOOM [ .vref cget -zoom ]
         set MAXCANV [ .vref maxcanvas ]
         set PERCLO [ lindex [ .vref cget -percentiles ] 0 ]
         set PERCHI [ lindex [ .vref cget -percentiles ] 1 ]
         set MARKSTYLE [ .vref cget -markstyle ]

#  Allow the user to muck about with the list for the reference NDF
#  while the later ones are being modified.
         .vref configure -status active
         [ .vref component exit ] configure -state disabled
         [ .vref component abort ] configure -state disabled

#  Try to place the new viewer next to the reference viewer on the screen
#  if it will fit.  If not, positioning is left to the window manager.
#  Note that, at least for some window managers, this places the window
#  in the wrong place by the size of the wm's decorations.  I don't know
#  how to correct this without mapping the window first, which would be
#  a bit untidy.
         set xgap 20
         set ygap 40
         set xsize $WINX
         set ysize $WINY
         set rxpos [ winfo x .vref ]
         set rypos [ winfo y .vref ]
         set minxpos 0
         set minypos 0
         set maxxpos [ expr [ winfo screenwidth .vref ] - $xsize ]
         set maxypos [ expr [ winfo screenheight .vref ] - $ysize ]
         set impossible 0.01
         set geometry ${xsize}x${ysize}
         set setposition 0
         if {    [ set xpos [ expr $rxpos + $xsize + $xgap ] ] <= $maxxpos && \
                 [ set ypos $rypos ] != $impossible \
              || [ set ypos [ expr $rypos + $ysize + $ygap ] ] <= $maxypos && \
                 [ set xpos $rxpos ] != $impossible \
              || [ set xpos [ expr $rxpos - $xsize - $xgap ] ] >= $minxpos && \
                 [ set ypos $rypos ] != $impossible \
              || [ set ypos [ expr $rypos - $ysize - $ygap ] ] >= $minypos && \
                 [ set xpos $rxpos ] != $impossible } {
            set setposition 1
            append geometry +${xpos}+${ypos}
         }

#  Construct a viewer for marking points on the other NDFs.
         ndfview .v \
                    -watchstatus status \
                    -percentiles [ list $PERCLO $PERCHI ] \
                    -zoom $ZOOM \
                    -geometry $geometry \
                    -markstyle $MARKSTYLE
         [ .v component exit ] configure -balloonstr "Finish marking this image"
         [ .v component abort ] configure \
            -cmd [ code "set ERROR {User aborted GUI}; .v deactivate" ]

#  Alter the help text somewhat to reflect the current state of play.
         set helplines $basichelp
         lappend helplines \
   "For each of the images displayed, mark the same objects as have been" \
   "indicated on the reference image.  You do not have to mark all the points" \
   "on each image, but each object marked should have the same index number" \
   "on every image on which you do mark it." \
   "" \
   "When you have finished marking points on each image, press the `Done'" \
   "button, and you will be asked to move on to the next one." \
   "" \
   "The reference image will remain visible, and you can add points to it" \
   "at any time."
         .vref configure -helptext [ join $helplines "\n" ]

#  Associate the help window of the new viewer with the help window of
#  the reference NDF's viewer's window.
         [ .v component help ] configure -redirect [ .vref component help ]

#  Create a warning dialog, which may be used if the user fails to select
#  any points on one of the NDFs.
         set warntext [ join { "No points have been marked on this image."
                               "Registration will be impossible unless"
                               "some points are selected on the image." } "\n" ]
         set warnbox [ iwidgets::messagedialog .warn \
                          -modality application \
                          -bitmap questhead \
                          -title "CCDALIGN: No points" \
                          -master .v \
                          -text $warntext ]
         $warnbox buttonconfigure Cancel -text "Next image"
         $warnbox buttonconfigure OK -text "Try again"
         $warnbox hide Help

#  Loop through the other NDFs getting a list of points from each one.
#  Exclude the reference NDF, since that one stays posted all the time.
         set i 0
         set done 0
         foreach ndfset $ndfsets {
            if { $i != $ref && $ERROR == "" } {
               incr done

#  Configure the viewer.
               set ndfset [ eval ndfset [ lindex $ndfsets $i ] ]
               .v configure -title "$title: %n (#$done/$nother)"
               .v clearpoints
               .v loadndf $ndfset $MAXCANV
               if { $done == 1 && $setposition } {
                  wm deiconify .v
                  wm geometry .v $geometry
               }
               ccdputs -log \
               "  Mark points on image #$done/$nother, [ $ndfset name ]:"

#  Activate the viewer and get the user to mark points.  Ensure that he/she
#  doesn't hit the exit button accidentally without marking any points.
               set tryagain 1
               while { $tryagain && $ERROR == "" } {
                  set tryagain 0
                  .v activate
                  tkwait variable status
                  if { [ llength [ .v points ] ] == 0 && $ERROR == "" } {
                     $warnbox center .v
                     set tryagain [ $warnbox activate ]
                  }
               }

#  Record the position list.
               ccdputs -log "    [ llength [ .v points ] ] points marked."
               ccdputs -log "  "
               set pts($i) [ .v points ]

#  Tidy up.
               $ndfset destroy
            }
            incr i
         }
         destroy .v
      }

      if { $ERROR == "" } {

#  Get list of points for the reference NDF.  We do this last since it may
#  have been modified while the other NDFs are being marked.
         set pts($ref) [ .vref points ]
         if { [ llength $pts($ref) ] != $npref } {
            set npref [ llength $pts($ref) ]
            ccdputs -log \
               "  Changes made to reference image, [ $refndfset name ]:"
            ccdputs -log \
               "    $npref points marked in total."
            ccdputs -log "  "
         }

#  Construct the list of all points for return to calling program.
         for { set i 0 } { $i < $nndfset } { incr i } {
            lappend POINTS $pts($i)
            lappend NPOINT [ llength $pts($i) ]
         }
      }

#  Destroy remaining windows.
      .vref configure -status done
      destroy .vref

# $Id$
