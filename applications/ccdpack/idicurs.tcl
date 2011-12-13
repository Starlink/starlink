#+
#  Name:
#     idicurs.tcl

#  Purpose:
#     Script to do the work for the IDICURS application.

#  Language:
#     Tcl.

#  Description:
#     This script causes the user to be presented with an Ndf viewer
#     so that points on it may be indicated.

#  External Variables:
#     CENTROID = boolean (Given and Returned)
#        Indicates whether points are centroided when added.
#     DOMAIN = string (Given)
#        The domain in which the ndfset is to be plotted, and in which
#        the POINTS coordinates will be supplied and returned in.
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
#     NDFSET = list of strings (Given)
#        A list of strings of the form {setname ndfname ndfname ...}
#        indicating which NDFs comprise the ndfset to be examined.
#        The setname may be an empty string if there is only one NDF.
#     PERCHI = real (Given and Returned)
#        The percentile of the data above which all values should be
#        plotted as the same colour.  Must be between 0 and 100.
#     PERCLO = real (Given and Returned)
#        The percentile of the data below which all values should be
#        plotted as the same colour.  Must be between 0 and 100.
#     POINTS = list of pairs (Given and Returned)
#        A list of the pixel coordinates indicated by the user on the NDF.
#        Each element of the list is of the form {I X Y}, where I is an
#        integer index and X and Y are the coordinates.
#     VERBOSE = integer (Given)
#        If non-zero then the positions of all the points will be written
#        through the CCDPACK log system at the end.
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
#     14-SEP-2000 (MBT):
#        Original version.
#     9-APR-2001 (MBT):
#        Upgraded for use with Sets.
#     {enter_further_changes_here}

#  Bugs:
#     {note_any_bugs_here}

#-

#  Initialise screen.
      wm withdraw .

#  Initialise the error status variable.
      set ERROR ""

#  Get the requested NDF object.
      set ndfset [ eval ndfset $NDFSET ]

#  Create an NDF viewer widget.
      ndfview .viewer \
                      -title "IDICURS: %n" \
                      -percentiles [ list $PERCLO $PERCHI ] \
                      -watchstatus viewstatus \
                      -zoom $ZOOM \
                      -geometry ${WINX}x${WINY} \
                      -markstyle $MARKSTYLE \
                      -centroiding $CENTROID

#  Tinker with cosmetic aspects of the viewer.
      [ .viewer component exit ] configure -balloonstr "Finish adding points"
      [ .viewer component abort ] configure \
          -cmd [ code "set ERROR {User aborted GUI}; .viewer deactivate" ]

#  Add a help control.
      catch { unset helplines }
      lappend helplines \
      "The window can be resized as normal and the scrollbars used to pan" \
      "the viewing region.  The cursor position is displayed below the image." \
      "The controls in the panel are used as follows:" \
      "" \
      "Mouse buttons:" \
      "   Click button 1 (left) on the image to add a point" \
      "   Click button 3 (right) on the image to remove a point" \
      "" \
      "Zoom:" \
      "   Select scale factor or zoom in or out to control magnification" \
      "" \
      "Cutoff:" \
      "   Select a preset or custom cutoff level to control image brightness" \
      "" \
      "Markers:" \
      "   The `Centroid' checkbox controls whether points added by clicking" \
      "   will be centroided on a nearby feature to get the exact position." \
      "   The other controls allow selection of the size, shape and colour" \
      "   of the plotted points, and the index of the next point to be marked."\
      "" \
      "WCS:" \
      "   Select a frame from the ones in the World Coordinate System" \
      "   extension of the image.  This controls the X,Y coordinates displayed"\
      "   which indicate the postion of the cursor, and the grid plotting." \
      "   The `Grid' button lets you select whether grid lines and/or " \
      "   labelled axes are plotted." \
      "" \
      "Control:" \
      "   `Help' shows this window." \
      "   `Abort' exits the program (following confirmation) without writing" \
      "   a position list." \
      "   `Done' indicates that you have finished with this image."
      .viewer configure -helptext [ join $helplines "\n" ]

#  Load the NDF into the widget.
      .viewer loadndf $ndfset $MAXCANV

#  If there is an initial point list, plot the points.
      if { ! [ catch { set POINTS } ] } {
         foreach point $POINTS {
            .viewer addpoint [ lindex $point 1 ] [ lindex $point 2 ] \
                             [ lindex $point 0 ]
         }
      }

#  Set the widget to respond to user actions.
      .viewer activate

#  Wait for the user to indicate that he has finished.
      tkwait variable viewstatus

#  Proceed to pack the results for passing back to the caller if nothing
#  untoward happened.
      if { $ERROR == "" } {

#  Retrieve configuration variables which the user may have changed
#  interactively.
         set ZOOM [ .viewer cget -zoom ]
         set MAXCANV [ .viewer maxcanvas ]
         regexp {^([0-9]+)x([0-9]+)} [ winfo geometry .viewer ] dummy WINX WINY
         set PERCLO [ lindex [ .viewer cget -percentiles ] 0 ]
         set PERCHI [ lindex [ .viewer cget -percentiles ] 1 ]
         set MARKSTYLE [ .viewer cget -markstyle ]
         set CENTROID [ .viewer cget -centroiding ]

#  Set the return variable to contain the list of points selected.  If
#  If we were displaying the indices then return the list with the indices
#  as plotted, otherwise return the list with indices starting from unity
#  and increasing to the number of points plotted.  Log the points to the
#  user while we're at it.
         set POINTS {}
         if { $VERBOSE } { ccdputs -log " " }
         if { [ llength [ .viewer points ] ] == 0 } {
            if { $VERBOSE } { ccdputs -log "    No points marked" }
         } else {
            if { $VERBOSE } {
               set frame [ .viewer cget -wcsframe ]
               set fmt "      %5s   %16s %16s"
               ccdputs -log "    Points marked (coordinate frame $frame):"
               ccdputs -log [ format $fmt "Index" \
                                     [ $ndfset frameatt Symbol(1) $frame ] \
                                     [ $ndfset frameatt Symbol(2) $frame ] ]
            }
            set i 0
            foreach point [ .viewer points ] {
               set i [ lindex $point 0 ]
               set x [ lindex $point 1 ]
               set y [ lindex $point 2 ]
               lappend POINTS [ list $i $x $y ]
               if { $VERBOSE } {
                  set fpos [ lindex [ $ndfset wcstran -format $DOMAIN $frame \
                                                   [ list $x $y ] ] 0 ]
                  set fx [ lindex $fpos 0 ]
                  set fy [ lindex $fpos 1 ]
                  ccdputs -log [ format $fmt $i $fx $fy ]
               }
            }
         }
      }

#  Free resources.
      destroy .viewer
      $ndfset destroy

# $Id$
