#+
#  Name:
#     idicurs.tcl
#
#  Purpose:
#     Script to do the work for the IDICURS application.
#
#  Language:
#     Tcl.
#
#  Description:
#     This script causes the user to be presented with an Ndf viewer 
#     so that points on it may be indicated.
#
#  External Variables:
#     DOMAIN = string (Given)
#        The domain in which the ndfset is to be plotted, and in which
#        the POINTS coordinates will be supplied and returned in.
#     MARKSTYLE = string (Given and Returned)
#        A string, in the form of comma-separated att=value pairs, 
#        indicating how markers should be plotted on the image.
#     MAXCANV = integer (Given and Returned)
#        The maximum X or Y dimension of the canvas in which the initial
#        NDF is to be displayed.  If zero, there is no limit.
#     MAXPOS = integer (Given)
#        The maximum number of points which the user may indicate.
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
#
#  Authors:
#     MBT: Mark Taylor (STARLINK)
#
#  History:
#     14-SEP-2000 (MBT):
#        Original version.
#     9-APR-2001 (MBT):
#        Upgraded for use with Sets.
#-

#  Initialise screen.
      wm withdraw .

#  Get the requested NDF object.
      set ndfset [ eval ndfset $NDFSET ]

#  Create an NDF viewer widget.
      ndfview .viewer \
                      -title "IDICURS: %n" \
                      -percentiles [ list $PERCLO $PERCHI ] \
                      -watchstatus viewstatus \
                      -zoom $ZOOM \
                      -maxpoints $MAXPOS \
                      -geometry ${WINX}x${WINY} \
                      -markstyle $MARKSTYLE

#  Tinker with cosmetic aspects of the viewer.
      [ .viewer component exit ] configure -balloonstr "Finish adding points"

#  Add a help control.
      catch { unset helplines }
      lappend helplines \
         "Click with mouse button 1 (left) on the image to add a point" \
         "Click with mouse button 3 (right) on the image to remove a point" \
         "" \
         "Use the `Markers' controls to set the shape of the markers" \
         "and change the index of the next point marked." \
         "" \
         "Click the `Done' button to finish marking points and end the program"
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

#  Retrieve configuration variables which the user may have changed 
#  interactively.
      set ZOOM [ .viewer cget -zoom ]
      set MAXCANV [ .viewer maxcanvas ]
      regexp {^([0-9]+)x([0-9]+)} [ winfo geometry .viewer ] dummy WINX WINY
      set PERCLO [ lindex [ .viewer cget -percentiles ] 0 ]
      set PERCHI [ lindex [ .viewer cget -percentiles ] 1 ]
      set MARKSTYLE [ .viewer cget -markstyle ]

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
      
#  Free resources.
      destroy .viewer
      $ndfset destroy

# $Id$
