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
#     MAXCANV = integer (Given and Returned)
#        The maximum X or Y dimension of the canvas in which the initial
#        NDF is to be displayed.  If zero, there is no limit.
#     MAXPOS = integer (Given)
#        The maximum number of points which the user may indicate.
#     NDFNAME = string (Given)
#        The name of the NDF to be examined.
#     PERCHI = real (Given and Returned)
#        The percentile of the data above which all values should be 
#        plotted as the same colour.  Must be between 0 and 100.
#     PERCLO = real (Given and Returned)
#        The percentile of the data below which all values should be 
#        plotted as the same colour.  Must be between 0 and 100.
#     POINTS = list of pairs (Given and Returned)
#        A list of the pixel coordinates indicated by the user on the NDF.
#        Each element of the list is of the form {I X Y}, where I is a
#        unique integer index and X and Y are the coordinates.
#     SHOWIND = integer (Given)
#        If true (non-zero), then the index numbers of each point will be
#        displayed.  Otherwise only markers representing the points 
#        themselves will be plotted.
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
#-

#  Initialise screen.
      wm withdraw .

#  Get the requested NDF object.
      set ndf [ ndf $NDFNAME ]

#  Create an NDF viewer widget.
      ndfview .viewer \
                      -title "IDICURS: %n" \
                      -percentiles [ list $PERCLO $PERCHI ] \
                      -watchstatus viewstatus \
                      -zoom $ZOOM \
                      -uselabels $SHOWIND \
                      -maxpoints $MAXPOS \
                      -geometry ${WINX}x${WINY}

#  Tinker with cosmetic aspects of the viewer.
      [ .viewer component exit ] configure -balloonstr "Finish adding points"

#  Add a help control.
      catch { unset helplines }
      lappend helplines \
         "Click with mouse button 1 (left) on the image to add a point" \
         "Click with mouse button 3 (right) on the image to remove a point"
      if { $SHOWIND } {
         lappend helplines \
         "" \
         "Use the `Index' control to change the number of the next point marked"
      }
      lappend helplines \
         "" \
         "Click the `Done' button to finish marking points and end the program"
      .viewer configure -helptext [ join $helplines "\n" ]

#  Load the NDF into the widget.
      .viewer loadndf $ndf $MAXCANV

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

#  Set the return variable to contain the list of points selected.  If 
#  If we were displaying the indices then return the list with the indices
#  as plotted, otherwise return the list with indices starting from unity
#  and increasing to the number of points plotted.
      if { $SHOWIND } {
         set POINTS [ .viewer points ]
      } else {
         set POINTS {}
         set i 0
         foreach point [ .viewer points ] {
            lappend POINTS [ list [ incr i ] [ lindex $point 1 ] \
                                             [ lindex $point 2 ] ]
         }
      }

#  Free resources.
      destroy .viewer
      $ndf destroy

# $Id$
