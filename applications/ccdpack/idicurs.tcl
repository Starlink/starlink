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
#     POINTS = list of pairs (Returned)
#        A list of the pixel coordinates indicated by the user on the NDF.
#        Each element of the list is of the form {X Y}.
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

#  Perform miscellaneous initialisation.
      tasksetup

#  Initialise screen.
      wm withdraw .

#  Get the requested NDF object.
      set ndf [ ndf $NDFNAME ]

#  Create an NDF viewer widget.
      ndfview .viewer \
                      -title "IDICURS: %n" \
                      -percentiles [ list $PERCLO $PERCHI ] \
                      -watchstate viewstate \
                      -zoom $ZOOM \
                      -uselabels 0 \
                      -maxpoints $MAXPOS
      .viewer configure -geometry ${WINX}x${WINY}

#  Load the NDF into the widget.
      .viewer loadndf $ndf $MAXCANV

#  Set the widget to respond to user actions.
      .viewer activate

#  Wait for the user to indicate that he has finished.
      tkwait variable viewstate

#  Retrieve configuration variables which the user may have changed 
#  interactively.
      set ZOOM [ .viewer cget -zoom ]
      set MAXCANV [ .viewer maxcanvas ]
      regexp {^([0-9]+)x([0-9]+)} [ winfo geometry .viewer ] dummy WINX WINY
      set PERCLO [ lindex [ .viewer cget -percentiles ] 0 ]
      set PERCHI [ lindex [ .viewer cget -percentiles ] 1 ]

#  Set the return variable to contain the list of points selected.
      set POINTS {} 
      foreach point [ .viewer points ] {
         lappend POINTS [ lrange $point 1 2 ]
      }

#  Free resources.
      destroy .viewer
      $ndf destroy

# $Id$
