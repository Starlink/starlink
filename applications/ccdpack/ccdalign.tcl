#+
#  Name:
#     ccdalign.tcl
#
#  Purpose:
#     Script to do the work for the CCDALIGN application.
#
#  Language:
#     Tcl.
#
#  Description:
#     This script uses [incr Tcl] classes to present a GUI to the user
#     which allows selection of a matching set of points on each one.
#
#  External Variables:
#     MAXCANV = integer (Given and Returned)
#        The maximum X or Y dimension of the canvas in which the initial
#        NDF is to be displayed.  If zero, there is no limit.
#     MAXPOS = integer (Given)
#        The maximum number of points which may be selected on any of
#        the NDFs.
#     NDFNAMES = list of strings (Given)
#        A list of strings giving the name of each of the NDFs which is
#        to be presented to the user for point selection.
#     NPOINT = list of integers (Returned)
#        The number of points for each image.  The Nth entry in this
#        list contains the number of points selected for the Nth NDF 
#        in the NDFNAMES variable.
#     PERCHI = real (Given and Returned)
#        The percentile of the data above which all values should be
#        plotted as the same colour.  Must be between 0 and 100.
#     PERCLO = real (Given and Returned)
#        The percentile of the data below which all values should be 
#        plotted as the same colour.  Must be between 0 and 100.
#     POINTS = list of lists of lists (Returned)
#        The points selected on the images.  The Nth entry in this list
#        contains data for the Nth NDF in the NDFNAMES variable.
#        The entry for each NDF is a list containing one entry for each 
#        selected point.  The entry for each point is a three-element
#        list containing the point index number, X coordinate and 
#        Y coordinate respectively.
#     REFPOS = integer (Given)
#        The position in the list NDFNAMES of the NDF to be used as the
#        reference image (the first image in the list has index 0).
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
#     24-JUL-2000 (MBT):
#        Original version.
#-

#  Sort out arguments.
      set title CCDALIGN
      set refndfname [ lindex $NDFNAMES $REFPOS ]
      set ref $REFPOS
      set ndfnames $NDFNAMES
      set nndf [ llength $ndfnames ]
      set nother [ expr $nndf - 1 ]

#  Set defaults for some arguments.
      foreach pair { { ZOOM 1 } { WINX 300 } { WINY 300 } \
                     { PERCLO 5 } { PERCHI 95 } { MAXCANV 0 } } {
         if { ! [ info exists [ lindex $pair 0 ] ] } { 
            eval set $pair
         }
      }

#  Initialise screen.
      wm withdraw .

#  Issue instructions about interacting with the GUI.
      ccdputs " "
      ccdputs "   For each image that is displayed, mark centroidable" \
              " features on it."
      ccdputs "   Add a point by clicking on the image with mouse button 1" \
              " (left),"
      ccdputs "   remove a point by clicking on it with mouse button 3 (right)."
      ccdputs "   Corresponding features should be labelled by the same" \
              " number on all images;"
      ccdputs "   the number of the next point to be added can be changed" \
              " in the toolbar."
      ccdputs "   Click the 'Done' button when you have added all the points" \
              " to an image."
      ccdputs " "
      
#  Allow selection of points for the reference NDF.
      set refndf [ ndf $refndfname ]
      Ndfview .vref \
                  -title "$title: %n (reference)" \
                  -watchstate state$ref \
                  -percentiles [ list $PERCLO $PERCHI ] \
                  -zoom $ZOOM \
                  -maxpoints $MAXPOS \
                  -geometry ${WINX}x${WINY}
      .vref loadndf $refndf $MAXCANV
      ccdputs -log "   Mark points on the reference image, [ $refndf name ]:"
      .vref activate
      tkwait variable state$ref
      ccdputs -log "   [ llength [ .vref points ] ] points initially marked."
      ccdputs -log " "

#  Get display preferences so that they can be propagated to subsequent
#  windows.
      set ZOOM [ .vref cget -zoom ]
      set MAXCANV [ .vref maxcanvas ]
      regexp {^([0-9]+)x([0-9]+)} [ .vref cget -geometry ] dummy WINX WINY 
      set PERCLO [ lindex [ .vref cget -percentiles ] 0 ]
      set PERCHI [ lindex [ .vref cget -percentiles ] 1 ]

#  Allow the user to muck about with the list for the reference NDF 
#  while the later ones are being modified.
      .vref configure -state active

#  Allow selection of points for the other NDFs.
      ndfview .v \
                 -watchstate state \
                 -percentiles [ list $PERCLO $PERCHI ] \
                 -zoom $ZOOM \
                 -maxpoints $MAXPOS \
                 -geometry ${WINX}x${WINY}

      set i 0
      set done 0
      foreach ndfname $ndfnames {
         if { $i != $ref } {
            incr done
            set ndf [ ndf [ lindex $ndfnames $i ] ]
            .v configure -title "$title: %n (#$done/$nother)"
            .v clearpoints
            .v loadndf $ndf $MAXCANV
            ccdputs -log \
            "   Mark points on image #$done/$nother, [ $ndf name ]:"
            .v activate
            tkwait variable state
            ccdputs -log "   [ llength [ .v points ] ] points marked."
            ccdputs -log " "
            set pts($i) [ .v points ]
            $ndf destroy
         }
         incr i
      }
      destroy .v

#  Get list of points for the reference NDF.  We do this last since it may
#  have been modified while the other NDFs are being marked.
      set pts($ref) [ .vref points ]

#  Construct the list of all points for return to calling program.
      for { set i 0 } { $i < $nndf } { incr i } {
         lappend POINTS $pts($i)
         lappend NPOINT [ llength $pts($i) ]
      }

#  Destroy remaining windows.
      .vref configure -state done
      destroy .vref
    
# $Id$
