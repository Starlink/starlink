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

#  Perform miscellaneous initialisation.
      tasksetup

#  Initialise screen.
      wm withdraw .

#  Allow selection of points for the reference NDF.
      set refndf [ ndf $refndfname ]
      Ndfview .v$ref \
                  -title "$title: %n (reference)" \
                  -watchstate state$ref \
                  -percentiles [ list $PERCLO $PERCHI ] \
                  -zoom $ZOOM \
                  -maxpoints $MAXPOS
      .v$ref loadndf $refndf $MAXCANV
      .v$ref configure -geometry ${WINX}x${WINY}
      .v$ref activate
      tkwait variable state$ref

#  Get display preferences so that they can be propagated to subsequent
#  windows.
      set ZOOM [ .v$ref cget -zoom ]
      set MAXCANV [ .v$ref maxcanvas ]
      regexp {^([0-9]+)x([0-9]+)} [ .v$ref cget -geometry ] dummy WINX WINY 
      set PERCLO [ lindex [ .v$ref cget -percentiles ] 0 ]
      set PERCHI [ lindex [ .v$ref cget -percentiles ] 1 ]

#  Allow the user to muck about with the list for the reference NDF 
#  while the later ones are being modified.
      .v$ref configure -state active

#  Allow selection of points for the other NDFs.
      set i 0
      set done 0
      foreach ndfname $ndfnames {
         if { $i != $ref } {
            incr done
            set ndf [ ndf [ lindex $ndfnames $i ] ]
            Ndfview .v$i \
                         -title "$title: %n (#$done/$nother)" \
                         -watchstate state$i \
                         -percentiles [ list $PERCLO $PERCHI ] \
                         -zoom $ZOOM \
                         -geometry ${WINX}x${WINY} \
                         -maxpoints $MAXPOS
            .v$i configure -geometry ${WINX}x${WINY}
            .v$i loadndf $ndf $MAXCANV
            .v$i activate
            tkwait variable state$i
            set ZOOM [ .v$i cget -zoom ]
            set MAXCANV [ .v$i maxcanvas ]
            regexp {^([0-9]+)x([0-9]+)} [ .v$i cget -geometry ] dummy WINX WINY 
            set PERCLO [ lindex [ .v$ref cget -percentiles ] 0 ]
            set PERCHI [ lindex [ .v$ref cget -percentiles ] 1 ]
            set pts($i) [ .v$i points ]
            destroy .v$i
            $ndf destroy
         }
         incr i
      }

#  Get list of points for the reference NDF.  We do this last since it may
#  have been modified while the other NDFs are being marked.
      set pts($ref) [ .v$ref points ]

#  Construct the list of all points for return to calling program.
      for { set i 0 } { $i < $nndf } { incr i } {
         lappend POINTS $pts($i)
         lappend NPOINT [ llength $pts($i) ]
      }

#  Destroy remaining windows.
      .v$ref configure -state done
      destroy .v$ref
    
# $Id$
