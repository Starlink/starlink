#+
#  Name:
#     pairndf.tcl
#
#  Purpose:
#     Script to do the work for the PAIRNDF application.
#
#  Language:
#     Tcl.
#
#  Description:
#     This script uses custom [incr Tcl] classes to present a GUI to the
#     user which allows interactive placement of pairs of NDFs.
#
#  External Variables:
#     MATPTS = list of lists of quads (Returned)
#        This gives a list of the centroided points for each of the 
#        pairs of NDFs which the user matched up.  It contains one 
#        element for each element of the PAIRS list.  Each element of
#        the list contains an entry for each of the points which waas
#        successfully centroided in both NDFs, and each of those
#        entries is a quad giving coordinates in each NDF, of the 
#        form {X1 Y1 X2 Y2}.
#     MAXCANV = integer (Given and Returned)
#        The maximum X or Y dimension of the canvas in which the initial
#        pair of NDFs is to be displayed.  If zero, there is no limit.
#     MAXPOS = integer (Given)
#        The maximum number of points which may be selected on the 
#        overlap region of any pair of NDFs.
#     NDFNAMES = list of strings (Given)
#        A list of strings giving the name of each of the NDFs which is
#        to be presented to the user for aligning using this script.
#     PAIRS = list of lists (Returned)
#        The script returns a list with one element for each pair of NDFs
#        which the user managed to match.  Each element of the list is 
#        of the form {I J NMAT XOFF YOFF}, where I and J are the indices
#        in the list NDFNAMES of the matched pair, NMAT is the number of
#        centroided points which were used to achieve the exact offsets,
#        and XOFF and YOFF are the position of the pixel index origin of
#        NDF J in pixel index coordinates of NDF I.
#     PERCHI = real (Given)
#        The percentile of the data above which all values should be 
#        plotted as the same colour.  Must be between 0 and 100.
#     PERCLO = real (Given)
#        The percentile of the data below which all values should be 
#        plotted as the same colour.  Must be between 0 and 100.
#     PREVX = integer (Given and Returned)
#        X dimension of the preview window for each NDF in the chooser 
#        widget.
#     PREVY = integer (Given and Returned)
#        Y dimensino of the preview window for each NDF in the chooser
#        widget.
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
#     4-SEP-2000 (MBT):
#        Original version.
#-

#  Global variables.
      global Done

#  Set defaults for some arguments.
      foreach pair { { MAXCANV 0 } { MAXPOS 100 } { PERCLO 5 } { PERCHI 95 } \
                     { PREVX 300 } { PREVY 300 } { WINX 300 } { WINY 300 } \
                     { ZOOM 1 } } {
         if { ! [ info exists [ lindex $pair 0 ] ] } {
            eval set $pair
         }
      }

#  Initialise screen.
      wm withdraw .

#  Generate ndf objects for all of the NDFs in the input list.
      set nndf 0
      set ndflist {}
      foreach n $NDFNAMES {
         incr nndf
         set ndf [ ndf $n ]
         set ndfs($nndf) $ndf
         lappend ndflist $ndf
      }

#  Create an NDF pair alignment widget.
      set aligner [ ndfalign .a ]
      wm withdraw $aligner
      $aligner configure \
                         -title "PAIRNDF: %An -- %Bn" \
                         -info "%n" \
                         -watchstate alignstate \
                         -zoom $ZOOM \
                         -uselabels 0 \
                         -maxpoints $MAXPOS \
                         -geometry ${WINX}x${WINY} \

#  Set the pair selection criterion.
      set choosestate ""
      proc pairok { a b } {
         global Done
         if { ( $a != "" && $b != "" ) && ( [ array size Done ] == 0 || \
                                            [ array names Done $a ] != "" || \
                                            [ array names Done $b ] != "" ) } { 
            return 1 
         } else {
            return 0
         }
      }

#  Prepare for constructing the NDF chooser widget.  Since it may contain
#  multiple GWM widgets, on a pseudocolor display it will probably run out
#  of colormap entries.  The Ndfchoose widget itself attempts to deal
#  with this (via the Ccdtop container mechanism), but makes a somewhat
#  messy job of it.  To make things work more smoothly, we ensure here
#  that the Ndfchoose widget is parented by a window with a non-pseudocolor
#  colormap, if at all possible.
      if { [ winfo visual . ] == "pseudocolor" } {
         set visual [ Ccdtop::bestvisual . {truecolor 12} {staticgray 8} \
                                           {staticcolor 8} {pseudocolor 8} ]
         set base [ frame .f -visual $visual ]
      } else {
         set base [ frame .f ]
      }

#  Create an NDF chooser widget.
      set chooser [ eval ndfchoose $base.c $ndflist ]
      $chooser configure \
                         -title "PAIRNDF chooser" \
                         -watchstate choosestate \
                         -choosepercentiles 1 \
                         -choosewcsframe 0 \
                         -validpair [ code pairok %A %B ] \
                         -viewport [ list $PREVX $PREVY ] \
                         -percentiles [ list $PERCLO $PERCHI ] 

#  Create some dialog boxes which may or may not be used.  Since they
#  may be used more than once, it may be more efficient to construct
#  them here than at time of use.

#  Construct a dialog box for abnormal exit of the chooser.
      set confirmtext "Not enough images have been paired\n"
      append confirmtext "to register them all.\n"
      append confirmtext "Do you really wish to exit?"
      set quitdialog [ iwidgets::messagedialog $chooser.qd \
                          -modality application \
                          -bitmap questhead \
                          -title "PAIRNDF: Confirm exit" \
                          -text $confirmtext ]
      $quitdialog buttonconfigure Cancel -text "Abort PAIRNDF"
      $quitdialog buttonconfigure OK -text "Continue processing"
      $quitdialog hide Help

#  Create a warning dialog for unsuccessful use of the aligner widget.
      global aligndialog
      global aligner
      set aligndialog [ iwidgets::messagedialog $aligner.qd \
                           -modality application \
                           -bitmap questhead \
                           -title "PAIRNDF: No pairs" ]
      $aligndialog buttonconfigure Cancel -text "Pick another pair"
      $aligndialog buttonconfigure OK -text "Try again"
      $aligndialog hide Help
      proc carryon { args } {
         global aligndialog
         global aligner
         lappend args "No pairing was made."
         $aligndialog configure -text [ join $args "\n" ]
         $aligndialog center $aligner
         return [ $aligndialog activate ]
      }

#  Loop until all the NDFs have been paired.
      while { [ array size Done ] < $nndf } {

#  Get the user to pick a pair of NDFs.
         wm deiconify $chooser
         raise $chooser
         set pair ""
         while { [ llength $pair ] != 2 } {
            $chooser configure -state active
            tkwait variable choosestate
  
            set pair [ $chooser getpair ]
            set iA [ lindex $pair 0 ]
            set iB [ lindex $pair 1 ]
 
            if { [ $chooser cget -state ] == "done" } {
               $quitdialog center $chooser
               set response [ $quitdialog activate ]
               if { $response } {
                  set pair ""
               } else {
                  break
               }
            }
         }

#  Exit the loop if the user has indicated that the session is at an end.
         if { $choosestate == "done" } break

#  Log to the user.
         ccdputs -log "  "
         ccdputs -log "  Aligning NDFS $iA and $iB"

#  Load the NDF pair into the aligner widget and wait for the user to 
#  select some positions in common.
         wm deiconify $aligner
         raise $aligner $chooser
         wm withdraw $chooser
         set percA [ $chooser percentiles $iA ]
         set percB [ $chooser percentiles $iB ]
         $aligner loadndf A $ndfs($iA) CURRENT $percA $MAXCANV
         $aligner loadndf B $ndfs($iB) CURRENT $percB $MAXCANV

#  Loop until the user is happy with the outcome.
         set tryagain 1
         while { $tryagain } {
            set tryagain 0
            $aligner activate
            tkwait variable alignstate
            set MAXCANV [ $aligner maxcanvas ]
            set ZOOM [ $aligner cget -zoom ]

#  Get the resulting lists of points.
            set pA [ $aligner pointsndf A CURRENT ]
            set pB [ $aligner pointsndf B CURRENT ]
            set npoints [ llength $pA ]

#  We have a usable list of objects.  Try to centroid them.
            if { $npoints > 0 } {
               set pts {}
               for { set i 0 } { $i < $npoints } { incr i } {
                  lappend pts [ list [ lindex [ lindex $pA $i ] 1 ] \
                                     [ lindex [ lindex $pA $i ] 2 ] \
                                     [ lindex [ lindex $pB $i ] 1 ] \
                                     [ lindex [ lindex $pB $i ] 2 ] ]
               }
	       set scale [ $aligner cget -zoom ]
               set offset [ ndfcentroffset $ndfs($iA) CURRENT $ndfs($iB) \
                            CURRENT $pts $scale ]
               set nmatch [ lindex $offset 0 ]

#  We have a successful match.
               if { $nmatch > 0 } {
                  set xoff [ lindex $offset 1 ]
                  set yoff [ lindex $offset 2 ]
                  set matchpts [ lindex $offset 3 ]

#  Tell the user that the match succeeded.
                  ccdputs -log \
                "    Centroiding successful: $nmatch/$npoints objects matched."

#  Add this datum to the results list.  By storing these in a hash at
#  this stage, we ensure we don't pass back duplicate pairings to the
#  calling routine.
                  set pairs($iA,$iB) [ list $nmatch $xoff $yoff $matchpts ]

#  Record the fact that these images have been connected.
                  set Done($iA) 1
                  set Done($iB) 1

#  Visually reflect the fact that they have been connected.
                  $chooser highlight $iA 1
                  $chooser highlight $iB 1

#  No objects were matched between frames.
               } else {
                  set tryagain [ carryon "Centroiding failed." ]
                  ccdputs -log \
                  "    Centroiding failed, no offset determined - ignored."
               }

#  There was an overlap, but the user failed to indicate any points to 
#  be centroided.
            } elseif { [ $aligner overlapping ] } {
               set tryagain \
                   [ carryon "No points were selected in the overlap." ]
               ccdputs -log "    No points selected in overlap - ignored."

#  The user decided there was no overlap between the selected images.
            } else {
               set tryagain [ carryon "Images do not overlap." ]
               ccdputs -log "    Images do not overlap - ignored."
            }
         }
         $aligner unbindall
         wm withdraw $aligner
      }

#  Write the returned PAIRS list.
      set PAIRS {}
      foreach key [ array names pairs ] {
         regexp {^([0-9]+),([0-9]+)$} $key dummy iA iB
         set val $pairs($key)
         set nmatch [ lindex $val 0 ]
         set xoff [ lindex $val 1 ]
         set yoff [ lindex $val 2 ]
         set matpts [ lindex $val 3 ]
         lappend PAIRS [ list $iA $iB $nmatch $xoff $yoff ]
         lappend MATPTS $matpts
      }

#  Retrieve characteristics of the windows which may have been 
#  changed by the user.
      set vp [ $chooser cget -viewport ]
      if { [ llength $vp ] == 2 } {
         set PREVX [ lindex $vp 0 ]
         set PREVY [ lindex $vp 1 ]
      }
      regexp {^([0-9]+)x([0-9]+)} [ winfo geometry $aligner ] dummy WINX WINY

#  Destroy remaining windows.
      destroy $chooser
      destroy $aligner

# $Id$
