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
#     user which allows interactive placement of pairs of NDF Sets.
#
#  External Variables:
#     MARKSTYLE = string (Given and Returned)
#        A string, in the form of comma-separated att=value pairs,
#        indicating how markers should be plotted on the image.
#     MATPTS = list of lists of quads (Returned)
#        This gives a list of the centroided points for each of the 
#        pairs of NDF Sets which the user matched up.  It contains one 
#        element for each element of the PAIRS list.  Each element of
#        the list contains an entry for each of the points which waas
#        successfully centroided in both NDF Sets, and each of those
#        entries is a quad giving coordinates in each NDF Set, of the 
#        form {X1 Y1 X2 Y2}.
#     MAXCANV = integer (Given and Returned)
#        The maximum X or Y dimension of the canvas in which the initial
#        pair of NDF Sets is to be displayed.  If zero, there is no limit.
#     MAXPOS = integer (Given)
#        The maximum number of points which may be selected on the 
#        overlap region of any pair of NDF Sets.
#     NDFSETS = list of lists (Given)
#        Each element of this list represents a set of NDFs which is
#        to be presented to the user for aligning using this script.
#        The format of each element is {setname ndfname ?ndfname ...?}.
#        The setname is a text name of each set which is to be presented
#        to the user, and each ndfname is the name of one of the NDFs
#        which is to be a member of that set.  If only one ndfname
#        appears in each list, the program will effectively work 
#        on single NDFs.
#     PAIRS = list of lists (Returned)
#        The script returns a list with one element for each pair of NDF
#        Sets which the user managed to match.  Each element of the list
#        is of the form {I J NMAT XOFF YOFF}, where I and J are the 
#        indices in the list NDFSETS of the matched pair, NMAT is the
#        number of centroided points which were used to achieve the
#        exact offsets, and XOFF and YOFF are the position of the pixel
#        index origin of NDF Set J in pixel index coordinates of 
#        NDF Set I.
#     PERCHI = real (Given)
#        The percentile of the data above which all values should be 
#        plotted as the same colour.  Must be between 0 and 100.
#     PERCLO = real (Given)
#        The percentile of the data below which all values should be 
#        plotted as the same colour.  Must be between 0 and 100.
#     PREVX = integer (Given and Returned)
#        X dimension of the preview window for each NDF Set in the chooser 
#        widget.
#     PREVY = integer (Given and Returned)
#        Y dimension of the preview window for each NDF Set in the chooser
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
#     8-MAR-2001 (MBT):
#        Upgraded for use with Sets.
#-

#  Global variables.
      global Done

#  Set defaults for some arguments.
      foreach pair { { MARKSTYLE "" } { MAXCANV 0 } { MAXPOS 100 } \
                     { PERCLO 5 } { PERCHI 95 } { PREVX 300 } { PREVY 300 } \
                     { WINX 300 } { WINY 300 } { ZOOM 1 } } {
         if { ! [ info exists [ lindex $pair 0 ] ] } {
            eval set $pair
         }
      }

#  Initialise screen.
      wm withdraw .

#  Generate ndfset objects for all of the NDFs in the input list.  At the 
#  same time, check whether they all share the same Current WCS frame.
      set nndfset 0
      set ndfsetlist {}
      set diffcurrent 0
      set maxleng 0
      foreach ns $NDFSETS {
         incr nndfset
         if { [ llength $ns ] > 2 || [ lindex $ns 0 ] != [ lindex $ns 1 ] } {
            set ns [ lreplace $ns 0 0 "set:[ lindex $ns 0 ]" ]
         }
         set ndfset [ eval ndfset $ns ]
         set ndfsets($nndfset) $ndfset
         lappend ndfsetlist $ndfset
         set current [ $ndfset frameatt domain CURRENT ]
         set nameleng [ string length [ $ndfset name ] ]
         if { $nameleng > $maxleng } {
            set maxleng $nameleng
         }
         if { $nndfset > 1 && $current != $lastcurrent } {
            set diffcurrent 1
         }
         set lastcurrent [ $ndfset frameatt domain CURRENT ]
      }

#  If not all the NDF Sets have the same Current frame, check that the 
#  user is aware of the fact.
     if { $diffcurrent } {
        catch { unset warnlines }
        lappend warnlines \
           "The input images do not all have Current coordinate systems" \
           "with the same name; they are:" \
           ""
        foreach ndfset $ndfsetlist {
           lappend warnlines [ format "    %-${maxleng}s:  %s" \
                                      [ $ndfset name ] \
                                      [ $ndfset frameatt domain CURRENT ] ]
        }
        lappend warnlines \
           "" \
           "PAIRNDF will only work if the Current coordinate systems of all" \
           "images are related by a simple offset (translation) in X and Y." \
           "" \
           "Do you wish to continue?"
        set warnbox [ iwidgets::messagedialog .warn \
                         -modality application \
                         -bitmap questhead \
                         -title "PAIRNDF: Different WCS frames" \
                         -text [ join $warnlines "\n" ] \
                    ]
        $warnbox buttonconfigure Cancel -text "Abort PAIRNDF"
        $warnbox buttonconfigure OK -text "Continue"
        [ $warnbox component message ] configure -justify left
        $warnbox hide Help
        $warnbox center
        if { ! [ $warnbox activate ] } {
           error "User abort."
        }
     }

#  Create an NDF pair alignment widget.
      set aligner [ ndfalign .a ]
      wm withdraw $aligner
      $aligner configure \
                         -title "PAIRNDF: %An -- %Bn" \
                         -info "%n (frame %f)" \
                         -watchstatus alignstatus \
                         -zoom $ZOOM \
                         -maxpoints $MAXPOS \
                         -geometry ${WINX}x${WINY} \
                         -markstyle $MARKSTYLE

      [ $aligner component exit ] configure -balloonstr "Use this alignment"

#  Set help text for the aligner widget.
      catch { unset helplines }
      lappend helplines \
   "Use this widget to align two images by positioning one over the other." \
   "" \
   "The images displayed are the ones selected in the chooser widget," \
   "resampled into their Current coordinate frames.  If the images shown here" \
   "are not related by a simple offset (translation), you will have to" \
   "quit the application and set the Current coordinate system of all the" \
   "images so that they are related by translation." \
   "" \
   "To make the alignment, do the following:" \
   "" \
   "   Grab one of the images by holding down the mouse button on a" \
   "   recognisable feature, drag it to the same feature on the other image," \
   "   and release the mouse button.  The picture will then be redrawn with" \
   "   the overlap region averaged between the two." \
   "   It should be clear by looking whether the two are well aligned;" \
   "   if they are not, you can make a small adjustment, or drag them apart" \
   "   and try again.  To do this you will have to grab one by a part which" \
   "   is not overlapping the other." \
   "   Note that the alignment does not have to be perfect, since marked" \
   "   objects will be centroided to provide an accurate alignment during the" \
   "   next stage." \
   "" \
   "   If you are not sure the positioning is correct, a good tip is to move" \
   "   one image a few pixels away; if all the features look smeared out then" \
   "   you are close to a correctly aligned position." \
   "" \
   "   When you are happy with the alignment, click with the (left) mouse" \
   "   button on the overlapping region to mark centroidable features." \
   "   If you mark any in error, they can be removed by clicking on them" \
   "   with the right mouse button." \
   "" \
   "   The images cannot be moved when any features are marked on them."\
   "   If you have marked some features and wish to realign the images,"\
   "   remove the features using the right mouse button then drag again."\
   "" \
   "   When you are happy with the features that you have marked, click the" \
   "   `Done' button.  The marked features will then be centroided in both" \
   "   the images to provide an accurate alignment." \
   "   You will then be returned to the chooser widget to select another pair."
      $aligner configure -helptext [ join $helplines "\n" ]

#  Set the pair selection criterion.
      set choosestatus ""
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
      set chooser [ eval ndfchoose $base.c $ndfsetlist ]
      $chooser configure \
                         -title "PAIRNDF chooser" \
                         -watchstatus choosestatus \
                         -choosepercentiles 1 \
                         -choosewcsframe 0 \
                         -validpair [ code pairok %A %B ] \
                         -viewport [ list $PREVX $PREVY ] \
                         -percentiles [ list $PERCLO $PERCHI ] 

#  Create some dialog boxes which may or may not be used.  Since they
#  may be used more than once, it may be more efficient to construct
#  them here than at time of use.

#  Construct a dialog box for abnormal exit of the chooser.
      set confirmtext "Not all of the images have been paired;\n"
      append confirmtext "If you exit now only a subset will be registered.\n"
      append confirmtext "Do you really wish to exit?"
      set quitdialog [ iwidgets::messagedialog $chooser.qd \
                          -modality application \
                          -bitmap questhead \
                          -master $chooser \
                          -title "PAIRNDF: Confirm exit" \
                          -text $confirmtext ]
      $quitdialog buttonconfigure Cancel -text "Finish pairing"
      $quitdialog buttonconfigure OK -text "Continue"
      $quitdialog hide Help

#  Create a warning dialog for unsuccessful use of the aligner widget.
      global aligndialog
      global aligner
      set aligndialog [ iwidgets::messagedialog $aligner.qd \
                           -modality application \
                           -bitmap questhead \
                           -master $aligner \
                           -title "PAIRNDF: No pairs" ]
      $aligndialog buttonconfigure Cancel -text "Pick another pair"
      $aligndialog buttonconfigure OK -text "Complete this alignment"
      $aligndialog hide Help
      proc carryon { args } {
         global aligndialog
         global aligner
         lappend args "No pairing was made."
         $aligndialog configure -text [ join $args "\n" ]
         $aligndialog center $aligner
         return [ $aligndialog activate ]
      }

#  Set up help text for the chooser widget.
      catch { unset helplines }
      lappend helplines \
   "Use this window to select a pair of images with some area in common." \
   "" \
   "Select one image on each side by clicking on the tab with the right name." \
   "When this is done, the image will be displayed in the upper part of" \
   "the window on that side, and some information about it in the lower part." \
   "" \
   "Normally images are plotted when they are selected, which may be slow.",
   "Clicking the `Pre-plot' button causes all the images to be plotted" \
   "straight away, so subsequent selections are (almost) instantaneous." \
   "" \
   "The images are displayed resampled into their Current coordinate frame;" \
   "they must all be related by a simple offset (translation) for this" \
   "program to work." \
   "" \
   "You can change the style of image display using the `Grid' button, and " \
   "select FITS headers to be shown below using the `FITS' button." \
   "If you resize the window, the displayed images will grow or shrink to fit."\
   "" \
   "You can individually change the brightness of each displayed image by" \
   "using the `Display cutoff' control in its information panel." \
   "To make alignment easier, it is a good idea to adjust the images all to" \
   "have about the same brightness." \
   "" \
   "When you have selected a pair which overlap, click the `Use this pair'" \
   "button, and you will be asked to align the two images.  Apart from the" \
   "first time, you will only be allowed to select a pair for alignment if" \
   "at least one of them has already been aligned.  Images which have already" \
   "been aligned are marked with a `+' symbol on their selection tabs." \
   "" \
   "When all of the images have been aligned in pairs, the program will" \
   "automatically move to the next stage, and mutually register them all."
      $chooser configure -helptext [ join $helplines "\n" ]

#  Loop until all the NDF Sets have been paired.
      while { [ array size Done ] < $nndfset } {

#  Get the user to pick a pair of NDF Sets.
         wm deiconify $chooser
         raise $chooser
         set pair ""
         while { [ llength $pair ] != 2 } {
            $chooser configure -status active
            tkwait variable choosestatus
  
            set pair [ $chooser getpair ]
            set iA [ lindex $pair 0 ]
            set iB [ lindex $pair 1 ]
 
            if { [ $chooser cget -status ] == "done" } {
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
         if { $choosestatus == "done" } break

#  Log to the user.
         ccdputs -log "  "
         ccdputs -log "  Aligning images $iA and $iB"

#  Load the NDF Set pair into the aligner widget and wait for the user to 
#  select some positions in common.
         wm positionfrom $chooser user
         wm withdraw $chooser
         wm deiconify $aligner
         raise $aligner
         set percA [ $chooser percentiles $iA ]
         set percB [ $chooser percentiles $iB ]
         $aligner loadndf A $ndfsets($iA) CURRENT $percA $MAXCANV
         $aligner loadndf B $ndfsets($iB) CURRENT $percB $MAXCANV

#  Loop until the user is happy with the outcome.
         set tryagain 1
         while { $tryagain } {
            set tryagain 0
            $aligner activate
            tkwait variable alignstatus
            set MARKSTYLE [ $aligner cget -markstyle ]
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
               set offset [ ndfcentroffset $ndfsets($iA) CURRENT \
                                           $ndfsets($iB) CURRENT $pts $scale ]
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
                  if { $iA < $iB } {
                     set key "$iA,$iB"
                     set xo $xoff
                     set yo $yoff
                     set mp $matchpts
                  } else {
                     set key "$iB,$iA"
                     set xo [ expr 0 - $xoff ]
                     set yo [ expr 0 - $yoff ]
                     set mp {}
                     foreach pt $matchpts {
                        lappend mp [ list [ lindex $pt 2 ] [ lindex $pt 3 ] \
                                          [ lindex $pt 0 ] [ lindex $pt 1 ] ]
                     }
                  }
                  set pairs($key) [ list $nmatch $xo $yo $mp ]

#  Record the fact that these images have been connected.
                  set Done($iA) 1
                  set Done($iB) 1

#  Visually reflect the fact that they have been connected.
                  $chooser highlight $iA 1
                  $chooser highlight $iB 1

#  No objects were matched between frames.
               } else {
                  set tryagain [ carryon "Centroiding failed." ]
                  if { ! $tryagain } {
                     ccdputs -log \
                     "    Centroiding failed, no offset determined - ignored."
                  }
               }

#  There was an overlap, but the user failed to indicate any points to 
#  be centroided.
            } elseif { [ $aligner overlapping ] } {
               set text [ join {"No points were selected."
                                "You have to mark centroidable objects in the"
                                "overlapping region to use this alignment."} \
                          "\n" ]
               set tryagain [ carryon $text ]
               if { ! $tryagain } {
                  ccdputs -log "    No points selected in overlap - ignored."
               }

#  The user decided there was no overlap between the selected images.
            } else {
               set tryagain [ carryon "Images do not overlap." ]
               if { ! $tryagain } {
                  ccdputs -log "    Images do not overlap - ignored."
               }
            }
         }
         $aligner unbindall
         wm positionfrom $aligner user
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
