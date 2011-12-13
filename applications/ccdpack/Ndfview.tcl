   itcl::class Ndfview {
#+
#  Name:
#     Ndfview

#  Purpose:
#     Provide a window which can display an ndf or ndfset object.
#
#     Description.
#     This class provides a mega-widget which is capable of displaying
#     an image, zooming it, changing the colour maps etc. and also
#     allowing the user to indicate a list of points which can be
#     marked and retrieved by the caller.

#  Language:
#     TCL

#  Type of Module:
#     [incr Tk] Mega-Widget

#  Public Methods:
#     activate
#        Following a call to this method, the object gets put in the
#        "active" status, and it will allow markers to be put on the
#        image, update the display with respect to user actions, etc.
#        Prior to this call, the visual state of the object will
#        probably be inconsistent with the configuration options which
#        have been passed to it.  This method should normally be called
#        only after an NDF has been loaded in using loadndf.
#
#     addpoint vx vy ?ipoint?
#        Adds a point at the given view coordinates to the list of
#        marked points.  If the ipoint parameter is not given then
#        the index of the new point will be selected in a sensible,
#        perhaps user-selectable, fashion from unused values.
#        The return value of this method is the index of the point plotted.
#           - vx       -- X coordinate of new point in view coordinates
#           - vy       -- Y coordinate of new point in view coordinates
#           - ?ipoint? -- Index of new point
#
#     clearpoints
#        Clears the contents of the marked points list, and erases all
#        the corresponding marked points from the display.
#
#     deactivate
#        After a call to this method, the user may no longer interact
#        with the widget to add new points.
#
#     loadndf ndf ?maxcanv?
#        This method loads an NDF into the viewer.
#           - ndf      -- A Tcl ndf or ndfset object.
#           - maxcanv  -- If given and non-zero, the GWM widget will not be
#                         more than maxcanv screen pixels in either direction.
#
#     points
#        Returns a list of currently marked points on the image.
#        The returned list has one element for each marked point, each
#        of these elements is of the form {index xpos ypos}.  Xpos and
#        ypos are in view coordinates.
#
#     Ndfview also inherits all the public methods of Gwmview.

#  Public Variables (Configuration Options):
#     centroiding = boolean
#        If true, then when the user clicks on the image to add a new
#        point, a point will be added at the nearest centroided
#        position (in the event of no centroid being found, a beep
#        will be beeped and no point will be added).  If false, the
#        coordinates indicated by the pointer will be used raw.
#
#     info = string
#        Gives a string which will be displayed somewhere in the window
#        near the displayed NDF.  Substitutions will be made as in the
#        title variable.
#
#     percentiles = list
#        A list containing two numbers between 0 and 100.  The user is
#        able to modify these somehow in the GUI.
#
#     title = string
#        This gives a title to show in the title bar which the window
#        manager displays.  The following substitutions will be made
#        before the title is displayed:
#           - %N  -- Full name
#           - %n  -- Shortened name (e.g. without full path)
#           - %p  -- Number of pixels ('X x Y' (single) or 'N pixels' (Set))
#           - %b  -- Pixel bounds ([X1:X2,Y1:Y2] (single) or '' (Set))
#
#     trackposition = string
#        This gives a string which can display the current position of
#        the cursor over an NDF image.  Before the text is displayed,
#        the following substitutions will be made:
#           - %x  -- X coordinate of the cursor
#           - %y  -- Y coordinate of the cursor
#
#        and the resulting string will be evaluated as a Tcl expression
#        before being printed on the widget.
#
#     Ndfview also inherits all the public variables of Gwmview.

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
#     10-NOV-2000 (MBT):
#        Original version.
#     5-APR-2001 (MBT):
#        Upgraded for use with Sets.
#     19-JUL-2001 (MBT):
#        Added centroiding, moved position marking methods into this
#        widget from Gwmview (this widget gets a Markercontrol to do
#        the work).  Also fixed a troublesome half-pixel alignment bug.
#     {enter_further_changes_here}

#  Bugs:
#     {note_any_bugs_here}

#-

#  Inheritance.
      inherit Gwmview


########################################################################
#  Constructor.
########################################################################
   constructor { args } {
      set canvas [ canvas ]
      itk_component add tracker {
         label [ childsite ].tracker -relief groove
      } {
         rename -font -trackfont trackFont Font
      }

#  Add more control groups.
      addgroup markers Markers
      addgroup style WCS
      addgroup cutoff Cutoff

#  Construct additional control widgets.
      set panel [ panel ]
      itk_component add dstyle {
         stylecontrol $panel.dstyle \
            -value "drawaxes=0,grid=0,numlab=0" \
            -valuevar displaystyle
      }
      itk_component add wcsframe {
         wcsframecontrol $panel.wcsframe \
            -value CURRENT \
            -valuevar wcsframe
      }
      itk_component add percut {
         percentilecontrol $panel.percut \
            -choices { { 20 80 } { 10 90 } { 5 95 } \
                       { 2 98 } { 1 99 } { 0.5 99.5 } } \
            -allowcustom 1 \
            -value { 6 94 } \
            -valuevar percentiles
      }
      itk_component add centroid {
         checkcontrol $panel.centroid \
            -label "Centroid" \
            -value 0 \
            -valuevar centroiding \
            -balloonstr "Points will be auto-centroided"
      }
      itk_component add marklist {
         markercontrol $panel.marklist \
            -canvas $canvas \
            -view2canvcmd [ code $this view2canv ] \
            -value "" \
            -valuevar markstyle
      }
      itk_component add abort {
         buttoncontrol $panel.abort \
            -text "Abort" \
            -cmd [ code $this deactivate ] \
            -balloonstr "Abort the application"
      }
      itk_component add abortdialog {
         iwidgets::messagedialog $itk_component(abort).confirm \
            -bitmap questhead \
            -modality application \
            -master $this \
            -title "Confirm Abort" \
            -text "Do you wish to abort the application?"
      } {
         ignore -modality
      }
      $itk_component(abortdialog) buttonconfigure Cancel \
         -text "Continue application"
      $itk_component(abortdialog) buttonconfigure OK \
         -text "Abort application"
      $itk_component(abortdialog) hide Help
      $itk_component(abort) configure -confirmcmd \
         "\[ $itk_component(abortdialog) center $itk_component(hull); \
             $itk_component(abortdialog) activate \]"

#  Add new control widgets to the panel.
      addcontrol $itk_component(wcsframe) style
      addcontrol $itk_component(dstyle) style
      addcontrol $itk_component(percut) cutoff
      addcontrol $itk_component(centroid) markers
      addcontrol $itk_component(marklist) markers
      addcontrol $itk_component(abort) action

#  Switch the positions of the Abort and Exit buttons.
      pack $itk_component(exit) -after $itk_component(abort)

#  Rearrange the control widget groups into two rows.
      itk_component add row1 { frame [ panel ].row1 }
      itk_component add row2 { frame [ panel ].row2 }
      set row1 $itk_component(row1)
      set row2 $itk_component(row2)
      lower $row1
      lower $row2
      pack $row1 $row2 -side top -expand 1 -fill both
      foreach grp { zoom cutoff markers } {
         set win [ groupwin $grp ]
         pack $win -in $row1 -side left -fill both -expand 1
      }
      foreach grp { style action } {
         set win [ groupwin $grp ]
         pack $win -in $row2 -side left -fill both -expand 1
      }

#  Set component aliases.
      set marklist $itk_component(marklist)

#  Do requested configuration.
      eval itk_initialize $args
   }


########################################################################
#  Public methods.
########################################################################

#-----------------------------------------------------------------------
      public method addpoint { vx vy { ipoint "" } } {
#-----------------------------------------------------------------------

#  Invoke addpoint method of the marker control widget to add the point
#  to the list and draw it.
         set ipoint [ $marklist addpoint $vx $vy $ipoint ]

#  Add a binding to remove the point from the canvas.
         set tag [ $marklist gettag $ipoint ]
         $canvas bind $tag <Button-3> [ code $marklist removepoint $ipoint ]

#  Return index of added point.
         return $ipoint
      }

#-----------------------------------------------------------------------
      public method clearpoints {} {
#-----------------------------------------------------------------------
         $marklist clearpoints
      }

#-----------------------------------------------------------------------
      public method points {} {
#-----------------------------------------------------------------------
         return [ $marklist points ]
      }



#-----------------------------------------------------------------------
      public method loadndf { ndfob { maxcanv 0 } } {
#-----------------------------------------------------------------------

#  Check that we are being asked to load a valid NDF Set object.
         if { ! [ catch { $ndfob validndfset } valid ] } {
            if { $valid } {
               set ndfsetob $ndfob
            } else {
               error "ndfview loadndf: \"$ndfob\" is an invalid ndf."
            }
         } elseif { ! [ catch { $ndfob validndf } valid ] } {
            if { $valid } {
               set ndfsetob [ ndfset "" [ $ndfob name ] ]
            } else {
               error "ndfview loadndf: \"$ndfob\" is an invalid ndfset."
            }
         } else {
            error "ndfview loadndf: \"$ndfob\" is neither an nfd nor a ndfset."
         }

#  Ensure that the GWM item is not currently occupied.
         removegwm

#  Store the ndf object.
         set ndfset $ndfsetob
         set nndf [ $ndfsetob nndf ]

#  Get the NDF name and make a shortened version.
         set fullname [ $ndfset name ]
         regsub {.*[./]} $fullname {} shortname

#  Write info substitution variables and image bounds.  We take the
#  opportunity here to verify that all NDFs are two-dimensional.
         set infodata(N) $fullname
         set infodata(n) $shortname
         if { $nndf == 1 } {
            set viewframe PIXEL
            set bounds [ $ndfsetob ndfdo 0 bounds ]
            set ndim [ llength $bounds ]
            if { $ndim != 2 } {
               set name [ $ndfsetob ndfdo 0 name ]
               error "NDF $name has $ndim dimensions instead of 2."
            }
            set x1 [ lindex [ lindex $bounds 0 ] 0 ]
            set x2 [ lindex [ lindex $bounds 0 ] 1 ]
            set y1 [ lindex [ lindex $bounds 1 ] 0 ]
            set y2 [ lindex [ lindex $bounds 1 ] 1 ]
            set infodata(b) "\[$x1:$x2,$y1:$y2\]"
            set infodata(p) "[ expr $x2 - $x1 + 1 ] x [ expr $y2 - $y1 + 1 ]"
            set xlo [ expr $x1 - 1 ]
            set xhi [ expr $x2 ]
            set ylo [ expr $y1 - 1 ]
            set yhi [ expr $y2 ]
         } else {
            set viewframe CCD_SET
            set infodata(b) ""
            set npix 0
            for { set i 0 } { $i < $nndf } { incr i } {
               set bounds [ $ndfsetob ndfdo $i bounds ]
               set ndim [ llength $bounds ]
               if { $ndim != 2 } {
                  set name [ $ndfsetob ndfdo $i name ]
                  error "NDF $name has $ndim dimensions instead of 2."
               }
               set x1 [ lindex [ lindex $bounds 0 ] 0 ]
               set x2 [ lindex [ lindex $bounds 0 ] 1 ]
               set y1 [ lindex [ lindex $bounds 1 ] 0 ]
               set y2 [ lindex [ lindex $bounds 1 ] 1 ]
               incr npix [ expr ( $x2 - $x1 + 1 ) * ( $y2 - $y1 + 1 ) ]
            }
            set infodata(p) "$npix pixels"
            set bbox [ $ndfsetob bbox $viewframe ]
            set xlo [ lindex [ lindex $bbox 0 ] 0 ]
            set xhi [ lindex [ lindex $bbox 0 ] 1 ]
            set ylo [ lindex [ lindex $bbox 1 ] 0 ]
            set yhi [ lindex [ lindex $bbox 1 ] 1 ]
         }
         set xdim [ expr $xhi - $xlo ]
         set ydim [ expr $yhi - $ylo ]

#  Configure the WCS frame widget.
         $itk_component(wcsframe) configure -ndf $ndfset

#  Get the size of an NDF pixel.
         configure -pixelsize [ $ndfset pixelsize $viewframe ]

#  Work out the size of the scrolledcanvas widget.
         wm deiconify $itk_interior
         update idletasks
         set canvwin [ $canvas component canvas ]
         set xcanv [ winfo width $canvwin ]
         set ycanv [ winfo height $canvwin ]

#  If the displayed size is smaller than the size of the scrolledcanvas
#  widget (but not necessarily than the canvas itself), enlarge it
#  until it takes up the available space.
         set enlarge 0
         while { 1 } {
            set z [ zoominc $zoom [ expr $enlarge + 1 ] ]
            set xsize [ expr $xdim * $z / $pixelsize ]
            set ysize [ expr $ydim * $z / $pixelsize ]
            if { $xsize <= $xcanv && $ysize <= $ycanv } {
               incr enlarge 1
               if { [ zoominc $zoom [ expr $enlarge + 1 ] ] <= $z } { break }
            } else {
               break
            }
         }
         set zoom [ zoominc $zoom $enlarge ]

#  If the displayed size is going to exceed the specified maximum canvas
#  size restriction, then iteratively shrink the zoomfactor until it fits.
         set shrink 0
         while { 1 && $enlarge <= 0 } {
            set z [ zoominc $zoom $shrink ]
            set xsize [ expr $xdim * $z / $pixelsize ]
            set ysize [ expr $ydim * $z / $pixelsize ]
            if { $maxcanv > 0 && [ max $xsize $ysize ] > $maxcanv } {
               incr shrink -1
               if { [ zoominc $zoom $shrink ] >= $z } { break }
            } else {
               break
            }
         }
         configure -zoom [ zoominc $zoom $shrink ]

#  Map the NDF.
         $ndfset mapped 1

#  Finally display the NDF.
         display
      }


#-----------------------------------------------------------------------
      public method activate {} {
#-----------------------------------------------------------------------

#  Call the inherited activate method.
         chain

#  Ensure that all aspects of the display are updated.
         display

#  Register the geometry.
         geomset

#  Set key bindings for marking objects.
         $canvas bind all <Button-1> [ code $this newpoint %x %y ]
      }


#-----------------------------------------------------------------------
      public method deactivate {} {
#-----------------------------------------------------------------------

#  Call the inherited deactivate method.
         chain

#  Unset all canvas bindings.
         unbindall
      }


########################################################################
#  Private methods.
########################################################################

#-----------------------------------------------------------------------
      private method display {} {
#-----------------------------------------------------------------------
#  This method ensures, if we are in the active status, that the display
#  is up to date with respect to the various configuration variables etc.

#  Only attempt any display if we are in the active status and have
#  something to display.
         if { $status != "active" || $fullname == "" } { return }

#  Only attempt display if characteristics have changed from the last
#  time they were displayed.  This is important for efficiency reasons
#  since the method may get called, possibly quite often, just to be
#  sure that the display is up to date.
         if { [ array exists displayed ] && \
              $displayed(fullname) == $fullname && \
              $displayed(percentiles) == $percentiles && \
              $displayed(zoomfactor) == $zoomfactor && \
              $displayed(displaystyle) == $displaystyle &&
              $displayed(wcsframe) == $wcsframe } {
            return
         }

#  Post a busy window.
         waitpush "Drawing image $fullname"

#  If the NDF has not been displayed before (but not if only its display
#  attributes have changed) then update the title bar and info panel.
         if { ! [ array exists displayed ] || \
              $displayed(fullname) != $fullname } {
            configure -title $title
            configure -info $info
         }

#  Keep a record of the attributes currently being displayed so if we
#  are called upon to do the same again we can skip it.
         set displayed(fullname) $fullname
         set displayed(percentiles) $percentiles
         set displayed(zoomfactor) $zoomfactor
         set displayed(displaystyle) $displaystyle
         set displayed(wcsframe) $wcsframe

#  Clear the canvas (it may contain markers as well as the GWM item).
         $canvas delete all

#  Create the GWM viewing item.
         makegwm $xlo $ylo $xdim $ydim

#  Display the NDF into the GWM item.
         set options {border=1 drawtitle=0 textlab=0 tickall=1 \
                      colour=3 colour(numlab)=5 colour(border)=4}
         lappend options $displaystyle
         $ndfset display "[ gwmname ]/GWM" \
                          [ lindex $percentiles 0 ] [ lindex $percentiles 1 ] \
                          $wcsframe [ join $options "," ]

#  Draw any points which have already been selected onto the new display.
         $marklist refreshpoints

#  Call any inherited 'display' method.
         chain

#  Return control to the user.
         waitpop
      }


#-----------------------------------------------------------------------
      private method newpoint { x y } {
#-----------------------------------------------------------------------
#  Add a point to the list of marked points, and draw it.

#  Get the coordinates of the point in view coordinates.
         set viewpos [ canv2view [ $canvas canvasx $x ] \
                                 [ $canvas canvasy $y ] ]
         set vx [ lindex $viewpos 0 ]
         set vy [ lindex $viewpos 1 ]

#  If required, try to centroid it.
         if { $centroiding } {
            if { [ catch { $ndfset centroid $vx $vy $viewframe $zoom } \
                         accpos ] } {

#  Centroiding failed - beep and take no further action.
               bell
            } else {

#  Centroiding succeeded - add the point.
               set avx [ lindex $accpos 0 ]
               set avy [ lindex $accpos 1 ]
               addpoint $avx $avy
            }

#  No centroiding required - add the point at the coordinates indicated.
         } else {
            addpoint $vx $vy
         }
      }


#-----------------------------------------------------------------------
      private method trackpos { x y } {
#-----------------------------------------------------------------------
         if { $status == "active" } {
            if { $x == "" || $y == "" } {
               $itk_component(tracker) configure -text ""
            } else {
               set viewpos [ canv2view [ $canvas canvasx $x ] \
                                       [ $canvas canvasy $y ] ]
               set wcspos [ lindex [ $ndfset wcstran -format \
                                     $viewframe $wcsframe $viewpos ] 0 ]
               set text $trackposition
               regsub -all %x $text [ lindex $viewpos 0 ] text
               regsub -all %y $text [ lindex $viewpos 1 ] text
               regsub -all %X $text [ lindex $wcspos 0 ] text
               regsub -all %Y $text [ lindex $wcspos 1 ] text
               $itk_component(tracker) configure -text [ expr \"$text\" ]
            }
         }
      }


#-----------------------------------------------------------------------
      private method infostring { string } {
#-----------------------------------------------------------------------
#  This method is used internally to substitute some information
#  which relates to the current state of the object into a string.
#  It is used to process, e.g., the title and info strings.
         foreach key [ array names infodata ] {
            regsub -all %$key $string $infodata($key) string
         }
         return $string
      }


########################################################################
#  Public variables.
########################################################################

#-----------------------------------------------------------------------
      public variable title "Ndfview" {
#-----------------------------------------------------------------------
         if { $status == "active" && $title != "" } {
            wm title $itk_interior [ infostring $title ]
         }
      }


#-----------------------------------------------------------------------
      public variable percentiles { 5 95 } {
#-----------------------------------------------------------------------
         display
      }


#-----------------------------------------------------------------------
      public variable displaystyle {} {
#-----------------------------------------------------------------------
         display
      }


#-----------------------------------------------------------------------
      public variable markstyle {} {
#-----------------------------------------------------------------------
      }


#-----------------------------------------------------------------------
      public variable centroiding { 0 } {
#-----------------------------------------------------------------------
      }


#-----------------------------------------------------------------------
      public variable info "%n (%p)" {
#-----------------------------------------------------------------------
         if { $status == "active" && $info != "" } {
            $itk_component(info) configure -text [ infostring $info ]
         }
      }


#-----------------------------------------------------------------------
      public variable trackposition { [ format "( %-10s, %-10s)" %X %Y ] } {
#-----------------------------------------------------------------------
         if { $trackposition == "" } {
            pack forget $itk_component(tracker)
            $canvas bind all <Motion> ""
            $canvas bind all <Leave> ""
         } else {
            $canvas bind all <Motion> [ code $this trackpos %x %y ]
            $canvas bind all <Leave> [ code $this trackpos "" "" ]
            pack $itk_component(tracker) -fill x -expand 0
         }
      }


#-----------------------------------------------------------------------
      public variable wcsframe "" {
#-----------------------------------------------------------------------
         display
      }



########################################################################
#  Private variables.
########################################################################

      private variable canvas ""       ;# Name of the canvas widget for display
      private variable displayed       ;# Array holding latest state of plot
      private variable fullname ""     ;# Full name of NDF or ndfset
      private variable infodata        ;# Array holding substitution string vals
      private variable marklist ""     ;# Marklist control widget
      private variable ndfset ""       ;# Tcl ndfset object
      private variable shortname ""    ;# Short name of NDF
      private variable viewframe PIXEL ;# View coordinate frame designator
      private variable xdim 0          ;# X size in view coordinate units
      private variable xlo 0           ;# Lower X bound in view coordinates
      private variable xhi 0           ;# Upper X bound in view coordinates
      private variable ydim 0          ;# Y size in view coordinate units
      private variable ylo 0           ;# Lower Y bound in view coordinates
      private variable yhi 0           ;# Upper Y bound in view coordinates
   }


########################################################################
#  Widget resource management
########################################################################

   itk::usual Ndfview {
      keep -background -cursor -foreground -geometry -canvasbackground
   }
   option add *Ndfview.trackFont fixed widgetDefault


########################################################################
#  Constructor alias
########################################################################

   proc ndfview { pathname args } {
      uplevel Ndfview $pathname $args
   }


# $Id$
