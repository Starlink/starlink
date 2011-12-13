   class Ndfalign {
#+
#  Name:
#     Ndfalign

#  Purpose:
#     Provide a window in which a user can align two images.
#
#     Description.
#     This class provides a mega-widget which will display two NDF
#     Sets and allow the user to overlay them interactively, thus
#     specifying an offset between the two.  The offset chosen by
#     the user can be retrieved by the caller.

#  Language:
#     TCL

#  Type of Module:
#     [incr Tk] Mega-Widget

#  Notes:
#     The display method contains elements of a workaround for a bug in
#     the PGPLOT driver for the GWM widget.  This is fixed in PGPLOT
#     5.2.0-6, but not in the Spring 2000 release.  When the fixed version
#     can be relied on to be there, this should be removed.

#  Public Methods:
#     activate
#        Following a call to this method, the object gets put in the
#        "active" status, and it will allow images to be aligned by the
#        user, update the display with respect to user actions, etc.
#        Prior to this call, the visual state of the object will
#        probably be inconsistent with the configuration options which
#        have been passed to it.  This method should normally be called
#        only after a pair of images has been loaded in using loadndf.
#
#     clearpoints
#        Removes all the points from the position lists and the canvas.
#
#     deactivate
#        After a call to this method, the user may no longer interact
#        with the widget to add points etc.
#
#     loadndf slot ndf ?frame? ?percs? ?maxcanv? ?fillwin?
#        This method loads an NDF Set into the viewer.
#           - slot     -- Either "A" or "B", to identify whether this is
#                         the A or B NDF.
#           - ndf      -- An ndf or ndfset object
#           - frame    -- The frame into which to resample the NDFs before
#                         plotting.  If absent, the current frame is used.
#           - percs    -- If present, this is a two-element list giving the
#                         percentile bounds between which the display of the
#                         NDF should be made.  If absent, a default is used.
#           - maxcanv  -- If given and non-zero, the GWM widget will not
#                         be more than maxcanv pixels in either direction.
#           - fillwin  -- If true, the zoom factor will be increased until
#                         the display is as big as it can be without
#                         exceeding the displayed size of the window.
#
#     maxcanvas
#        Returns a number a bit bigger than the larger of the width and
#        the height of a GWM item big enough to hold the two currently
#        loaded images just after the two loadndf calls at the current
#        zoom factor.  This may not be the same that given by the most
#        recently created GWM item, which is what we would get if we
#        just inherited the Gwmview maxcanvas method, since the two
#        images may overlap or be far from each other when this is called.
#
#     offset
#        Returns a two-element list giving the X and Y position of the
#        origin of the B image in CURRENT frame coordinates of the A image.
#
#     overlapping
#        Returns a boolean value indicating whether the two images
#        have a non-empty overlap as currently offset.
#
#     points slot frame
#        This method returns the list of selected points of the image
#        currently loaded into the indicated slot.  The points will be
#        given in coordinates of the indicated frame.  Each element
#        of the point list is of the form {index xpos ypos}.
#           - slot     -- Either "A" or "B" to identify the image.
#           - frame    -- Coordinate frame in which to return coords.
#                         Numeric index, domain name, CURRENT or BASE.
#
#     refreshpoints
#        Ensures that all the known points are drawn on the images.
#
#     removepoint ipoint
#        Removes the point with the given index from the canvas and the
#        list of known points.
#
#     Ndfalign also inherits all the public methods of Gwmview.

#  Public Variables (Configuration Options):
#     info = string
#        Gives a string which will be displayed somewhere in the window
#        near each displayed NDF Set.  The following substitutions will be
#        made before the string is displayed:
#           - %N  -- Full name
#           - %n  -- Shortened name (e.g. without full path)
#           - %f  -- Domain of WCS frame used
#           - %p  -- Number of pixels ('X x Y' (single) or 'N pixels' (Set))
#           - %b  -- Pixel bounds ('[X1:X2,Y1:Y2]' (single) or '' (Set))
#
#     markstyleA = string
#        The value which indicates the style of marker drawn corresponding
#        to features on the image in slot A.  See the Markstylecontrol
#        widget for syntax.
#
#     markstyleB = string
#        The value which indicates the style of marker drawn corresponding
#        to features on the image in slot B.  See the Markstylecontrol
#        widget for syntax.
#
#     title = string
#        This gives a title to show in the title bar which the window
#        manager displays.  The following substitutions will be made
#        before the title is displayed:
#           - %$N -- Full name
#           - %$n -- Shortened name (e.g. without full path)
#           - %$f -- Domain of WCS frame used
#           - %p  -- Number of pixels ('X x Y' (single) or 'N pixels' (Set))
#           - %b  -- Pixel bounds ('[X1:X2,Y1:Y2]' (single) or '' (Set))
#
#        In each of the above, the "$" should be replaced by "A" or "B"
#        to apply to the "A" or "B" image.
#
#     Ndfalign also inherits all the public variables of Gwmview.

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
#     8-MAR-2001 (MBT):
#        Upgraded for use with Sets.
#     19-JUL-2001 (MBT):
#        Added centroidig, and moved position marking methods into
#        this widget from Gwmview (this widget gets two Markercontrol
#        widgets to do the work).
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

#  Initialise some of the slot-indexed arrays, so we don't have to keep
#  testing for their non-existence before they have been set.
      foreach slot { A B } {
         set ndfset($slot) ""
         set wcsframe($slot) ""
         set fullname($slot) ""
         set shortname($slot) ""
         set present($slot) 0
         set nndf($slot) 0
      }

#  Set aliases for some widget components.
      set canvas [ canvas ]
      set panel [ panel ]

#  Add some groups to the control panel.
      addgroup markers Markers

#  Construct additional control widgets for marker selection.
      set side(A) "left"
      set side(B) "right"
      foreach slot { A B } {
         itk_component add markers$slot {
            markercontrol $panel.mark$slot \
               -canvas $canvas \
               -view2canvcmd [ code $this view2canv ] \
               -shownumcontrol 0 \
               -value "" \
               -valuevar markstyle$slot
         }
         set marklist($slot) $itk_component(markers$slot)
         [ $marklist($slot) component markstylecontrol ] configure \
            -balloonstr "Marker style for $side($slot) hand image"
         addcontrol $marklist($slot) markers
      }

#  Rearrange the control groups on the control panel in a different order.
      set grpwins {}
      foreach group { zoom markers action } {
         lappend grpwins [ groupwin $group ]
      }
      eval pack forget $grpwins
      eval pack $grpwins -side left -fill y

#  Split the info panel into two parts.
      foreach slot { A B } {
         itk_component add infofrm$slot {
            frame $itk_component(info).frm$slot
         }
         itk_component add info$slot {
            label $itk_component(infofrm$slot).info$slot
         }
      }
      pack $itk_component(infofrmA) $itk_component(infofrmB) \
           -side left -fill x -expand 1
      pack $itk_component(infoA)
      pack $itk_component(infoB)

#  Process initial configuration options.
      eval itk_initialize $args

#  Register initial size of window.
      geomset
   }


########################################################################
#  Public methods.
########################################################################

#-----------------------------------------------------------------------
      public method loadndf { slot ndfob { frame CURRENT } { percs { 0 100 } } \
                              { maxcanv 0 } { fillwin 1 } } {
#-----------------------------------------------------------------------

#  Validate the slot identifier.
         if { $slot != "A" && $slot != "B" } {
            error "Invalid NDF Set load in Ndfalign - programming error"
         }

#  Ensure that we have a valid NDF Set object to load.
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
            error "ndfview loadndf: \"$ndfob\" is neither an ndf nor a ndfset."
         }

#  Store the NDF Set object.
         set ndfset($slot) $ndfsetob
         set nndf($slot) [ $ndfsetob nndf ]
         set wcsframe($slot) $frame

#  Validate and store the display percentiles.
         if { [ llength $percs ] != 2 } {
            error "Percentiles \"$percs\" is not a two-element list"
         }
         set percentiles($slot) $percs

#  Get the NDF name and make a shortened version.
         set fullname($slot) [ $ndfset($slot) name ]
         regsub {.*/} $fullname($slot) {} shortname($slot)

#  Write info string substitution variables.  We take the opportunity
#  here to verify that all NDFs are two-dimensional.
         set infodata($slot,f) \
             [ $ndfset($slot) frameatt Domain $wcsframe($slot) ]
         set infodata($slot,N) $fullname($slot)
         set infodata($slot,n) $shortname($slot)
         if { $nndf($slot) == 1 } {
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
            set infodata($slot,b) "\[$x1:$x2,$y1:$y2\]"
            set infodata($slot,p) \
               "[ expr $x2 - $x1 + 1 ] x [ expr $y2 - $y1 + 1 ]"
        } else {
            set infodata($slot,b) ""
            set npix 0
            for { set i 0 } { $i < $nndf($slot) } { incr i } {
               set bounds [ $ndfsetob ndfdo $i bounds ]
               set ndim [ llength $bounds ]
               if { $ndim != 2 } {
                  set name [ $ndfsetob ndfdo $i name ]
                  error "NDF $name has $ndim dimensions instead of 2"
               }
               set x1 [ lindex [ lindex $bounds 0 ] 0 ]
               set x2 [ lindex [ lindex $bounds 0 ] 1 ]
               set y1 [ lindex [ lindex $bounds 1 ] 0 ]
               set y2 [ lindex [ lindex $bounds 1 ] 1 ]
               incr npix [ expr ( $x2 - $x1 + 1 ) * ( $y2 - $y1 + 1 ) ]
            }
            set infodata($slot,p) "$npix pixels"
        }

#  Store coordinates of the bounding box in the frame we are using.
         set bbox [ $ndfset($slot) bbox $wcsframe($slot) ]
         set xlo($slot) [ lindex [ lindex $bbox 0 ] 0 ]
         set xhi($slot) [ lindex [ lindex $bbox 0 ] 1 ]
         set ylo($slot) [ lindex [ lindex $bbox 1 ] 0 ]
         set yhi($slot) [ lindex [ lindex $bbox 1 ] 1 ]

#  Record that this slot is occupied by a valid image.
         set present($slot) 1

#  Initialise some variables to clean values.
         clearpoints
         set overlap 0

#  If we have two NDFs then it's time to worry about details of the display.
         if { $present(A) && $present(B) } {

#  Compare the pixel sizes of the two images in their selected frames.
            set pa [ $ndfset(A) pixelsize $wcsframe(A) ]
            set pb [ $ndfset(B) pixelsize $wcsframe(B) ]
            if { [ max [ expr $pa / $pb ] [ expr $pb / $pa ] ] > 20 } {
               ccdputs -log "  There is a gross discrepancy in pixel sizes; " \
                            "alignment will be difficult."
            }

#  Change the pixelsize variable if pixel sizes in the frames of the
#  currently loaded images are grossly different from the currently used
#  value.
            set psize [ max $pa $pb [ expr 1 / $pa ] [ expr 1 / $pb ] ]
            if { [ max [ expr $psize / $pixelsize ] \
                       [ expr $pixelsize / $psize ] ] > 1.5 } {
               configure -pixelsize [ max $pa $pb ]
            }
            configure -zoom $zoom

#  Work out the size of the scrolledcanvas widget.
            set canvwin [ $canvas component canvas ]
            update idletasks
            wm deiconify $itk_interior
            set xcanv [ winfo width $canvwin ]
            set ycanv [ winfo height $canvwin ]

#  If the displayed size is smaller than the size of the scrolledcanvas
#  widget (but not necessarily than the canvas itself), enlarge it
#  until it takes up the available space.
            set enlarge 0
            while { $fillwin } {
               set z [ zoominc $zoom [ expr $enlarge + 1 ] ]

#  Set the initial offset.  The images should be adjacent to each other
#  with a little gap in between.
               set xoff(A) [ expr 0 - $xlo(A) ]
               set yoff(A) [ expr 0 - $ylo(A) ]
               set xoff(B) [ expr $xhi(A) - $xlo(A) \
                                + $pixgap * $pixelsize / $z - $xlo(B) ]
               set yoff(B) [ expr 0 - $ylo(B) ]

#  Work out how big the GWM will need to be to display the pair of images.
               set xsize [ expr $z / $pixelsize * \
                                ( $xhi(B) + $xoff(B) - $xlo(A) - $xoff(A) ) ]
               set ysize [ expr $z / $pixelsize * \
                                [ max [ expr $yhi(A) - $ylo(A) ] \
                                      [ expr $yhi(B) - $yhi(B) ] ] ]

#  Increase the zoom level (if it will go any larger) and try again if
#  necessary.
               if { $xsize <= $xcanv && $ysize <= $ycanv } {
                  incr enlarge 1
                  if { [ zoominc $zoom [ expr $enlarge + 1 ] ] <= $z } { break }
               } else {
                  break
               }
            }
            set zoom [ zoominc $zoom $enlarge ]

#  We may have to iterate the following steps, shrinking the zoom each
#  time, if they result in requiring a GWM canvas which exceeds the
#  requested maximum dimensions.
            set shrink 0
            while { 1 && $enlarge <= 0 } {
               set z [ zoominc $zoom $shrink ]

#  Set the initial offset.  The images should be adjacent to each other
#  with a little gap in between.
               set xoff(A) [ expr 0 - $xlo(A) ]
               set yoff(A) [ expr 0 - $ylo(A) ]
               set xoff(B) [ expr $xhi(A) - $xlo(A) \
                                + $pixgap * $pixelsize / $z - $xlo(B) ]
               set yoff(B) [ expr 0 - $ylo(B) ]

#  Work out how big the GWM will need to be to display the pair of images.
               set xsize [ expr $z / $pixelsize * \
                                ( $xhi(B) + $xoff(B) - $xlo(A) - $xoff(A) ) ]
               set ysize [ expr $z / $pixelsize * \
                                [ max [ expr $yhi(A) - $ylo(A) ] \
                                      [ expr $yhi(B) - $yhi(B) ] ] ]

#  Reduce the zoom level (if it will go any smaller) and try again if
#  necessary.
               if { $maxcanv > 0 && [ max $xsize $ysize ] > $maxcanv } {
                  incr shrink -1
                  if { [ zoominc $zoom $shrink ] >= $z } { break }
               } else {
                  break
               }
            }
            configure -zoom [ zoominc $zoom $shrink ]

#  Finally update the display.
            display
         }
      }


#-----------------------------------------------------------------------
      public method maxcanvas {} {
#-----------------------------------------------------------------------
         foreach slot { A B } {
            set bb [ $ndfset($slot) bbox $wcsframe($slot) ]
            set x($slot) [ expr [ lindex [ lindex $bb 0 ] 1 ] - \
                                [ lindex [ lindex $bb 0 ] 0 ] ]
            set y($slot) [ expr [ lindex [ lindex $bb 1 ] 1 ] - \
                                [ lindex [ lindex $bb 1 ] 0 ] ]
         }
         set xsize [ expr ( $x(A) + $x(B) ) * $zoomfactor + $pixgap ]
         set ysize [ expr [ max $y(A) $y(B) ] * $zoomfactor ]
         return [ expr [ max $xsize $ysize ] * 1.4 ]
      }


#-----------------------------------------------------------------------
      public method activate {} {
#-----------------------------------------------------------------------

#  Call the inherited activate method.
         chain

#  Ensure that all aspects of the display are updated.
         display
      }


#-----------------------------------------------------------------------
      public method deactivate {} {
#-----------------------------------------------------------------------

#  Call the inherited deactivate method.
         chain
      }


#-----------------------------------------------------------------------
      public method points { slot frame } {
#-----------------------------------------------------------------------
         set rp {}
         foreach p [ $marklist($slot) points ] {
            set pfrm [ list [ expr [ lindex $p 1 ] - $xoff($slot) ] \
                            [ expr [ lindex $p 2 ] - $yoff($slot) ] ]
            set ppix [ lindex [ $ndfset($slot) wcstran $wcsframe($slot) \
                                $frame $pfrm ] 0 ]
            lappend rp [ list [ lindex $p 0 ] \
                              [ lindex $ppix 0 ] [ lindex $ppix 1 ] ]
         }
         return $rp
      }


#-----------------------------------------------------------------------
      public method clearpoints {} {
#-----------------------------------------------------------------------
         foreach slot { A B } {
            $marklist($slot) clearpoints
         }
      }


#-----------------------------------------------------------------------
      public method removepoint { ipoint } {
#-----------------------------------------------------------------------
         foreach slot { A B } {
            $marklist($slot) removepoint $ipoint
         }
      }


#-----------------------------------------------------------------------
      public method refreshpoints {} {
#-----------------------------------------------------------------------
         foreach slot { A B } {
            $marklist($slot) refreshpoints
         }
      }


#-----------------------------------------------------------------------
      public method offset {} {
#-----------------------------------------------------------------------
         return [ list [ expr $xoff(B) - $xoff(A) ] \
                       [ expr $yoff(B) - $yoff(A) ] ]
      }


#-----------------------------------------------------------------------
      public method overlapping {} {
#-----------------------------------------------------------------------
         if { $overlap > 0 } {
            return 1
         } else {
            return 0
         }
      }


########################################################################
#  Private methods.
########################################################################

#-----------------------------------------------------------------------
      private method display {} {
#-----------------------------------------------------------------------
#  This method ensures, if we are in the active status, that the display
#  is up to date with respect to the various configuration variables etc.

#  Only attempt to display if we are in the active status and have two
#  images to display.
         if { $status != "active" || ! $present(A) || ! $present(B) } {
            return
         }

#  Only attempt display if characteristics have changed from the last time
#  they were displayed.  This is important for efficiency reasons since
#  the method may get called, possibly quite often, just to be sure that
#  the display is up to date.
         if { ! [ array exists displayed ] ||
              $displayed(A,fullname) != $fullname(A) || \
              $displayed(B,fullname) != $fullname(B) || \
              $displayed(A,xoff) != $xoff(A) || \
              $displayed(B,xoff) != $xoff(B) || \
              $displayed(A,yoff) != $yoff(A) || \
              $displayed(B,yoff) != $yoff(B) || \
              $displayed(zoomfactor) != $zoomfactor } {

#  This may be time-consuming.  Post a waiting message.
            waitpush "Drawing images"

#  If this pair of images has not been displayed before (but not if only
#  the display characteristics have changed) then update the title bar.
            if { ! [ array exists displayed ] || \
                 $displayed(A,fullname) != $fullname(A) || \
                 $displayed(B,fullname) != $fullname(B) } {
               configure -title $title
               configure -info $info
            }

#  Keep a record of the attributes currently being displayed so if we
#  are called upon to do the same again we can skip it.
            set displayed(A,fullname) $fullname(A)
            set displayed(B,fullname) $fullname(B)
            set displayed(A,xoff) $xoff(A)
            set displayed(B,xoff) $xoff(B)
            set displayed(A,yoff) $yoff(A)
            set displayed(B,yoff) $yoff(B)
            set displayed(zoomfactor) $zoomfactor

#  Set the bounds of the GWM item to hold both images.
            set vxlo [ min [ expr $xoff(A) + $xlo(A) ] \
                           [ expr $xoff(B) + $xlo(B) ] ]
            set vxhi [ max [ expr $xoff(A) + $xhi(A) ] \
                           [ expr $xoff(B) + $xhi(B) ] ]
            set vylo [ min [ expr $yoff(A) + $ylo(A) ] \
                           [ expr $yoff(B) + $ylo(B) ] ]
            set vyhi [ max [ expr $yoff(A) + $yhi(A) ] \
                           [ expr $yoff(B) + $yhi(B) ] ]

#  Clear the canvas (it may contain items other than the GWM).
            $canvas delete all

#  Make the new GWM item.
            makegwm $vxlo $vylo [ expr $vxhi - $vxlo ] [ expr $vyhi - $vylo ]

#  Create polygons on the canvas for both images.  This both provides
#  visual feedback for the user and makes it easier to identify
#  positions on the canvas.
            foreach slot { A B } {
               for { set i 0 } { $i < $nndf($slot) } { incr i } {
                  set vertices ""
                  foreach pt [ $ndfset($slot) ndfdo $i polygon \
                                                       $wcsframe($slot) ] {
                     set cpt [ view2canv \
                                  [ expr $xoff($slot) + [ lindex $pt 0 ] ] \
                                  [ expr $yoff($slot) + [ lindex $pt 1 ] ] ]
                     lappend vertices [ lindex $cpt 0 ] [ lindex $cpt 1 ]
                  }
                  eval $canvas create polygon $vertices \
                          -fill \{\} -outline green -tags image$slot
               }
            }

#  Draw the images onto the GWM.
            set overlap [ \
               ndfdrawpair [ gwmname ]/GWM \
                  $vxlo $vylo [ expr $vxhi - $vxlo ] [ expr $vyhi - $vylo ] \
                  $zoomfactor \
                  $ndfset(A) $wcsframe(A) $xoff(A) $yoff(A) \
                  [ lindex $percentiles(A) 0 ] [ lindex $percentiles(A) 1 ] \
                  $ndfset(B) $wcsframe(B) $xoff(B) $yoff(B) \
                  [ lindex $percentiles(B) 0 ] [ lindex $percentiles(B) 1 ] \
            ]
            # $canvas create oval -5 -5 5 5 -fill yellow

#  Redraw any points in the points list.
            refreshpoints

#  Remove the waiting message.
            waitpop
         }

#  Set up key bindings for dragging, dropping and marking points.
         foreach slot { A B } {
            $canvas bind image$slot <ButtonPress-1> \
                                    [ code $this image_press $slot %x %y ]
            $canvas bind image$slot <Button1-Motion> \
                                    [ code $this image_motion $slot %x %y ]
            $canvas bind image$slot <ButtonRelease-1> \
                                    [ code $this image_release $slot %x %y ]
         }
      }


#-----------------------------------------------------------------------
      private method image_press { slot x y } {
#-----------------------------------------------------------------------
         set cx [ $canvas canvasx $x ]
         set cy [ $canvas canvasy $y ]
         set inmotion ""

#  See if we are in the overlap region.
         if { $slot == "A" } { set otherslot B } else { set otherslot A }
         set olap 0
         foreach item [ $canvas find overlapping $cx $cy $cx $cy ] {
            if { [ lsearch -exact [ $canvas gettags $item ] \
                                  image$otherslot ] > -1 } {
               set olap 1
               break
            }
         }

#  If we are in the overlap region, try to add a point.
         if { $olap } {
            set viewpos [ canv2view $cx $cy ]
            set vx [ lindex $viewpos 0 ]
            set vy [ lindex $viewpos 1 ]

#  Try to centroid it on both images.
            set fail 0
            foreach slot { A B } {
               if { ! $fail } {
                  set svx [ expr $vx - $xoff($slot) ]
                  set svy [ expr $vy - $yoff($slot) ]
                  set fail [ catch { $ndfset($slot) centroid $svx $svy \
                                     $wcsframe($slot) $zoom } accpos($slot) ]
               }
            }

#  If centroiding failed, just beep and forget it.
            if { $fail } {
               bell

#  If centroiding was successful in both, add the points and bind actions.
            } else {
               foreach slot { A B } {
                  set avx [ expr [ lindex $accpos($slot) 0 ] + $xoff($slot) ]
                  set avy [ expr [ lindex $accpos($slot) 1 ] + $yoff($slot) ]
                  set ipoint [ $marklist($slot) addpoint $avx $avy ]
                  set tag [ $marklist($slot) gettag $ipoint ]
                  $canvas bind $tag <Button-3> \
                     [ code $this removepoint $ipoint ]
                  $canvas bind $tag <Button-1> \
                     [ $canvas bind gwmitem <Button-1> ]
               }
            }

#  If we are not in an overlap and no points exist, start a drag sequence.
         } elseif { [ llength [ $marklist(A) points ] ] == 0 } {
            set inmotion $slot
            set xlast [ set xfirst $cx ]
            set ylast [ set yfirst $cy ]

#  If we are not in an overlap and points exist then do not permit dragging.
#  Just hoot.
         } else {
            bell
         }
      }


#-----------------------------------------------------------------------
      private method image_motion { slot x y } {
#-----------------------------------------------------------------------
         if { $inmotion != "" } {
            set cx [ $canvas canvasx $x ]
            set cy [ $canvas canvasy $y ]
            clearpoints
            $canvas move image$inmotion [ expr $cx - $xlast ] \
                                        [ expr $cy - $ylast ]
            set xlast $cx
            set ylast $cy
         }
      }


#-----------------------------------------------------------------------
      private method image_release { slot x y } {
#-----------------------------------------------------------------------
         if { $inmotion != "" } {
            set cdispx [ expr [ $canvas canvasx $x ] - $xfirst ]
            set cdispy [ expr [ $canvas canvasy $y ] - $yfirst ]
            set cstart [ view2canv $xoff($inmotion) $yoff($inmotion) ]
            set vnow [ canv2view [ expr [ lindex $cstart 0 ] + $cdispx ] \
                                 [ expr [ lindex $cstart 1 ] + $cdispy ] ]
            set xoff($inmotion) [ lindex $vnow 0 ]
            set yoff($inmotion) [ lindex $vnow 1 ]
            clearpoints
            display
         }
         set inmotion ""
      }


#-----------------------------------------------------------------------
      private method infostring { string } {
#-----------------------------------------------------------------------
#  This method is used internally to substitute some information
#  which relates to the current state of the object into a string.
#  It is used to process, e.g., the title and info strings.

         foreach key [ array names infodata ?,? ] {
            regexp {(.),(.)} $key dummy slot letter
            regsub -all %${slot}${letter} $string \
                   $infodata($slot,$letter) string
         }
         return $string
      }



########################################################################
#  Public variables.
########################################################################

#-----------------------------------------------------------------------
      public variable title "Ndfalign" {
#-----------------------------------------------------------------------
         if { $status == "active" && $title != "" } {
            wm title $itk_interior [ infostring $title ]
         }
      }


#-----------------------------------------------------------------------
      public variable info "%n" {
#-----------------------------------------------------------------------
         if { $status == "active" && $info != "" } {
            foreach slot { A B } {
               regsub -all % $info %$slot slotinfo
               if { $present($slot) } {
                  $itk_component(info$slot) configure \
                     -text [ infostring "$slotinfo" ]
               }
            }
         }
      }


#-----------------------------------------------------------------------
      public variable markstyleA {} {
#-----------------------------------------------------------------------
      }


#-----------------------------------------------------------------------
      public variable markstyleB {} {
#-----------------------------------------------------------------------
      }



########################################################################
#  Private variables.
########################################################################

#  Instance Variables.

#  The following are arrays indexed by slot, i.e. "A" or "B", according
#  to which NDF they refer to.
      private variable infodata        ;# Array holding substitution strings
      private variable fullname        ;# Full name of NDF Set
      private variable marklist        ;# Marker control widget
      private variable ndfset          ;# Tcl ndf object
      private variable nndf            ;# Number of member NDFs
      private variable percentiles     ;# Display percentiles for NDF
      private variable present         ;# Is slot occupied by a valid NDF?
      private variable shortname       ;# Short name of NDF Set
      private variable wcsframe        ;# Frame into which NDF is resampled
      private variable xlo             ;# Lower X bounding box frame coordinate
      private variable xhi             ;# Upper X bounding box frame coordinate
      private variable xoff            ;# X origin frame coordinate offset
      private variable ylo             ;# Lower Y bounding box frame coordinate
      private variable yhi             ;# Upper Y bounding box frame coordinate
      private variable yoff            ;# Y origin frame coordinate offset

#  The following are not indexed by slot.
      private variable canvas          ;# Name of the canvas widget for display
      private variable displayed       ;# Array holding latest state of plot
      private variable inmotion        ;# Image currently being dragged
      private variable overlap         ;# Number of pixels good in both images
      private variable xfirst          ;# Starting X pos of image being dragged
      private variable xlast           ;# Last X pos of image being dragged
      private variable yfirst          ;# Starting Y pos of image being dragged
      private variable ylast           ;# Last Y pos of image being dragged

#  Class Variables.
      private common pixgap 16         ;# Initial gap between adjacent NDFs

   }


########################################################################
#  Widget resource management
########################################################################

   itk::usual Ndfalign {
      keep -background -cursor -foreground -geometry -canvasbackground
   }
   option add *Ndfalign.canvasBackground black widgetDefault


########################################################################
#  Constructor alias
########################################################################

   proc ndfalign { pathname args } {
      uplevel Ndfalign $pathname $args
   }


# $Id$
