   itcl::class Ndfchoose {
#+
#  Name:
#     Ndfchoose

#  Purpose:
#     Provide a method of choosing images from a group.

#  Language:
#     TCL

#  Type of Module:
#     [incr Tk] Mega-Widget

#  Description:
#     This class provides a GUI which allows a user to choose pairs of
#     NDF Sets from a given group.  The user is given thumbnail images and
#     some textual information (like their names) and client widgets
#     can call methods in order to find out which images have been chosen.

#  Constructor:
#     ndfchoose pathname ndf ndf ?ndf ...? ?options?
#        An NDF chooser object is constructed in the normal way; apart
#        from any configuration options, all arguments are existing ndf
#        or ndfset objects representing images which are to form the
#        list amongst which the user can choose.  Images cannot
#        be added to this list after construction time.  The images
#        form an ordered list, and are subsequently referred to by
#        index; the index of the first is 1, of the second is 2, etc.

#  Public Methods:
#     getpair
#        Returns an ordered list of two image indices, indicating the pair
#        of images which is currently selected by the user.  An invalid
#        ndfset is indicated by an empty string.  If the -validate
#        configuration variable has been specified, and the pair does
#        not satisfy this criterion, then a single empty string will
#        be returned.
#
#     highlight index ?state?
#        This method controls highlighting of the image indicated.
#        With no optional state argument it returns a value giving
#        the current state of highlighting.  If the state argument
#        is supplied, it will set the highlighting state to the given
#        value.  There are currently two highlighting states available,
#        0 and 1.  0 means not highlighted and 1 means highlighted.
#        All images start in highlighting state 0.
#           - index     -- Index of the image whose state is to be set/queried
#           - ?state?   -- State into which image index should be put
#
#     percentiles index
#        This method returns a list giving the percentile cutoff
#        values which are used to display each image.  The list has two
#        elements {lo hi} where 0 <= lo <= hi <= 100.
#           - index     -- Index of the image whose percentiles are required
#
#     preplot
#        Prepares all the images so that subsequent selections will
#        be effective (almost) instantaneously rather than having to
#        display the NDFs on demand (until a subsequent viewport
#        reconfiguration - i.e. window resize - is done).
#
#     wcsframe index
#        This method returns the domain name of the WCS frame currently
#        selected for the given image
#           - index     -- Index of the image whose frame name is required

#  Public Variables (Configuration Options):
#     choosepercentiles = boolean
#        If true, the user will be able to manipulate the percentile
#        cutoff used for NDF display.
#
#     choosewcsframe = boolean
#        If true, the user will be able to select the WCS frame chosen
#        for the displayed NDFs.
#
#     percentiles = list
#        A list containing two numbers between 0 and 100.  This is used
#        as a default percentile cutoff limit for NDF display, but the
#        user can select different values for different images.
#
#     skip2 = boolean
#        If true, and the chooser is constructed with only two images
#        to choose from, then the chooser will automatically select
#        that pair with no user interaction at all.
#
#     status = string
#        A value which gives the status of the object.  It may have the
#        following values:
#           active:
#              The viewer will attempt to reflect changes in its
#              configuration on the display.
#           inactive:
#              The viewer may not attempt to reflect changes in its
#              configuration on the display.  The viewer will only enter
#              this from the active status if a subsequent call of the
#              getpair method will return a valid pair.
#           done:
#              The viewer's work is done (e.g. the exit button has
#              been pressed).
#
#        Only the values 'active' and 'inactive' may be written into this
#        variable from outside.  This variable may be tracked from
#        outside the class (for instance if a trace is to be run on it)
#        using the 'watchstatus' public variable.
#
#     validpair = string
#        This gives an expression which indicates whether a pair is
#        valid or not.  Before use any occurrences of the sequences "%A"
#        and "%B" are replaced by the indices of the first and second
#        members of the pair respectively.  The string is then evaluated
#        to give a boolean result determining whether the pair is valid
#        or not.  The status variable will only change from "active" to
#        "inactive" if the getpair method will return a pair which
#        satisfies this criterion.
#
#     viewport = list
#        The value of the viewport variable is a list of two integers,
#        being the width and height in pixels of the window in which
#        each previewed image is displayed.

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
#     4-APR-2001 (MBT):
#        Prevented resize attempts during window plotting.  For some
#        reason I don't understand, this can cause core dumps.
#        Also added the pre-plot button.
#     7-JUN-2001 (MBT):
#        Modified to avoid unnecessary re-creation of GWM window.  This
#        also leads to core dumps (GWM window lost by PGPLOT - I suspect,
#        but can't prove, a bug in GWM or PGPLOT).  This can still
#        happen however if you do a lot of chooser resizing.
#     9-JUL-2001 (MBT):
#        Added scrollbars to the tabsets, to accomodate many NDFs.
#     13-AUG-2001 (MBT):
#        Fixed size of info windows (i.e. prevented geometry propagation)
#        which prevents some resizing problems.  Also set a minimum
#        resizable size for the window.
#     {enter_further_changes_here}

#  Bugs:
#     The resizing of this widget is somewhat fragile; it will not cope
#     with a window resize (WINCH) request while it is already handling
#     one (attempting to do so results in a Tcl/Tk coredump for reasons
#     I don't understand).  The widget therefore informs the window
#     manager that the window should not be resized during the time
#     it is dealing with resize requests.  Unfortunately, a few WMs
#     (the only one I've seen is sawmill) do not respect this - sawmill
#     does resizing by passing a cascade of WINCH events one for each
#     point the mouse goes through during the user interactive window
#     resize.  This leads to a core dump.
#     {note_new_bugs_here}

#-

#  Inheritance.
      inherit Ccdtop


########################################################################
#  Constructor.
########################################################################
      constructor { args } {

#  Invoke the Ccdtop constructor with arguments indicating that the outer
#  frame must be a GWM-colormap-holding frame.
         Ccdtop::constructor -colormap
      } {

#  Parse the arguments: any arguments before the first one starting with
#  a minus sign are interpreted as ndfset objects, and any following are
#  configuration options.  We happen to know that ndfset objects never
#  start with a minus sign, so no ambiguities can arise.  Also validate
#  that the arguments are what they ought to be.
         set ndfsetlist "<blank>"
         set nndfset 0
         while { [ llength $args ] > 0 && \
                 [ string index [ lindex $args 0 ] 0 ] != "-" } {
            set ndfob [ lindex $args 0 ]
            if { ! [ catch { $ndfob validndfset } valid ] } {
               if { $valid } {
                  set ndfset $ndfob
               } else {
                  error "Argument \"$ndfob\" is an invalid ndf."
               }
            } elseif { ! [ catch { $ndfob validndf } valid ] } {
               if { $valid } {
                  set ndfset [ ndfset "" [ $ndfob name ] ]
               } else {
                  error "Argument \"$ndfob\" is an invalid ndfset."
               }
            } else {
               error "Argument \"$ndfob\" is not an ndf or an ndfset object."
            }
            set highlight([incr nndfset]) 0
            lappend ndfsetlist $ndfset
            set args [ lreplace $args 0 0 ]
         }
         if { $nndfset == 0 } {
            error "No ndfset objects supplied"
         }

#  Work out the maximum X and Y dimensions of any of the images.  These
#  are required for working out relative sizes of displayed images -
#  only required if the user is not permitted to change the selected
#  WCS frame.
         if { ! $choosewcsframe } {
            for { set i 1 } { $i <= $nndfset } { incr i } {
               set bbox [ [ lindex $ndfsetlist $i ] bbox [ wcsframe $i ] ]
               set xmax [ max $xmax \
                              [ expr [ lindex [ lindex $bbox 0 ] 1 ] - \
                                     [ lindex [ lindex $bbox 0 ] 0 ] ] ]
               set ymax [ max $ymax \
                              [ expr [ lindex [ lindex $bbox 1 ] 1 ] - \
                                     [ lindex [ lindex $bbox 1 ] 0 ] ] ]
            }
         }

#  Initialise values of selected images.
         set inview(A) ""
         set inview(B) ""

#  Set the list of all available FITS header lines.  Use those from the
#  first named NDF in the first NDF Set as an example.
         set allfits [ [ lindex $ndfsetlist 1 ] ndfdo 0 fitshead ]

#  Add control groups to the control panel.
         addgroup style Style
         addgroup action Control

#  Construct control widgets.
         set panel [ panel ]
         itk_component add dstyle {
            stylecontrol $panel.dstyle \
               -value "drawaxes=0,grid=0,numlab=0" \
               -valuevar displaystyle
         }
         itk_component add showfits {
            buttoncontrol $panel.showfits \
               -text "FITS" \
               -cmd [ code $this fitsselect ] \
               -balloonstr "FITS headers in panel"
         }
         itk_component add gotpair {
            buttoncontrol $panel.gotpair \
               -text "Use this pair" \
               -cmd [ code $this configure -status inactive ] \
               -state disabled \
               -balloonstr "Do alignment on this pair"
         }
         itk_component add preplot {
            buttoncontrol $panel.preplot \
               -text "Pre-plot" \
               -cmd [ code $this preplot ] \
               -balloonstr "Prepare all images for chooser display"
         }
         itk_component add help {
            helpcontrol $panel.help
         } {
            usual
            keep -helptext
         }
         itk_component add done {
            buttoncontrol $panel.done \
               -text "Exit" \
               -cmd [ code $this configure -status done ] \
               -balloonstr "Abort the application"
         }

#  Add control widgets to the control groups.
         addcontrol $itk_component(dstyle) style
         addcontrol $itk_component(showfits) style
         addcontrol $itk_component(preplot) action
         addcontrol $itk_component(gotpair) action
         addcontrol $itk_component(help) action
         addcontrol $itk_component(done) action

#  Construct the choosing area.  This is the business end of the widget
#  and consists of two tabsets and two frames in which images can be
#  displayed.  Each window is drawn once for each of the images under the
#  widget's control, and this window is mapped or unmapped in one of
#  the frames by a geometry manager according to what the user selects.
#  First set a container frame within which all this display will happen.
         set interior [ childsite ]
         itk_component add choosearea {
            frame $interior.choosearea
         }

#  Set the dimensions of the windows to hold images.
         set viewx [ lindex $viewport 0 ]
         set viewy [ lindex $viewport 1 ]

#  Most elements of the choosearea have an A side and a B side; construct
#  these in a loop.
         set tabpos(A) w
         set tabpos(B) e
         set side(A) left
         set side(B) right
         foreach slot { A B } {

#  Construct the tabsets, and controlling scrollbars.  The tabsets are
#  window items within a canvas, since a canvas is a scrollable widget
#  but a tabset is not.  An iwidgets::scrolledframe is unfortunately not
#  suitable here since it constrains the scrollbar to lie on the right.
            itk_component add tabframe$slot {
               frame $itk_component(choosearea).tframe$slot
            }
            itk_component add tabcanv$slot {
               canvas $itk_component(tabframe$slot).canv
            }
            itk_component add tabs$slot {
               iwidgets::tabset $itk_component(tabframe$slot).tabs \
                                -tabpos $tabpos($slot) \
                                -bevelamount 4 \
                                -tabborders no \
                                -command [ code $this ndfselect $slot ]
            }
            itk_component add tabscroll$slot {
               scrollbar $itk_component(tabframe$slot).sb \
                         -orient vertical \
                         -command "$itk_component(tabcanv$slot) yview"
            }
            $itk_component(tabcanv$slot) create window 0 0 \
                                         -window $itk_component(tabs$slot) \
                                         -anchor "n$tabpos($slot)"
            $itk_component(tabcanv$slot) configure \
               -yscrollcommand "$itk_component(tabscroll$slot) set" -width 1
            pack $itk_component(tabcanv$slot) -side $side($slot) -anchor n

#  Construct the NDF area frames.
            itk_component add ndf$slot {
               frame $itk_component(choosearea).ndf$slot
            }

#  Construct the viewing area frames.
            itk_component add view$slot {
               frame $itk_component(ndf$slot).view$slot \
                                   -width $viewx \
                                   -height $viewy \
                                   -relief groove -borderwidth 2
            }
            pack $itk_component(view$slot) -side top

#  Construct the NDF information panels.
            itk_component add describe$slot {
               frame $itk_component(ndf$slot).info$slot \
                                   -relief groove -borderwidth 2
            }
            pack $itk_component(describe$slot) -side top -anchor w -fill x

#  Add selection tabs to both tabsets for each of the NDFs.
            $itk_component(tabs$slot) add -label "<blank>" -state disabled
            for { set i 1 } { $i < [ llength $ndfsetlist ] } { incr i } {
               $itk_component(tabs$slot) add -label " "
            }

#  Pack the ndf frames themselves and the controlling tabsets into the
#  choosing area.
            pack $itk_component(tabframe$slot) -side $side($slot) \
                                               -fill y -anchor n
            pack $itk_component(ndf$slot) -side $side($slot)

#  Set the initially selected images to blank ones.
            ndfselect $slot 0
         }

#  Initialise the NDF tab labels to the unhighlighted state.
         setstate unhighlighted all

#  Pack the components into the hull.
         pack $itk_component(choosearea) -side top

#  Generate frames which will contain the windows to be displayed for each
#  of the images.
         for { set i 0 } { $i < $nndfset } { incr i } {
            itk_component add image$i {
               frame $itk_component(choosearea).image$i \
                         -width $viewx -height $viewy
            }
         }

#  Do requested configuration.
         eval itk_initialize $args

#  Set a binding to handle window resize events.
         bind $itk_interior <Configure> [ code $this winch %W ]
      }


########################################################################
#  Public methods.
########################################################################

#-----------------------------------------------------------------------
      public method getpair { } {
#-----------------------------------------------------------------------
         if { [ isvalid ] } {
            set ia ""
            set ib ""
            if { $inview(A) != "A" } { set ia $inview(A) }
            if { $inview(B) != "B" } { set ib $inview(B) }
            return [ list $ia $ib ]
         } else {
            return ""
         }
      }


#-----------------------------------------------------------------------
      public method highlight { index { state "" } } {
#-----------------------------------------------------------------------
         if { $index < 1 || $index > $nndfset } {
            error "Highlight index out of range"
         }
         if { $state ==  "" } {
            return $highlight($index)
         } else {
            set highlight($index) $state
            if { $state > 0 } {
               setstate highlighted $index
            } else {
               setstate unhighlighted $index
            }
         }
      }


#-----------------------------------------------------------------------
      public method percentiles { index } {
#-----------------------------------------------------------------------
         set retval ""
         if { [ catch { $percentilecontrol($index) cget -value } retval ] } {
            set retval ""
         }
         if { $retval == "" } {
            set retval $percentiles
         }
         return $retval
      }


#-----------------------------------------------------------------------
      public method preplot {} {
#-----------------------------------------------------------------------
         waitpush "Plotting all windows"
         for { set index 1 } { $index <= $nndfset } { incr index } {
            ndfplotwindow $index
         }
         waitpop
      }


#-----------------------------------------------------------------------
      public method wcsframe { index } {
#-----------------------------------------------------------------------
         set retval ""
         if { [ catch { $wcsframecontrol($index) cget -value } retval ] } {
            set retval ""
         }
         if { $retval == "" } {
            if { $index != "A" && $index != "B" } {
               set ndfset [ lindex $ndfsetlist $index ]
               set retval [ $ndfset frameatt Domain CURRENT ]
            }
         }
         return $retval
      }



########################################################################
#  Private methods.
########################################################################

#-----------------------------------------------------------------------
      private method setstate { mode args } {
#-----------------------------------------------------------------------
#  A mixed bag of functionality, grouped together for ease of argument
#  processing.

         if { [ lindex $args 0 ] == "all" } {
            set args {}
            for { set i 1 } { $i <= $nndfset } { incr i } {
               lappend args $i
            }
         }

         foreach i $args {
            switch $mode {
               ready {
                  $itk_component(tabsA) tabconfigure $i -state normal
                  $itk_component(tabsB) tabconfigure $i -state normal
               }
               unready {
                  $itk_component(tabsA) tabconfigure $i -state disabled
                  $itk_component(tabsB) tabconfigure $i -state disabled
               }
               highlighted {
                  set name [ [ lindex $ndfsetlist $i ] name ]
                  $itk_component(tabsA) tabconfigure $i \
                      -label "$name +" \
                      -font "helvetica -12 bold"
                  $itk_component(tabsB) tabconfigure $i \
                      -label "+ $name" \
                      -font "helvetica -12 bold"
               }
               unhighlighted {
                  set name [ [ lindex $ndfsetlist $i ] name ]
                  $itk_component(tabsA) tabconfigure $i \
                      -label "$name   " \
                      -font "helvetica -12 normal"
                  $itk_component(tabsB) tabconfigure $i \
                      -label "   $name" \
                      -font "helvetica -12 normal"
               }
               default {
                  error "$this setstate $mode: No such mode"
               }
            }
         }
      }


#-----------------------------------------------------------------------
      private method ndfplotwindow { index } {
#-----------------------------------------------------------------------
#  This method returns the name of the window which contains the drawn
#  NDF.  The argument may be either the numerical index of one of the
#  ndfs in the list, or a slot name (A or B).  In the latter case, a
#  suitable blank window will be returned.

#  Prevent resize attempts during the plotting.  For reasons I don't
#  understand, a user resize during plotting leads to a core dump.
         wm resizable $itk_interior 0 0

#  Validate the index argument.
         if { $index == "A" } {
            set isndfset 0
            set side left
            set textpos "-y 10 -relx 0 -x 10 -anchor nw"
         } elseif { $index == "B" } {
            set isndfset 0
            set side right
            set textpos "-y 10 -relx 1 -x -10 -anchor ne"
         } elseif { $index > 0 } {
            set isndfset 1
            set ndfset [ lindex $ndfsetlist $index ]
         } else {
            error "Invalid index argument \"$index\" to ndfplotwindow"
         }

#  Ensure that the corresponding info window exists (certain widgets
#  on this are interrogated to do the display).
         ndfinfowindow $index

#  Get characteristics of the plot.
         set width [ lindex $viewport 0 ]
         set height [ lindex $viewport 1 ]
         if { $isndfset } {
            set percs [ percentiles $index ]
            set wcsframe [ wcsframe $index ]
         } else {
            set percs 0
            set style 0
            set wcsframe 0
         }

#  Unless we establish otherwise, we will need to create a new window
#  and GWM widget, and draw the image onto it.
         set createwindow 1
         set drawimage 1

#  See if the window and GWM widget already exists.
         if { [ array names gwmwin $index ] != "" } {

#  See if it has the right shape; if so, we will not need to create a
#  new one.
            if { ! $isndfset || ( $plotted($index,width) == $width && \
                                  $plotted($index,height) == $height ) } {
               set createwindow 0

#  If it has the right shape, see if the image has been plotted with
#  all the right attributes; if so, we will not need to plot it again.
               if { $plotted($index,percs) == $percs && \
                    $plotted($index,wcsframe) == $wcsframe && \
                    $plotted($index,displaystyle) == $displaystyle } {
                  set drawimage 0
               }

#  The window has the wrong shape; here we should destroy the existing
#  one prior to creating a new one on which to replot the image.
#  However, a bug somewhere (I'm fairly sure it's in the GWM driver
#  of PGPLOT) can sometimes cause an error with the message
#
#      %PGPLOT, /gwm: lost PGPLOT window 1
#
#  followed by a core dump to occur when GWM windows are re-used.
#  This bug has been reported, but since it is hard to pin down in
#  a reproducible way a it does not seem likely to be fixed.
#  Never destroying the GWM windows seems to make it OK, though this
#  is not ideal since resources which are no longer used are not
#  being freed up.
            } else {
               place forget $itk_component($gwmwin($index))
               # destroy $itk_component($gwmwin($index))
            }
         }

#  Set the name of the GWM component.  If we are creating a new GWM
#  widget here, ensure that it is a window name which has not been
#  used before.
         if { $createwindow } {
            set gwmwin($index) "plot[ incr gseq ]"
         }

#  Check whether we need to do anything; we may only need to return
#  the window name.
         if { $drawimage } {

#  The window does not currently exist; we need to create and fill it.
#  Create the containing frame.
            if { $createwindow } {
               container $gwmwin($index) $itk_component(choosearea) \
                                         [ expr "{$index}" == "{1}" ]
            }

#  Only proceed if the widget is in the active state.
            if { $isndfset } {

#  Create the GWM widget.
               if { $status == "active" } {

#  Display might be time-consuming: post a busy window.
                  waitpush "Drawing image [ $ndfset name ]"

#  Work out the dimensions of the GWM widget.  Only attempt to adjust the
#  sizes for consistency if there is no possibility of the user changing
#  the WCS frame out from under us, since this would make it difficult
#  to keep the relative sizes in step.
                  if { $choosewcsframe } {
                     set gwidth $width
                     set gheight $height
                  } else {
                     set bbox [ $ndfset bbox $wcsframe ]
                     set xdim [ expr [ lindex [ lindex $bbox 0 ] 1 ] - \
                                     [ lindex [ lindex $bbox 0 ] 0 ] ]
                     set ydim [ expr [ lindex [ lindex $bbox 1 ] 1 ] - \
                                     [ lindex [ lindex $bbox 1 ] 0 ] ]
                     if { [ expr 1.0 * $xmax / $ymax ] > \
                          [ expr 1.0 * $width / $height ] } {
                        set fraction [ expr 1.0 * $xdim / $xmax ]
                        set gwidth [ expr $width * $fraction ]
                        set gheight [ expr $gwidth * 1.0 * $ydim / $xdim ]
                     } else {
                        set fraction [ expr 1.0 * $ydim / $ymax ]
                        set gheight [ expr $height * $fraction ]
                        set gwidth [ expr $gheight * 1.0 * $xdim / $ydim ]
                     }
                  }

#  Get the name that the GWM widget has/will have.
                  set gwmname \
                      [ winfo id $itk_component(choosearea) ]_$gwmwin($index)

#  Construct the GWM widget if necessary.
                  if { $createwindow } {
                     itk_component add $gwmwin($index):display {
                        gwm $itk_component($gwmwin($index)).gwm \
                            -width $gwidth -height $gheight -name $gwmname
                     }
                  }

#  Plot the NDF inside the GWM.
                  set options {border=0 drawtitle=0 textlab=0 tickall=0 \
                               colour=3 colour(numlab)=5 colour(border)=4 \
                               minticklen=0 majticklen=0}
                  lappend options $displaystyle
                  $ndfset display -resamp "$gwmname/GWM" \
                                  [ lindex $percs 0 ] [ lindex $percs 1 ] \
                                  $wcsframe [ join $options "," ]

                  pack $itk_component($gwmwin($index):display) \
                       -expand 1 -fill both

#  It may be a good idea to unmap the NDF here (although it may not).
                  $ndfset mapped 0

#  Remove the busy window.
                  waitpop
               }

#  This is a blank window; write instructions to the user.
            } else {
               itk_component add $gwmwin($index):display {
                  frame $itk_component($gwmwin($index)).blank \
                      -width $width \
                      -height $height
               }
               set msg \
                  "Use the tabs to\npick an image for\ndisplay on the $side"
               itk_component add $gwmwin($index):instructions {
                  label $itk_component($gwmwin($index):display).instruct$index \
                     -justify $side -text $msg
               }
               itk_component add $gwmwin($index):arrow {
                  label $itk_component($gwmwin($index):display).arrow$index \
                     -bitmap @$CCDdir/arrow_$side.xbm
               }
               pack $itk_component($gwmwin($index):arrow) \
                  -side $side -anchor n -expand 0
               pack $itk_component($gwmwin($index):instructions) \
                  -side $side -anchor n -expand 1 -fill both
               pack $itk_component($gwmwin($index):display) \
                  -expand 1 -fill both
            }

#  Store characteristics of this plot so that we know whether subsequent
#  plot requests are out of date.
            set plotted($index,width) $width
            set plotted($index,height) $height
            set plotted($index,percs) $percs
            set plotted($index,wcsframe) $wcsframe
            set plotted($index,displaystyle) $displaystyle
         }

#  Withdraw the ban on user window resizing.
         wm resizable $itk_interior 1 1

#  Return the name of the window.
         return $itk_component($gwmwin($index))
      }


#-----------------------------------------------------------------------
      private method ndfinfowindow { index } {
#-----------------------------------------------------------------------
#  This method returns the name of a window which contains textual
#  annotations for the drawn image, i.e. its name, applicable FITS headers,
#  etc.  The argument may be either the numerical index of one of the
#  images in the list, or a slot name (A or B).  In the latter case, a
#  suitable blank window will be returned.

#  Validate the index argument.
         if { $index == "A" || $index == "B" } {
            set isndfset 0
         } elseif { $index > 0 } {
            set isndfset 1
            set ndfset [ lindex $ndfsetlist $index ]
         } else {
            error "Invalid index argument \"$index\" to ndfinfowindow"
         }

#  Store current values of the controls in the info window.
         set percval [ percentiles $index ]
         set wcsval [ wcsframe $index ]

#  Check whether the window exists and is out of date.  If so, destroy
#  it preparatory to generating a new one.
         if { [ array names itk_component info$index ] != "" } {
            if { $noted($index,showfits) != $showfits } {
               destroy $itk_component(info$index)
            }
         }

#  Check whether the window already exists.  If it does not, all we need
#  to do is return the name.
         if { [ array names itk_component info$index ] == "" } {

#  The window does not currently exist; we need to create and fill it.
#  Create the containing frame.
            itk_component add info$index {
               frame $itk_component(choosearea).info$index
            }

#  Get dimensions of the NDF.
            set dims ""
            set name ""
            set val ""
            if { $isndfset } {
               set name [ $ndfset name ]
               if { [ $ndfset nndf ] == 1 } {
                  foreach dim [ $ndfset ndfdo 0 bounds ] {
                     append dims "[ lindex $dim 0 ]:[ lindex $dim 1 ] x "
                  }
                  regsub " x $" $dims "" dims
               } else {
                  set dims 0
                  for { set i 0 } { $i < [ $ndfset nndf ] } { incr i } {
                     set npix 1
                     foreach dim [ $ndfset ndfdo $i bounds ] {
                        set npix [ expr $npix * ( [ lindex $dim 1 ] - \
                                                  [ lindex $dim 0 ] + 1 ) ]
                     }
                     incr dims $npix
                  }
               }
            }

#  Construct a set of labelled widgets containing the information.
            set lws ""

#  Construct a list of key, value pairs to be displayed in the text region.
            set pairs {}
            lappend pairs [ list "Name" $name ]
            lappend pairs [ list "Pixels" $dims ]
            if { ! $choosewcsframe } {
               lappend pairs [ list "WCS frame" $wcsval ]
            }
            if { ! $choosepercentiles } {
               lappend pairs [ list "Display cutoff" \
                            "[ lindex $percval 0 ]% - [ lindex $percval 1 ]%" ]
            }
            foreach fh $showfits {
               set key [ lindex $fh 1 ]
               if { $isndfset } {
                  set val [ lindex [ $ndfset ndfdo 0 fitshead $key ] 0 ]
               }
               lappend pairs [ list "FITS $key" $val ]
            }

#  Write the list of pairs in the window.
            foreach pair $pairs {
               set keytext [ lindex $pair 0 ]
               set val [ lindex $pair 1 ]
               regsub -all { +} $keytext "_" key
               itk_component add info$index:key_$key {
                  iwidgets::labeledwidget \
                     $itk_component(info$index).w$key -labeltext "$keytext:"
               }
               if { $isndfset } {
                  itk_component add info$index:val_$key {
                     label [ $itk_component(info$index:key_$key) \
                             childsite ].val \
                        -text $val
                  }
                  pack $itk_component(info$index:val_$key)
               }
               lappend lws $itk_component(info$index:key_$key)
            }

#  Put a percentile selection control in the text region.
            if { $choosepercentiles } {
               itk_component add info$index:key_percentile {
                  iwidgets::labeledwidget \
                     $itk_component(info$index).wpercentile \
                     -labeltext "Display cutoff:"
               }
               if { $isndfset } {
                  itk_component add info$index:val_percentile {
                     percentilecontrol [ \
                        $itk_component(info$index:key_percentile) \
                        childsite ].val \
                        -allowcustom 1
                  }
                  set percentilecontrol($index) \
                     $itk_component(info$index:val_percentile)
                  $percentilecontrol($index) configure \
                     -value $percval \
                     -command [ code $this refresh $index ]
                  lappend controls $percentilecontrol($index)
                  pack $itk_component(info$index:val_percentile)
               }
               lappend lws $itk_component(info$index:key_percentile)
            }

#  Put a frame selection control in the text section.
            if { $choosewcsframe } {
               itk_component add info$index:key_wcsframe {
                  iwidgets::labeledwidget $itk_component(info$index).wwcsframe \
                     -labeltext "WCS frame:"
               }
               if { $isndfset } {
                  itk_component add info$index:val_wcsframe {
                     wcsframecontrol [ \
                        $itk_component(info$index:key_wcsframe) childsite ].val
                  }
                  set wcsframecontrol($index) \
                     $itk_component(info$index:val_wcsframe)
                  $wcsframecontrol($index) configure \
                     -ndf $ndfset \
                     -value $wcsval \
                     -command [ code $this refresh $index ]
                  lappend controls $wcsframecontrol($index)
                  pack $itk_component(info$index:val_wcsframe)
               }
               lappend lws $itk_component(info$index:key_wcsframe)
            }

#  Pack and align the labeled widgets.
            eval pack $lws -side top -anchor w
            eval iwidgets::Labeledwidget::alignlabels $lws

#  Store characteristics of this info window so that we know whether
#  subsequent info window requests are out of date.
            set noted($index,showfits) $showfits

#  Set the height of the window, and turn off geometry propagation so that
#  long labels cannot mess up the size of the parent window.
            set vy 0
            foreach lw $lws {
               incr vy [ winfo reqheight $lw ]
            }
            $itk_component(info$index) configure -height $vy
            pack propagate $itk_component(info$index) 0
         }

#  Set the width of the window to that of the width of the NDF viewport.
#  If these windows are not kept to the same width, a long label in
#  this window (the info window) can lead to problems when resizing.
         $itk_component(info$index) configure -width [ lindex $viewport 0 ]

#  Return the name of the window.
         return $itk_component(info$index)
      }


#-----------------------------------------------------------------------
      private method refresh { index } {
#-----------------------------------------------------------------------
#  Should be called if some of the displayed (selected) plot or info
#  windows may have become out of date.
         update idletasks
         foreach slot { A B } {
            if { $inview($slot) == $slot } {
               ndfselect $slot 0
            } else {
               ndfselect $slot $inview($slot)
            }
            update
         #  if { $inview($slot) == $index } {
         #     ndfselect $slot $index
         #  }
         }
         wm geometry $itk_interior ""
         update idletasks
      }


#-----------------------------------------------------------------------
      private method ndfselect { slot index } {
#-----------------------------------------------------------------------
#  If item is zero, this indicates a blank window.  Otherwise, it is the
#  window corresponding to the indicated image.
         $itk_component(gotpair) configure -state disabled
         if { $slot == "A" } { set otherslot B } else { set otherslot A }
         if { $inview($slot) != "" } {
            place forget [ ndfplotwindow $inview($slot) ]
            pack forget [ ndfinfowindow $inview($slot) ]
         }
         if { $index > 0 && $inview($otherslot) == $index } {
            place forget [ ndfplotwindow $inview($otherslot) ]
            pack forget [ ndfinfowindow $inview($otherslot) ]
            ndfselect $otherslot 0
         }
         $itk_component(tabs$slot) select $index
         if { $index == 0 } {
            set item $slot
            if { $slot == "A" } {
               set pos "-relx 0 -rely 0 -anchor nw"
            } elseif { $slot == "B" } {
               set pos "-relx 1 -rely 0 -anchor ne"
            }
            $itk_component(view$slot) configure -bg \
                 [ lindex [ $itk_component(view$slot) configure -bg ] 3 ]
         } else {
            set item $index
            set pos "-relx 0.5 -rely 0.5 -anchor center"
            $itk_component(view$slot) configure -bg black
         }
         set inview($slot) $item
         eval place [ ndfplotwindow $item ] \
            -in $itk_component(view$slot) $pos
         pack [ ndfinfowindow $item ] \
            -in $itk_component(describe$slot) -anchor w -fill x
         if { [ isvalid ] } {
            $itk_component(gotpair) configure -state normal
         }
      }


#-----------------------------------------------------------------------
      private method isvalid { } {
#-----------------------------------------------------------------------
         set ia "{}"
         set ib "{}"
         if { $inview(A) != "A" } { set ia $inview(A) }
         if { $inview(B) != "B" } { set ib $inview(B) }
         regsub %A $validpair $ia exp
         regsub %B $exp $ib exp
         return [ eval $exp ]
      }


#-----------------------------------------------------------------------
      private method winch { W } {
#-----------------------------------------------------------------------
#  This method is called if the toplevel window receives a configure
#  event, which will happen, for instance, if it is resized.
         if { $W != $itk_interior } {
            return
         }
         if { $resizer_ != {} } {
            after cancel $resizer_
         }
         set xinc [ expr ( [ winfo width $itk_interior ] - \
                              [ winfo reqwidth $itk_interior ] ) / 2 ]
         set yinc [ expr [ winfo height $itk_interior ] - \
                       [ winfo reqheight $itk_interior ] ]
         if { $xinc != 0 || $yinc != 0 } {
            set resizer_ [after 100 \
                             [code $this incrementviewport_ $xinc $yinc]]
         }
      }

      protected variable resizer_ {}
      private method incrementviewport_ {xinc yinc} {
         configure -viewport [ list [ expr [ lindex $viewport 0 ] + $xinc ] \
                                  [ expr [ lindex $viewport 1 ] + $yinc ] ]
         update
      }

#-----------------------------------------------------------------------
      private method fitsselect { } {
#-----------------------------------------------------------------------
#  This method interrogates the user to update the showfits private
#  variable.

#  If we haven't done this before, build the FITS selection window.
         if { [ array names itk_component fits_dialog ] == "" } {

#  Construct the dialog box.
            itk_component add fits_dialog {
               iwidgets::dialog $itk_component(showfits).d \
                  -modality application \
                  -title "Select FITS headers for display"
            }
            $itk_component(fits_dialog) buttonconfigure OK \
                 -command [ code $itk_component(fits_dialog) deactivate 1 ]
            $itk_component(fits_dialog) buttonconfigure Cancel \
                 -command [ code $itk_component(fits_dialog) deactivate 0 ]
            $itk_component(fits_dialog) hide Help
            $itk_component(fits_dialog) hide Apply

#  Add the main scrolledlistbox to hold the FITS headers.
            set geom 80x[ min [ llength $allfits ] 20 ]
            itk_component add fits_listbox {
               iwidgets::scrolledlistbox \
                  [ $itk_component(fits_dialog) childsite ].lb \
                  -visibleitems $geom \
                  -selectmode multiple \
                  -hscrollmode none \
                  -vscrollmode dynamic \
                  -textfont fixed
            }
            eval $itk_component(fits_listbox) insert end $allfits
            pack $itk_component(fits_listbox) -fill x

#  Add some help text.
            set helptext [ join {
               "This window displays the FITS headers of one of the images "
               "(the first one).\n"
               "\n"
               "Select any header card(s) of interest by clicking on them.\n"
               "They can be deselected by clicking again.\n"
               "\n"
               "The selected headers will then be displayed below each "
               "image in the chooser window."
            } "" ]
            itk_component add fits_help {
               label \
                  [ $itk_component(fits_dialog) childsite].help \
                  -justify left \
                  -text $helptext
            }
            pack $itk_component(fits_help) -anchor w

#  Post the window.
            $itk_component(fits_dialog) center $itk_component(showfits)
         }

#  Set the selections correctly.
         $itk_component(fits_listbox) selection clear 0 end
         foreach showhead $showfits {
            $itk_component(fits_listbox) selection set [ lindex $showhead 0 ]
         }

#  Whip out the FITS selection window and get the user to interact with it.
         if { [ $itk_component(fits_dialog) activate ] } {
            set showfits {}
            foreach i [ $itk_component(fits_listbox) curselection ] {
               set head [ lindex $allfits $i ]
               regexp {^[A-Z0-9_\-\.]+} $head key
               lappend showfits [ list $i $key ]
            }
         }

#  Cause the display to be updated.
         refresh all
      }


########################################################################
#  Public variables.
########################################################################

#-----------------------------------------------------------------------
      public variable viewport { 200 200 } {
#-----------------------------------------------------------------------
#  If the viewport size is changed, then all the windows will need to
#  be redrawn at the right size.
         if { $status == "active" } {
            if { $viewport != $lastvp } {
               set vpwidth [ lindex $viewport 0 ]
               set vpheight [ lindex $viewport 1 ]

#  Set the frames containing the images to the requested size.
               foreach slot { A B } {
                  $itk_component(view$slot) configure -width $vpwidth \
                                                      -height $vpheight
               }

#  Set the tabset and associated scrollbar to the right size in each slot.
               update idletasks
               set side(A) left
               set side(B) right
               foreach slot { A B } {
                  set bbox [ $itk_component(tabcanv$slot) bbox all ]
                  set tabwidth [ expr [ lindex $bbox 2 ] - [ lindex $bbox 0 ] ]
                  set tabheight [ lindex $bbox 3 ]
                  set sbheight [ winfo height $itk_component(ndf$slot) ]
                  $itk_component(tabcanv$slot) configure \
                       -width $tabwidth -height $sbheight \
                       -scrollregion $bbox

#  Only post the scrollbar if the tabset is too high to fit at once.
                  if { $tabheight > $sbheight } {
                     pack $itk_component(tabscroll$slot) \
                          -before $itk_component(tabcanv$slot) \
                          -fill y -anchor n -side $side($slot)
                  } else {
                     pack forget $itk_component(tabscroll$slot)
                  }
               }

#  Redraw all the windows.  This will only do an actual redraw of the
#  ones which are currently being viewed, but others will be marked for
#  redrawing at a later date.
               for { set i 0 } { $i < $nndfset } { incr i } {
                  $itk_component(image$i) configure -width $vpwidth \
                                                    -height $vpheight
               }
               refresh all
            }

#  Remember the shape of the window we have just configured.
            set lastvp $viewport
         }
      }


#-----------------------------------------------------------------------
      public variable status { } {
#-----------------------------------------------------------------------
         if { ! [ isvalid ] } {
            $itk_component(gotpair) configure -state disabled
         }
         if { $status == "active" } {
            configure -viewport $viewport

#  If we have not already done so, set a minimum size for the window.
            if { [ wm minsize $itk_interior ] == {1 1} } {
               set wnow [ winfo width $itk_interior ]
               set hnow [ winfo height $itk_interior ]
               wm minsize $itk_interior \
                  [ expr $wnow - 2 * [ lindex $viewport 0 ] + 200 ] \
                  [ expr $hnow - [ lindex $viewport 1 ] + 100 ]
            }
         }
      }


#-----------------------------------------------------------------------
      public variable percentiles { 5 95 } {
#-----------------------------------------------------------------------
      }


#-----------------------------------------------------------------------
      public variable displaystyle { } {
#-----------------------------------------------------------------------
         refresh all
      }


#-----------------------------------------------------------------------
      public variable validpair { expr 1 } {
#-----------------------------------------------------------------------
         $itk_component(gotpair) configure \
            -state [ expr [ isvalid ]?"normal":"disabled" ]
      }


#-----------------------------------------------------------------------
      public variable choosepercentiles { 0 } {
#-----------------------------------------------------------------------
      }


#-----------------------------------------------------------------------
      public variable choosewcsframe { 0 } {
#-----------------------------------------------------------------------
      }


#-----------------------------------------------------------------------
      public variable skip2 { 0 } {
#-----------------------------------------------------------------------
         if { $skip2 && $nndfset == 2 } {
            set inview(A) 1
            set inview(B) 2
         }
      }


########################################################################
#  Private variables.
########################################################################
      private variable allfits         ;# List of available FITS header lines
      private variable gseq 0          ;# Unique sequence number for gwm widgets
      private variable gwmwin          ;# Array by ndf index of gwm widget name
      private variable highlight       ;# Array by ndf index of highlight state
      private variable inview          ;# Array by slot of viewed images
      private variable noted           ;# Array containing info characteristics
      private variable lastvp { 1 1 }  ;# Last value of viewport variable
      private variable nndfset         ;# Number of images under chooser control
      private variable ndfsetlist      ;# List of ndfset objects
      private variable percentilecontrol;# Perc control widgets for each image
      private variable plotted         ;# Array containing plot characteristics
      private variable showfits {}     ;# FITS headers to display for each image
      private variable wcsframecontrol ;# Frame control widgets for each image
      private variable xmax 0          ;# Largest image current coords X size
      private variable ymax 0          ;# Largest image current coords Y size


      private common CCDdir $env(CCDPACK_DIR)

   }


########################################################################
#  Widget resource management
########################################################################

   itk::usual Ndfchoose {
      keep -background -cursor -foreground
   }
   option add *Ndfchoose.font "helvetica -12 normal" widgetDefault

   itk::usual Gwm {
      keep -background -cursor -foreground
   }


########################################################################
#  Constructor alias
########################################################################

   proc ndfchoose { pathname args } {
      uplevel Ndfchoose $pathname $args
   }


# $Id$
