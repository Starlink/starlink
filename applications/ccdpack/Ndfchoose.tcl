   class Ndfchoose {
#+
#  Name:
#     Ndfchoose

#  Type of Module:
#     [incr Tk] Mega-Widget

#  Purpose:
#     Provide a method of choosing NDFs from a set.

#  Description:
#     This class provides a GUI which allows a user to choose pairs of
#     NDFs from a given set.  The user is given thumbnail images and
#     some textual information (like their names) and client widgets
#     can call methods in order to find out which NDFs have been chosen.

#  Constructor:
#
#     ndfchoose pathname ?options? ndf ndf ?ndf? ...
#        An NDF chooser object is constructed in the normal way; apart
#        from any configuration options, all arguments are existing ndf 
#        objects which are to form the list amongst which the user can
#        choose.  Ndf objects cannot be added to this list after 
#        construction time.  The ndf objects form an ordered list, 
#        and are subsequently referred to by index; the index of the 
#        first is 1, of the second is 2, etc.

#  Public Methods:
#
#     getpair
#        Returns an ordered list of two ndf indices, indicating the pair 
#        of NDFs which is currently selected by the user.  An invalid
#        ndf is indicated by an empty string.  If the -validate 
#        configuration variable has been specified, and the pair does
#        not satisfy this criterion, then a single empty string will
#        be returned.
#
#     highlight index ?state?
#        This method controls highlighting of the NDF indicated.
#        With no optional state argument it returns a value giving
#        the current state of highlighting.  If the state argument
#        is supplied, it will set the highlighting state to the given
#        value.  There are currently two highlighting states available,
#        0 and 1.  0 means not highlighted and 1 means highlighted.
#        All NDFs start in highlighting state 0.
#           - index     -- Index of the ndf whose state is to be set/queried
#           - ?state?   -- State into which ndf index should be put
#
#     percentiles index
#        This method returns a list giving the percentile cutoff
#        values which are used to display each NDF.  The list has two
#        elements {lo hi} where 0 <= lo <= hi <= 100.


#  Public Variables (Configuration Options):
#
#     percentiles = list
#        A list containing two numbers between 0 and 100.  This is used 
#        as a default percentile cutoff limit for NDF display, but the
#        user can select different values for different NDFs.
#
#     state = string
#        A value which gives the state of the object.  It may have the
#        following values:
#           active:
#              The viewer will attempt to reflect changes in its
#              configuration on the display.
#           inactive:
#              The viewer may not attempt to reflect changes in its
#              configuration on the display.  The viewer will only enter
#              this from the active state if a subsequent call of the 
#              getpair method will return a valid pair.
#           done:
#              The viewer's work is done (e.g. the exit button has
#              been pressed).
#
#        Only the values 'active' and 'inactive' may be written into this
#        variable from outside.  This variable may be tracked from
#        outside the class (for instance if a trace is to be run on it)
#        using the 'watchstate' public variable.
#
#     validpair = string
#        This gives an expression which indicates whether a pair is 
#        valid or not.  Before use any occurrences of the sequences "%A"
#        and "%B" are replaced by the indices of the first and second
#        members of the pair respectively.  The string is then evaluated
#        to give a boolean result determining whether the pair is valid
#        or not.  The state variable will only change from "active" to
#        "inactive" if the getpair method will return a pair which 
#        satisfies this criterion.
#
#     watchstate = string
#        This gives a name of a variable in the caller's context which
#        will be kept up to date with the state of this object, i.e.
#        it will have the same value as the object's $state public
#        variable.  It is useful to configure this so that the variable
#        can be traced to watch for changes in state.
#-

#  Inheritance.
      inherit itk::Toplevel


########################################################################
#  Constructor.
########################################################################
      constructor { args } {

#  Parse the arguments: any arguments before the first one starting with
#  a minus sign are interpreted as ndf objects, and any following are
#  configuration options.  We happen to know that ndf objects never 
#  start with a minus sign, so no ambiguities can arise.  Also validate
#  that the arguments are what they ought to be.
         set ndflist "<blank>"
         set nndf 0
         while { [ llength $args ] > 0 && \
                 [ string index [ lindex $args 0 ] 0 ] != "-" } {
            set ndf [ lindex $args 0 ]
            if { [ catch { $ndf validndf } ] } {
               error "Argument \"$ndf\" is not a valid ndf object."
            }
            set highlight([incr nndf]) 0
            lappend ndflist $ndf
            set args [ lreplace $args 0 0 ]
         }
         if { $nndf == 0 } {
            error "No ndf objects supplied"
         }

#  Initialise values of selected NDFs.
         set inview(A) ""
         set inview(B) ""

#  Set the list of all available FITS header lines.  Use those from the 
#  first named NDF as an example.
         set allfits [ [ lindex $ndflist 1 ] fitshead ]

#  Calculate the dimensions available for viewing the NDFs themselves.
         set viewx [ expr [ lindex $viewport 0 ] / 2 ]
         set viewy [ lindex $viewport 1 ]

#  Construct the top level container frame.
         container body $itk_interior 1
         pack $itk_component(body)
       
#  Construct the control panel.
         itk_component add panel {
            frame $itk_component(body).controls
         }
         itk_component add showfits {
            button $itk_component(panel).showfits \
               -text "Show FITS" \
               -command [ code $this fitsselect ]
         }
         lappend controls $itk_component(showfits)
         itk_component add dstyle {
            stylecontrol $itk_component(panel).dstyle \
               -value "drawaxes=0,grid=0,numlab=0" \
               -valuevar displaystyle
         }
         lappend controls $itk_component(dstyle)
         itk_component add gotpair {
            button $itk_component(panel).gotpair \
               -text "Use this pair" \
               -command [ code $this configure -state inactive ] \
               -state disabled
         } {
            usual
            ignore -state
         }
         lappend controls $itk_component(gotpair)
         itk_component add done {
            button $itk_component(panel).done \
               -text "Exit" \
               -command [ code $this configure -state done ]
         }
         lappend controls $itk_component(done)

         eval pack $controls -side left

#  Construct the choosing area.  This is the business end of the widget
#  and consists of two tabsets and two frames in which NDFs can be 
#  displayed.  Each window is drawn once for each of the NDFs under the
#  widget's control, and this window is mapped or unmapped in one of
#  the frames by a geometry manager according to what the user selects.
#  First set a container frame within which all this display will happen.
         itk_component add choosearea {
            frame $itk_component(body).choosearea
         }

#  Most elements of the choosearea have an A side and a B side; construct
#  these in a loop.
         set tabpos(A) w
         set tabpos(B) e
         set side(A) left
         set side(B) right
         foreach slot { A B } {

#  Construct the tabsets, in their own frame so they can get packed at 
#  the top edge of it.
            itk_component add tabs$slot {
               iwidgets::tabset $itk_component(choosearea).tabs$slot \
                                -tabpos $tabpos($slot) \
                                -bevelamount 4 \
                                -tabborders no \
                                -command [ code $this ndfselect $slot ] 
            }

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
            for { set i 1 } { $i < [ llength $ndflist ] } { incr i } {
               $itk_component(tabs$slot) add -label " "
            }

#  Pack the ndf frames themselves and the controlling tabsets into the 
#  choosing area.
            pack $itk_component(tabs$slot) -side $side($slot) -fill y -anchor n
            pack $itk_component(ndf$slot) -side $side($slot)

#  Set the initially selected NDFs to blank ones.
            ndfselect $slot 0
         }

#  Initialise the NDF tab labels to the unhighlighted state.
         setstate unhighlighted all

#  Pack the components into the hull.
         pack $itk_component(choosearea) -side top
         pack $itk_component(panel) -side top

#  Save the initial dimensions of the window.
         update idletasks
         set lastxwin [ winfo width $itk_interior ]
         set lastywin [ winfo height $itk_interior ]

#  Generate frames which will contain the windows to be displayed for each
#  of the NDFs.
         for { set i 0 } { $i < $nndf } { incr i } {
            itk_component add image$i {
               frame $itk_component(choosearea).image$i \
                         -width $viewx -height $viewy
            }
         }

#  Do requested configuration.
         eval itk_initialize $args

#  Set a binding to handle window resize events.
         # This doesn't work yet - partly because of a bug in GWM.
         # bind $itk_component(choosearea) <Configure> [ code $this winch ]
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
         if { $index < 1 || $index > $nndf } {
            error "Highlight index out of range"
         }
         if { $state ==  "" } {
            return $highlight($index)
         } else {
            set hightlight($index) $state
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
         return [ $percentilecontrol($index) cget -value ]
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
            for { set i 1 } { $i <= $nndf } { incr i } {
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
                  set name [ [ lindex $ndflist $i ] name ]
                  $itk_component(tabsA) tabconfigure $i -label "$name +"
                  $itk_component(tabsB) tabconfigure $i -label "+ $name"
               }
               unhighlighted {
                  set name [ [ lindex $ndflist $i ] name ]
                  $itk_component(tabsA) tabconfigure $i -label "$name   "
                  $itk_component(tabsB) tabconfigure $i -label "   $name"
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

#  Validate the index argument.
         if { $index == "A" } {
            set isndf 0
            set side left
            set textpos "-y 10 -relx 0 -x 10 -anchor nw"
         } elseif { $index == "B" } {
            set isndf 0
            set side right
            set textpos "-y 10 -relx 1 -x -10 -anchor ne"
         } elseif { $index > 0 } {
            set isndf 1
            set ndf [ lindex $ndflist $index ]
         } else {
            error "Invalid index argument \"$index\" to ndfplotwindow"
         }

#  Check whether the window already exists.  If it does, all we need
#  to do is return the name.
         if { [ array names itk_component plot$index ] == "" } {

#  The window does not currently exist; we need to create and fill it.

#  First ensure that the corresponding info window exists (certain widgets
#  on this are interrogated to do the display).
         ndfinfowindow $index

#  Create the containing frame.
            container plot$index $itk_component(choosearea) \
                      [ expr "{$index}" == "{1}" ]

#  Create the GWM widget.
            set width [ expr [ lindex $viewport 0 ] / 2 ]
            set height [ lindex $viewport 1 ]
            if { $isndf } {
               set gwmname [ winfo id $itk_component(choosearea) ]_$index
               itk_component add plot$index:display {
                  gwm $itk_component(plot$index).gwm \
                      -width $width \
                      -height $height \
                      -name $gwmname
               }
#  Plot the NDF inside the GWM.
               set percs [ percentiles $index ]
               set scalevals [ $ndf percentile [ lindex $percs 0 ] \
                                               [ lindex $percs 1 ] ]
               set devname "xw;$gwmname"
               iCCDRunTask \
                  display " \
                     in=[ $ndf name ] \
                     device=$devname \
                     scale=true \
                     mode=scale \
                     low=[ lindex $scalevals 0 ] \
                     high=[ lindex $scalevals 1 ] \
                     margin=0 \
                     style=\"drawtitle=0,tickall=1,$displaystyle\" \
                  " \
                  3 $itk_component(choosearea)

               iCCDRunTask \
                  lutable "coltab=grey mapping=linear device=$devname reset" \
                  3 $itk_component(choosearea)

#  It may be a good idea to unmap the NDF here (although it may not).
               $ndf mapped 0

#  This is a blank window; write instructions to the user.
            } else {
               itk_component add plot$index:display {
                  frame $itk_component(plot$index).blank \
                      -width $width \
                      -height $height
               }
               set msg "Use the tabs to\npick an NDF for\ndisplay on the $side"
               itk_component add plot$index:instructions {
                  label $itk_component(plot$index:display).instruct$index \
                     -justify $side -text $msg
               }
               itk_component add plot$index:arrow {
                  label $itk_component(plot$index:display).arrow$index \
                     -bitmap @arrow_$side.xbm
               }
               pack $itk_component(plot$index:arrow) \
                  -side $side -anchor n -expand 0
               pack $itk_component(plot$index:instructions) \
                  -side $side -anchor n -expand 1 -fill both
            }
            pack $itk_component(plot$index:display) -expand 1 -fill both
         }

#  Return the name of the window.
         return $itk_component(plot$index)
      }


#-----------------------------------------------------------------------
      private method ndfinfowindow { index } {
#-----------------------------------------------------------------------
#  This method returns the name of a window which contains textual 
#  annotations for the drawn NDF, i.e. its name, applicable FITS headers,
#  etc.  The argument may be either the numerical index of one of the
#  ndfs in the list, or a slot name (A or B).  In the latter case, a 
#  suitable blank window will be returned.

#  Validate the index argument.
         if { $index == "A" || $index == "B" } {
            set isndf 0
         } elseif { $index > 0 } {
            set isndf 1
            set ndf [ lindex $ndflist $index ]
         } else {
            error "Invalid index argument \"$index\" to ndfinfowindow"
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
            if { $isndf } { 
               set name [ $ndf name ]
               foreach dim [ $ndf bounds ] {
                  append dims "[ lindex $dim 0 ]:[ lindex $dim 1 ] x "
               }
               regsub " x $" $dims "" dims
            }

#  Put a percentile selection button in the text region.
            itk_component add info$index:key_percentile {
               iwidgets::labeledwidget $itk_component(info$index).wpercentile \
                  -labeltext "Display cutoff:"
            }
            if { $isndf } {
               itk_component add info$index:val_percentile {
                  percentilecontrol [ \
                     $itk_component(info$index:key_percentile) childsite ].val \
                     -allowcustom 1 \
                     -value $percentiles \
                     -command [ code $this refreshplot $index ]
               }
               set percentilecontrol($index) \
                  $itk_component(info$index:val_percentile)
               lappend controls $percentilecontrol($index)
               pack $itk_component(info$index:val_percentile)
            }
            pack $itk_component(info$index:key_percentile) -side top -anchor w

#  Construct a list of key, value pairs to be displayed in the text region.
            set pairs {}
            lappend pairs [ list "Name" $name ]
            lappend pairs [ list "Dimensions" $dims ]
            foreach fh $showfits {
               set key [ lindex $fh 1 ]
               if { $isndf } {
                  set val [ lindex [ $ndf fitshead $key ] 0 ]
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
               if { $isndf } {
                  itk_component add info$index:val_$key {
                     label [ $itk_component(info$index:key_$key) \
                             childsite ].val \
                        -text $val
                  }
                  pack $itk_component(info$index:val_$key)
               }
               pack $itk_component(info$index:key_$key) -side top -anchor w
            }
         }

#  Return the name of the window.
         return $itk_component(info$index)
      }


#-----------------------------------------------------------------------
      private method refreshinfo { index } {
#-----------------------------------------------------------------------
         if { $index == "all" } {
            foreach comp [ array names itk_component {info[0-9AB]*} ] {
               destroy $itk_component($comp)
               unset itk_component($comp)
            }
         } else {
            destroy $itk_component($index)
            unset itk_component($index)
         }
         foreach slot { A B } {
            if { $inview($slot) == $slot } {
               ndfselect $slot 0
            } else {
               ndfselect $slot $inview($slot)
            }
         }
      }


#-----------------------------------------------------------------------
      private method refreshplot { index } {
#-----------------------------------------------------------------------
#  This probably isn't going to work until the GWM bug is fixed.  However,
#  if it did, it would be how to implement resizing the NDFs after a 
#  window resize event.
         if { $index == "all" } {
            foreach comp [ array names itk_component {info[0-9AB]*} ] {
               destroy $itk_component($comp)
               unset itk_component($comp)
            }
         } else {
            destroy $itk_component($index)
            unset itk_component($index)
         }
         foreach slot { A B } {
            if { $inview($slot) == $slot } {
               ndfselect $slot 0
            } else {
               ndfselect $slot $inview($slot)
            }
         }
      }


#-----------------------------------------------------------------------
      private method ndfselect { slot index } {
#-----------------------------------------------------------------------
#  If item is zero, this indicates a blank window.  Otherwise, it is the
#  window corresponding to the indicated NDF.
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
         } else {
            set item $index
            set pos "-relx 0.5 -rely 0.5 -anchor center"
         }
         set inview($slot) $item
         eval place [ ndfplotwindow $item ] \
            -in $itk_component(view$slot) $pos
         pack [ ndfinfowindow $item ] \
            -in $itk_component(describe$slot) -anchor w
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
      private method winch { } {
#-----------------------------------------------------------------------
#  This method is called if the toplevel window receives a configure
#  event, which will happen, for instance, if it is resized.
      #  set thisxwin [ winfo width $itk_interior ]
      #  set thisywin [ winfo height $itk_interior ]
      #  set vx [ expr [ lindex $viewport 0 ] + $thisxwin - $lastxwin ]
      #  set vy [ expr [ lindex $viewport 1 ] + $thisywin - $lastywin ]
      #  set lastxwin $thisxwin
      #  set lastywin $thisywin
      #  configure -viewport [ list $vx $vy ]
      }


#-----------------------------------------------------------------------
      private method container { component parent { install 0 } } {
#-----------------------------------------------------------------------
#  This method creates a frame capable in terms of colormaps of holding
#  a GWM widget.  If it can create a window using a visual which is not
#  going to run out of colours, this does not require extra work. 
#  However, if we are stuck with a PseudoColor visual then creating
#  lots of GWM widgets will fail, since each needs to allocate a significant
#  number of colours (is PseudoColor the only one in which this might 
#  happen?).  In this case, this method will create a frame
#  with a new colormap.  In order that the colormaps of the various 
#  parts of the widget are consistent with each other, it then messes
#  about in the new window in such a way as to cause allocation of 
#  the colours which are going to need to be consistent within the
#  all the internal windows of the widget; since the same thing 
#  happens in every window just after it is created, hopefully they
#  ought to end up with the bottom ends of their colormaps looking the
#  same.  Probably this is not guaranteed by X, but (a) it seems to work
#  on the PseudoColor display I have to hand, and (b) I've got no idea
#  how else to go about it.  If the optional install argument is set
#  true, then window manager will be instructed to use the new colormap
#  for the widget when it gets colormap focus (usually if the pointer is
#  over it).
#
#  A simpler solution would be to force all the windows to use the same
#  colormap, however I don't think this is possible since each GWM
#  widget insists on grabbing a bunch of new colours from the colormap
#  when it starts up.
#
#  My knowledge of X is not encyclopaedic, so there may be any number of
#  things I'm doing wrong here.
         set path $parent.hold_$component

#  Find out what visual we are going to use.  If possible, use the visual
#  of the parent window, since it's likely to be what the display is 
#  most at home using (though possibly one could be smarter about making
#  this decision).  If it's not suitable though, pick something from
#  what is available.
         set parentvisual [ winfo visual $parent ]
         set visual $parent
         set pseudo 0
         if { $parentvisual == "pseudocolor" } {
            set visual $parent
            set pseudo 1
         } elseif { $parentvisual == "truecolor" } {
            set visual $parent
            set pseudo 0
         } else {
            foreach vis [ winfo visualsavailable $parent ] {
               set type [ lindex $vis 0 ]
               set depth [ lindex $vis 1 ]
               if { $type == "pseudocolor" && $depth >= 8 } { 
                  set visual $vis
                  set pseudo 1
                  break
               } elseif { $type == "truecolor" && $depth >= 12 } {
                  set visual $vis
                  set pseudo 0
                  break
               }
            }
         }

#  If it's not going to be PseudoColor, we just need to create a frame.
         if { ! $pseudo } {
            itk_component add $component {
               frame $path
            }

#  It will be PseudoColor.  Create a frame with a new colormap, and
#  then create some windows in it which will force allocation of colours
#  we're going to need in the first few slots.
         } else {
            itk_component add $component {
               frame $path -colormap new
            }
            iwidgets::pushbutton $path.but -text T
            # button $path.but -text T

#  If required, notify the window manager that this colormap should be the
#  one used for the toplevel window.
            if { $install } {
               wm colormapwindows $itk_interior $path
            }
         }
      }


#-----------------------------------------------------------------------
      private method fitsselect { } {
#-----------------------------------------------------------------------
#  This method interrogates the user to update the showfits private 
#  variable.

#  If we haven't done this before, build the FITS selection window.
         if { [ array names itk_component fits_dialog ] == "" } {
            itk_component add fits_dialog {
               iwidgets::dialog $itk_interior.d \
                  -modality application \
                  -title "Select FITS headers for display"
            }
            $itk_component(fits_dialog) buttonconfigure OK \
                 -command [ code $itk_component(fits_dialog) deactivate 1 ]
            $itk_component(fits_dialog) buttonconfigure Cancel \
                 -command [ code $itk_component(fits_dialog) deactivate 0 ]
            $itk_component(fits_dialog) hide Help
            $itk_component(fits_dialog) hide Apply
            set geom 80x[ min [ llength $allfits ] 20 ]
            itk_component add fits_listbox {
               iwidgets::scrolledlistbox \
                  [ $itk_component(fits_dialog) childsite ].lb \
                  -visibleitems $geom \
                  -selectmode multiple \
                  -hscrollmode none \
                  -vscrollmode dynamic
            } {
               rename -textfont -fitsfont fitsFont Font
            }
            eval $itk_component(fits_listbox) insert end $allfits
            pack $itk_component(fits_listbox) -fill x
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
         refreshinfo
      }


########################################################################
#  Private procedures.
########################################################################

#-----------------------------------------------------------------------
      private proc max { num args } {
#-----------------------------------------------------------------------
         foreach x $args {
            if { $x > $num } { set num $x }
         }
         return $num
      }


#-----------------------------------------------------------------------
      private proc min { num args } {
#-----------------------------------------------------------------------
         foreach x $args {
            if { $x < $num } { set num $x }
         }
         return $num
      }


########################################################################
#  Public variables.
########################################################################

#-----------------------------------------------------------------------
      public variable viewport { 400 200 } {
#-----------------------------------------------------------------------
#  It ought to be possible to reconfigure viewport so that all the 
#  NDF windows get drawn at the new size.  Currently this is problematic
#  because I can't recreate GWM items in the same place without getting
#  an error.  If this gets sorted out though, what I'll need to do here
#  is ensure that all the currently existing NDF display windows are
#  undrawn.
         # refreshplot all
      }


#-----------------------------------------------------------------------
      public variable percentiles { 5 95 } {
#-----------------------------------------------------------------------
      }


#-----------------------------------------------------------------------
      public variable displaystyle { } {
#-----------------------------------------------------------------------
         # refreshplot all
      }


#-----------------------------------------------------------------------
      public variable watchstate "" {
#-----------------------------------------------------------------------
         set watchlevel [expr [info level] - 1]
         upvar #$watchlevel $watchstate wstate
         set wstate $state
      }


#-----------------------------------------------------------------------
      public variable state "inactive" {
#-----------------------------------------------------------------------
         if { $state == "inactive" || $state == "active" || $state == "done" } {
            if { $watchstate != "" } {
               upvar #$watchlevel $watchstate wstate
               set wstate $state
            }
            if { $state == "active" } {
               foreach c $controls {
                  $c configure -state normal
               }
            } elseif { $state == "inactive" } {
               foreach c $controls {
                  $c configure -state disabled
               }
            }
         } else {
            error "Invalid value \"$state\" for state"
         }
      }


#-----------------------------------------------------------------------
      public variable validpair { expr 1 } {
#-----------------------------------------------------------------------
         $itk_component(gotpair) configure \
            -state [ expr [ isvalid ]?"normal":"disabled" ]
      }


########################################################################
#  Private variables.
########################################################################
      private variable allfits         ;# List of available FITS header lines
      private variable controls        ;# List of control panel widgets
      private variable highlight       ;# Array by ndf index of highlight state
      private variable inview          ;# Array by slot of viewed NDFs
      private variable lastviewport { 0 0 }
      private variable lastxwin 0
      private variable lastywin 0
      private variable nndf            ;# Number of NDFs under chooser control
      private variable ndflist         ;# List of ndf objects
      private variable percentilecontrol;# Perc control widgets for each NDF
      private variable showfits {}     ;# FITS headers to display for each NDF 
      private variable watchlevel 0    ;# Call stack level of calling code

   }


########################################################################
#  Widget resource management
########################################################################

   itk::usual Ndfchoose {
      keep -background -cursor -foreground
   }
   option add *Ndfchoose.fitsFont fixed widgetDefault

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
