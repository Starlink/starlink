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
#     ndfchoose pathname ndf ndf ?ndf ...? ?options?
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
#           - index     -- Index of the ndf whose percentiles are required
#
#     wcsframe index
#        This method returns the domain name of the WCS frame currently
#        selected for the given NDF.
#           - index     -- Index of the ndf whose frame name is required

#  Public Variables (Configuration Options):
#
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
#        user can select different values for different NDFs.
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
#        each previewd NDF is displayed.
#
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
         addcontrol $itk_component(gotpair) action
         addcontrol $itk_component(help) action
         addcontrol $itk_component(done) action

#  Construct the choosing area.  This is the business end of the widget
#  and consists of two tabsets and two frames in which NDFs can be 
#  displayed.  Each window is drawn once for each of the NDFs under the
#  widget's control, and this window is mapped or unmapped in one of
#  the frames by a geometry manager according to what the user selects.
#  First set a container frame within which all this display will happen.
         set interior [ childsite ]
         itk_component add choosearea {
            frame $interior.choosearea
         }

#  Set the dimensions of the windows to hold NDFs.
         set viewx [ lindex $viewport 0 ]
         set viewy [ lindex $viewport 1 ]

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
         bind $itk_interior <Configure> [ code $this winch ]
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
      public method wcsframe { index } {
#-----------------------------------------------------------------------
         set retval ""
         if { [ catch { $wcsframecontrol($index) cget -value } retval ] } {
            set retval ""
         }
         if { $retval == "" } {
            if { $index != "A" && $index != "B" } {
               set ndf [ lindex $ndflist $index ]
               set retval [ $ndf frameatt Domain CURRENT ]
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
                  $itk_component(tabsA) tabconfigure $i \
                      -label "$name +" \
                      -font "helvetica -12 bold"
                  $itk_component(tabsB) tabconfigure $i \
                      -label "+ $name" \
                      -font "helvetica -12 bold"
               }
               unhighlighted {
                  set name [ [ lindex $ndflist $i ] name ]
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

#  Ensure that the corresponding info window exists (certain widgets
#  on this are interrogated to do the display).
            ndfinfowindow $index

#  Get characteristics of the plot.
            set width [ lindex $viewport 0 ]
            set height [ lindex $viewport 1 ]
            if { $isndf } {
               set percs [ percentiles $index ]
               set wcsframe [ wcsframe $index ]
            } else {
               set percs 0
               set style 0
               set wcsframe 0
            }

#  Check whether the window exists and is out of date.  If so, destroy
#  it preparatory to generating a new one.
         if { [ array names itk_component plot$index ] != "" && \
              ( $plotted($index,width) != $width || \
                $plotted($index,height) != $height || \
                $plotted($index,percs) != $percs || \
                $plotted($index,wcsframe) != $wcsframe || \
                $plotted($index,displaystyle) != $displaystyle ) } {
            destroy $itk_component(plot$index)
            unset itk_component(plot$index)
         }

#  Check whether the window already exists.  If it does, all we need
#  to do is return the name.
         if { [ array names itk_component plot$index ] == "" } {

#  The window does not currently exist; we need to create and fill it.
#  Create the containing frame.
            container plot$index $itk_component(choosearea) \
                      [ expr "{$index}" == "{1}" ]

#  Create the GWM widget.
            if { $isndf } {

#  Display might be time-consuming: post a busy window.
               waitpush "Drawing image [ $ndf name ]"

#  Construct the GWM widget.
               set gwmname [ winfo id $itk_component(choosearea) ]_$index
               itk_component add plot$index:display {
                  gwm $itk_component(plot$index).gwm \
                      -width $width \
                      -height $height \
                      -name $gwmname
               }

#  Get the percentile values.
               set scalevals [ $ndf percentile [ lindex $percs 0 ] \
                                               [ lindex $percs 1 ] ]

#  Plot the NDF inside the GWM.
               set options {border=1 drawtitle=0 textlab=0 tickall=1 \
                            colour=3 colour(numlab)=5 colour(border)=4}
               lappend options $displaystyle
               $ndf display -resamp "$gwmname/GWM" \
                            [ lindex $scalevals 0 ] [ lindex $scalevals 1 ] \
                            $wcsframe [ join $options "," ]

             # set devname "xw;$gwmname"
             # taskrun lutable \
             #    "coltab=grey mapping=linear device=$devname reset"
             # taskrun display " \
             #       in=[ $ndf name ] \
             #       device=$devname \
             #       scale=true \
             #       mode=scale \
             #       low=[ lindex $scalevals 0 ] \
             #       high=[ lindex $scalevals 1 ] \
             #       margin=0 \
             #       style=\"drawtitle=0,tickall=1,$displaystyle\" \
             #    "

#  It may be a good idea to unmap the NDF here (although it may not).
               $ndf mapped 0

#  Remove the busy window.
               waitpop

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
                     -bitmap @$CCDdir/arrow_$side.xbm
               }
               pack $itk_component(plot$index:arrow) \
                  -side $side -anchor n -expand 0
               pack $itk_component(plot$index:instructions) \
                  -side $side -anchor n -expand 1 -fill both
            }
            pack $itk_component(plot$index:display) -expand 1 -fill both

#  Store characteristics of this plot so that we know whether subsequent 
#  plot requests are out of date.
            set plotted($index,width) $width
            set plotted($index,height) $height
            set plotted($index,percs) $percs
            set plotted($index,wcsframe) $wcsframe
            set plotted($index,displaystyle) $displaystyle
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

#  Store current values of the controls in the info window.
         set percval [ percentiles $index ]
         set wcsval [ wcsframe $index ]

#  Check whether the window exists and is out of date.  If so, destroy
#  it preparatory to generating a new one.
         if { [ array names itk_component info$index ] != "" } {
            if { $noted($index,showfits) != $showfits } {
               destroy $itk_component(info$index)
               unset itk_component(info$index)
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
            if { $isndf } { 
               set name [ $ndf name ]
               foreach dim [ $ndf bounds ] {
                  append dims "[ lindex $dim 0 ]:[ lindex $dim 1 ] x "
               }
               regsub " x $" $dims "" dims
            }

#  Construct a set of labelled widgets containing the information.
            set lws ""

#  Construct a list of key, value pairs to be displayed in the text region.
            set pairs {}
            lappend pairs [ list "Name" $name ]
            lappend pairs [ list "Dimensions" $dims ]
            if { ! $choosepercentiles } {
               lappend pairs [ list "Display cutoff" \
                            "[ lindex $percval 0 ]% - [ lindex $percval 1]%" ]
            }
            if { ! $choosewcsframe } {
               lappend pairs [ list "WCS frame" $wcsval ]
            }
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
               lappend lws $itk_component(info$index:key_$key)
            }

#  Put a percentile selection control in the text region.
            if { $choosepercentiles } {
               itk_component add info$index:key_percentile {
                  iwidgets::labeledwidget \
                     $itk_component(info$index).wpercentile \
                     -labeltext "Display cutoff:"
               }
               if { $isndf } {
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
               if { $isndf } {
                  itk_component add info$index:val_wcsframe {
                     wcsframecontrol [ \
                        $itk_component(info$index:key_wcsframe) childsite ].val
                  }
                  set wcsframecontrol($index) \
                     $itk_component(info$index:val_wcsframe)
                  $wcsframecontrol($index) configure \
                     -ndf $ndf \
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
         }

#  Return the name of the window.
         return $itk_component(info$index)
      }


#-----------------------------------------------------------------------
      private method refresh { index } {
#-----------------------------------------------------------------------
#  Should be called if some of the displayed (selected) plot or info
#  windows may have become out of date.
         update idletasks
         set oldbind [ bind $itk_interior <Configure> ]
         bind $itk_interior <Configure> ""
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
         bind $itk_interior <Configure> $oldbind
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
         update idletasks
         set xinc [ expr ( [ winfo width $itk_interior ] - \
                           [ winfo reqwidth $itk_interior ] ) / 2 ]
         set yinc [ expr [ winfo height $itk_interior ] - \
                         [ winfo reqheight $itk_interior ] ]
         if { $xinc != 0 || $yinc != 0 } {
            waitpush "Resizing chooser window"
            set oldbind [ bind $itk_interior <Configure> ]
            bind $itk_interior <Configure> ""
            configure -viewport [ list [ expr [ lindex $viewport 0 ] + $xinc ] \
                                       [ expr [ lindex $viewport 1 ] + $yinc ] ]
            update idletasks
            bind $itk_interior <Configure> $oldbind
            waitpop
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
         if { $viewport != $lastvp } {
            foreach slot { A B } {
               $itk_component(view$slot) configure \
                   -width [ lindex $viewport 0 ] -height [ lindex $viewport 1 ]
            }
            for { set i 0 } { $i < $nndf } { incr i } {
               $itk_component(image$i) configure \
                   -width [ lindex $viewport 0 ] -height [ lindex $viewport 1 ]
            }
            if { $status == "active" } {
               refresh all
            }
         }
         set lastvp $viewport
      }


#-----------------------------------------------------------------------
      public variable status { } {
#-----------------------------------------------------------------------
         if { ! [ isvalid ] } {
            $itk_component(gotpair) configure -state disabled
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


########################################################################
#  Private variables.
########################################################################
      private variable allfits         ;# List of available FITS header lines
      private variable highlight       ;# Array by ndf index of highlight state
      private variable inview          ;# Array by slot of viewed NDFs
      private variable noted           ;# Array containing info characteristics
      private variable lastvp { 0 0 }  ;# Last value of viewport variable
      private variable nndf            ;# Number of NDFs under chooser control
      private variable ndflist         ;# List of ndf objects
      private variable percentilecontrol;# Perc control widgets for each NDF
      private variable plotted         ;# Array containing plot characteristics
      private variable showfits {}     ;# FITS headers to display for each NDF 
      private variable wcsframecontrol ;# Frame control widgets for each NDF

      private common CCDdir $env(CCDPACK_DIR)

   }


########################################################################
#  Widget resource management
########################################################################

   itk::usual Ndfchoose {
      keep -background -cursor -foreground
   }
   option add *Ndfchoose.font "helvetica -12 normal" widgetDefault
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
