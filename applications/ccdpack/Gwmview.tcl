class Gwmview {
#+
#  Name:
#     Gwmview

#  Type of module:
#     [incr Tk] Mega-Widget

#  Purpose:
#     Provide a window which can hold a GWM display.

#  Description:
#     This class provides a mega-widget for holding a GWM canvas item
#     on a canvas.  The GWM item can be destroyed and recreated at
#     a different size on the same widget.  As well as the GWM display
#     itself this widget contains buttons which are likely to be 
#     useful in conjunction with it, in particular enough buttons for
#     the user to be able to do the following:
#        - Indicate that the widget is no longer needed
#        - Specify what kind of colour table is wanted (not yet done)
#        - Specify a zoom level
#        - Specify the number of the next point to be drawn (optional)
#
#     The value of the zoomlevel is significant to the widget in that 
#     the 'view'-type coordinates with which one talks to this widget 
#     are mapped to pixel values using it.
#
#     This widget does any necessary worrying about scrolling the canvas,
#     resizing the window, and so on.  Resizing is allowed, but there
#     is a minimum size determined by the size of the buttons panel
#     (currently not properly implemented).
#
#     Some methods supplied by this widget require or return coordinates
#     on the GWM item.  In these cases 'view coordinates' are used;
#     the origin and extent of the GWM window in view coordinates is set 
#     when it is constructed with the makegwm method, so that it can
#     effectively be addressed in any coordinate system of your choice.
#     The methods canv2view and view2canv are supplied for converting 
#     between this coordinate system and Tk canvas coordinates which 
#     are needed if the canvas needs to be addressed directly, for 
#     instance to create canvas item bindings.

#  Public methods:
#
#     activate
#        Sets the state so that the widget is ready to be used.  This 
#        method should be called after sufficient configuration has been
#        done that the widget can usefully be displayed and the user
#        can begin to interact with it.  Before calling this method
#        the widget is likely to be in a non-interacting state; many 
#        of the other methods will not take any effect until the 
#        widget has been put into an active state.
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
#        If adding this point would cause the number of points in the
#        marked points list to exceed the maxpoints configuration 
#        variable, then no point will be added and the value -1 (which
#        is never a valid point index) will be returned.
#
#     canvas
#        Returns the name of the canvas widget on which any GWM item 
#        will be drawn.  This name will not change during the lifetime
#        of the Gwmview widget.
#
#     canv2view cx cy
#        Converts canvas coordinates to viewer coordinates.  Returns a
#        two-element list giving viewer coordinates corresponding to
#        the arguments.
#           - cx       -- X coordinate in Tk canvas coordinates
#           - cy       -- Y coordinate in Tk canvas coordinates
#
#     childsite
#        Returns the path of a frame into which windows can be placed.
#
#     clearpoints
#        Clears the contents of the marked points list, and erases all 
#        the corresponding marked points from the display.
#        This may be more efficient than calling the removepoint method
#        on each point individually.
#
#     deactivate
#        Put the widget into the 'inactive' state.  After this call many
#        of the other methods may not reflect their activity in the 
#        visible state of the widget.  Inquiring the state of the widget,
#        for instance using the points method, should continue to work
#        however.  This method can be called to prevent further user
#        interaction with the widget and is called when the 'Done' 
#        button is pushed.  This method can be overridded by children 
#        of this class to do additional shutdown, for instance to 
#        remove bindings (see the unbindall method).
#
#     devname
#        Returns the name of the graphics device associated with the GWM, 
#        as used for submitting to ADAM tasks.
#
#     display
#        Updates the display with respect to the current state of the
#        widget.  It is called, for instance, when the zoomfactor is 
#        updated.  This may not do much in this widget, but widgets
#        which inherit from it can override this method to do whatever
#        display updating they may require.  This method may be 
#        called more often than is necessary, so if it is going to do
#        anything slow then it ought to check whether a redisplay is
#        really required before doing it.
#
#     gwmname
#        Returns the name of the GWM.
#
#     makegwm xorigin yorigin xsize ysize ?name?
#        Actually makes the GWM display, as a GWM item on a scrolled
#        canvas whose name can be got using the canvas method.
#           - xorigin  -- X view coordinate of the bottom left corner
#           - yorigin  -- Y view coordinate of the bottom left corner
#           - xsize    -- X extent in view coordinates of the GWM item
#           - ysize    -- Y extent in view coordinates of the GWM item
#           - ?name?   -- Name of the GWM (a unique name will be generated
#                         if this is not given)
#
#        The size of the resulting GWM display will be [xy]size * zoomfactor
#        screen pixels.  Note that since zoomfactor depends on the
#        pixelsize variable as well as the zoomlevel variable,
#        pixelsize must have a sensible value in terms of the chosen
#        view coordinate system in order to give you a sensibly-sized
#        GWM widget.
#
#        Only one GWM display is allowed per Gwmview widget.  If one
#        already exists, using this method will replace it with a new one.
#        The name of the GWM and its corresponding device should be got 
#        using the gwmname or devname methods respectively.
#
#        NOTE: there is/was a bug in the GWM canvas item and/or its
#        PGPLOT driver.  To work around this, the makegwm method always
#        puts the created GWM canvas item at coordinates {0 0}, not those
#        indicated by xorigin and yorigin.  A calling application should
#        call makegwm, then do any PGPLOT operations required on the 
#        GWM, then do:
#           $canvas move gwmitem [ lindex $shift 0 ] [ lindex $shift 1 ]
#        where $shift is the return value of the makegwm method.  
#        If xorigin and yorigin are specified as zero, this step may be
#        skipped.  Hopefully the bug will be removed one day.
#        The bug is now fixed in some versions of PGLPLOT.  See Notes 
#        section.
#
#     markertype type colour size
#        Sets the current marker style; any markers subsequently
#        drawn will be drawn according to the last call of this method.
#           - type     -- An integer giving the shape.  Currently between
#                         1 and 8.
#           - colour   -- The colour to draw in (normal Tk format)
#           - size     -- Approximate height in canvas pixels of the marker
#
#     maxcanvas
#        Returns a number a bit bigger than the larger of the width and
#        the height of the most recently created GWM item.  The intention
#        is that this can be used to limit the size of canvas of future
#        canvases created to be not too much bigger than the last one,
#        since this is likely to avoid trouble for the user when switching
#        between display of a small and a much larger image.
#        Note this returns a value in screen (canvas) coordinates, not
#        view coordinates.
#
#     points
#        Returns a list of currently marked points on the image.
#        The returned list has one element for each marked point, each 
#        of these elements is of the form {index xpos ypos}.  Xpos and
#        ypos are in view coordinates.
#
#     refreshpoints
#        Draws all the points which are currently in the points list.
#        This method can be called after clearing some or all of the 
#        canvas to ensure that all the points which are current get
#        marked on it.
#
#     removegwm
#        Removes any existing GWM display from the widget.  It is
#        not an error to call this method if there is no extant GWM
#        display.
#
#     removepoint ipoint
#        Removes the point with index ipoint from the points list.
#           - ipoint   -- Index in the points list of the point to be removed
#
#     unbindall
#        Removes all bindings to items on the canvas on which the GWM item
#        resides.
#
#     view2canv vx vy
#        Converts viewer coordinates to canvas coordinates.  Returns a
#        two-element list giving canvas coordinates corresponding to
#        the arguments.
#           - vx       -- X coordinate in Gwmview viewer coordinates
#           - vy       -- Y coordinate in Gwmview viewer coordinates
#
#     Gwmview also inherits all the public methods of itk::Toplevel.
 
#  Public variables (configuration options):
#
#     geometry = string
#        This gives the geometry in the normal X11 format.
#
#     lutable = list
#        This is a one or two element list giving the configuration of
#        the colour lookup table.  The first element gives the name 
#        of the table (COLTAB parameter of KAPPA's LUTABLE task) and 
#        the second element, if present, gives the map type (MAPPING 
#        parameter of LUTABLE) - if absent the value LINEAR is used.
#
#     maxpoints = integer
#        The maximum number of entries in the marked points list 
#        associated with the widget.  If zero, there is no limit.
#
#     pixelsize = real
#        This variable is provided for convenience, to modify the way 
#        the zoomfactor variable is accessed.  It should be set to
#        the (approximate) linear size of a pixel (e.g. an NDF pixel)
#        in view coordinates.  This variable controls the actual size 
#        in screen pixels of the GWM created by the makegwm method.
#
#     state = string
#        A value which gives the state of the object.  It may have the
#        following values:
#           inactive:  
#              The viewer will not attempt to reflect changes in its 
#              configuration on the display.
#           active:
#              The viewer will attempt to reflect changes in its 
#              configuration on the display.
#           done:
#              The viewer's work is done (e.g. the exit button has 
#              been pressed).
#
#        Only the values 'active' and 'inactive' may be written into this 
#        variable from outside.  This variable may be tracked from 
#        outside the class (for instance if a trace is to be run on it) 
#        using the 'watchstate' public variable.
#
#     watchstate = string
#        This gives a name of a variable in the caller's context which 
#        will be kept up to date with the state of this object, i.e.
#        it will have the same value as the object's $state public 
#        variable.  It is useful to configure this so that the variable
#        can be traced to watch for changes in state.
#
#     zoom = real
#        A factor giving the number of screen pixels to an NDF pixel.
#
#     zoomfactor = real
#        A factor converting from view coordinates to Gwm coordinates
#        (differs from zoom by a factor of pixelsize).
#
#     Gwmview also inherits all the public variables of itk::Toplevel
 
#  Public procedures:
#
#     max num ...
#        Gives the maximum of all the (numeric) arguments supplied.
#        Just here because it's useful.
#
#     mim num ...
#        Gives the minimum of all the (numeric) arguments supplied.
#        Just here because it's useful.

#  Notes:
#     The canvas on which the GWM item is displayed can be accessed
#     by using its widget name, which is held in the public variable
#     canvas.  The following tag names are used to refer to items
#     on the canvas:
#        - gwmitem  -- The GWM item
#        - markN    -- Marker number N
#        
#     The display method contains elements of a workaround for a bug in
#     the PGPLOT driver for the GWM widget.  This is fixed in PGPLOT
#     5.2.0-6, but not in the Spring 2000 release.  When the fixed version
#     can be relied on to be there, this should be removed.

#-

#  Inheritance.
      inherit itk::Toplevel


########################################################################
#  Constructor.
########################################################################
      constructor { args } {

#  Add buttons in a panel.
         itk_component add panel {
            frame $itk_interior.buttons
         }
         itk_component add zoom {
            zoomcontrol $itk_component(panel).zoom \
               -min 0.05 \
               -max 8 \
               -value 1 \
               -valuevar zoom
         }
         lappend controls $itk_component(zoom)
         itk_component add marknum {
            marknumcontrol $itk_component(panel).marknum
         }
         lappend controls $itk_component(marknum)
         itk_component add exit {
            button $itk_component(panel).exit \
               -text "Done" \
               -command [ code $this deactivate ]
         }
         lappend controls $itk_component(exit)

      #  itk_component add colour {
      #     button $itk_component(panel).colour \
      #        -text "Colours" -command "$this docolours"
      #  }

         eval pack $controls -side left

#  Add the frame to hold the actual GWM viewing window.
         itk_component add viewarea {
            frame $itk_interior.viewarea -relief groove -borderwidth 2
         }
         itk_component add canvas {
            iwidgets::scrolledcanvas $itk_component(viewarea).canvas
         } {
            usual
            rename -textbackground \
                   -canvasbackground canvasBackground Background
         }

         pack $itk_component(canvas) -fill both -expand 1
         set canvas $itk_component(canvas)

#  Add info panel.
         itk_component add info {
            label $itk_interior.info -relief groove
         }

#  Add childsite panel.
         itk_component add childsite {
            frame $itk_interior.childsite
         }

#  Pack widgets into the hull.
         pack $itk_component(panel) -side bottom -fill none -expand 0
         pack $itk_component(childsite) -side bottom -fill x -expand 0
         pack $itk_component(info) -side bottom -fill x -expand 0
         pack $itk_component(viewarea) -side bottom -fill both -expand 1

#  Initialise marker type.
         markertype 2 red 10

#  Do requested configuration.
         wm withdraw $itk_interior
         eval itk_initialize $args
         wm deiconify $itk_interior

#  Set binding to handle window resize events.
         bind $itk_interior <Configure> [ code $this winch ]

#  Arrange for geometry constraints to be met.
         update idletasks
         set wnow [ winfo width $itk_interior ]
         set hnow [ winfo height $itk_interior ]
         wm minsize $itk_interior $wnow [ min $hnow $wnow ]
         configure -geometry $geometry
      }


########################################################################
#  Public methods.
########################################################################

#-----------------------------------------------------------------------
      public method makegwm { xorigin yorigin xsize ysize { name "" } } {
#-----------------------------------------------------------------------
#  Keep a record of the size of the canvas.
         set gwmdims [ list $xsize $ysize ]
         set gwmorigin [ list $xorigin $yorigin ]

#  Clear out any GWM items which already exist on the canvas.
         removegwm

#  Get the GWM name.
         if { $name == "" } {
            set name [ winfo id $itk_interior ]
         }
         set gwmname $name

#  There seems to be a bug in the GWM canvas item and/or its PGPLOT driver.
#  It means that it is effectively only possible to plot correctly on a
#  GWM canvas item if its creation coordinates, and its current coordinates,
#  are {0 0}.  Thus this method always puts the GWM item at the origin,
#  but returns the distance away from its requested coordinates that it
#  is.  The calling application can then do a $canvas move gwmitem to
#  move it after it has performed whatever PGPLOT operations it needs to.  
#  If this bug gets fixed, then the 'set corigin $properorigin' line below
#  could get uncommented.
         set properorigin [ view2canv $xorigin [ expr $yorigin + $ysize ] ]
         set corigin { 0 0 }
         set corigin $properorigin; set properorigin { 0 0 }

#  Create the canvas item itself.
         $canvas create gwm \
            [ lindex $corigin 0 ] [ lindex $corigin 1 ] \
            -name $name \
            -width [ expr $xsize * $zoomfactor ] \
            -height [ expr $ysize * $zoomfactor ] \
            -tags gwmitem

#  Set the lookup table.
         configure -lutable $lutable

#  Return the distance by which this has to be shifted on the canvas.
         return $properorigin
      }


#-----------------------------------------------------------------------
      public method view2canv { vx vy } {
#-----------------------------------------------------------------------
         set cx [ expr $vx * $zoomfactor ]
         set cy [ expr - $vy * $zoomfactor ]
         return [ list $cx $cy ]
      }


#-----------------------------------------------------------------------
      public method canv2view { cx cy } {
#-----------------------------------------------------------------------
         set vx [ expr $cx / $zoomfactor ]
         set vy [ expr - $cy / $zoomfactor ]
         return [ list $vx $vy ]
      }


#-----------------------------------------------------------------------
      public method unbindall {} {
#-----------------------------------------------------------------------
#  Unset all canvas bindings.  Because of the way that bind works
#  (a binding is to a tag, not an item) we can't just use 'all' for the
#  tabs to remove the bindings (though maybe there is some easier way?).
         catch { unset taghash } 
         foreach item [ $canvas find all ] {
            foreach tag [ $canvas gettags $item ] {
               set taghash($tag) 1
            }
         }
         foreach tag [ array names taghash ] { 
            foreach sequence [ $canvas bind $tag ] {
               $canvas bind $tag $sequence {}
            }
         }
      }


#-----------------------------------------------------------------------
      public method removegwm {} {
#-----------------------------------------------------------------------
         $canvas delete gwmitem
         set gwmname ""
      }


#-----------------------------------------------------------------------
      public method addpoint { vx vy { ipoint "" } } {
#-----------------------------------------------------------------------
#  Add a point to the list of marked points.

#  Use default value for point index if none is specified.
         if { $ipoint == "" } {
            set ipoint [ $itk_component(marknum) next ]
         }

#  Check that we have a valid point index.
         if { $ipoint < 0 } {
            bell 
            return -1
         }

#  If any point with this index exists in the list already, remove it.
         removepoint $ipoint

#  Check that we have not exceeded the maximum number of points allowed.
         if { $maxpoints > 0 && [ llength $points ] >= $maxpoints } {
            bell
            return -1
         }

#  Set the canvas tag to use for this point.
         set tag mark$ipoint

#  Draw the point on the canvas.
         marker $vx $vy [ list $tag marker ] $ipoint

#  Add this point to the list of points that we know about.
         lappend points [ list $ipoint $vx $vy $tag ]
         $itk_component(marknum) use $ipoint

#  Rearrange the elements of the points list to be in index-ascending order
#  (this isn't really necessary, but makes the output list look tidier 
#  and makes some processing easier).
         set points [ lsort -integer -index 0 $points ]

#  Return the index of the point which was added.
         return $ipoint
      }


#-----------------------------------------------------------------------
      public method removepoint { ipoint } {
#-----------------------------------------------------------------------
#  Remove a point from the list of points and erase it from the canvas.

#  If any point in the list has the index ipoint, then remove it from the
#  list and from the canvas.
         set i 0
         foreach p $points {
            if { [ lindex $p 0 ] == $ipoint } {
               set tag [ lindex $p 3 ]
               $canvas delete $tag
               set points [ lreplace $points $i $i ]
            }
            incr i
         }

#  Possibly modify the number of the next marker to be plotted according
#  to whether we've just opened up a suitable gap in the list.
         $itk_component(marknum) unuse $ipoint
      }


#-----------------------------------------------------------------------
      public method clearpoints {} {
#-----------------------------------------------------------------------
#  Removes any extant points from the points list and the canvas.
         if { [ llength $points ] } {
            $canvas delete marker
            set points {}
         }
         $itk_component(marknum) clear
      }


#-----------------------------------------------------------------------
      public method points {} {
#-----------------------------------------------------------------------
#  Generate a list of points in the required format from the private
#  points variable.

         set rp {}
         foreach p $points {
            lappend rp [ list [ lindex $p 0 ] [ lindex $p 1 ] [ lindex $p 2 ] ]
         }
         return $rp
      }


#-----------------------------------------------------------------------
      public method markertype { type colour size } {
#-----------------------------------------------------------------------
#  Set new values for the markconfig and markitem private variables based
#  on the arguments.
         set mitem ""
         set mconfig ""
         if { $type <= 4 } {
            set s [ expr $size / 2 ]
            set mitem "line 0 -$s  0 $s  0 0  -$s 0  $s 0"
            switch $type {
               1 { set mconfig "-width 2" }
               2 { set mconfig "-width 3" }
               3 { set mconfig "-width [ expr $scale / 10 ]" }
               4 { set mconfig "-width [ expr $scale / 6 ]" }
            }
         } elseif { $type <= 8 } {
            set s [ expr $size / 2 ]
            set mitem "line -$s -$s  -$s $s  $s $s  $s -$s  -$s -$s"
            switch { expr $type -4 } {
               1 { set mconfig "-width 2" }
               2 { set mconfig "-width 3" }
               3 { set mconfig "-width [ expr $scale / 10 ]" }
               4 { set mconfig "-width [ expr $scale / 6 ]" }
            }
         }
         lappend mconfig -fill $colour
         if { $mitem == "" } {
            error "Invalid marker specification (programming error)"
            exit
         }

#  If the new values differ from the old ones, reconfigure the items to
#  reflect the new values.
         if { $markitem != "" } {
            if { [ lindex $mitem 0 ] != [ lindex $markitem 0 ] } {
               puts "Different item types - not currently dealt with."
            } elseif { $mitem != $markitem } {
               puts "Different item geometries - not currently dealt with."
            } elseif { $mconfig != $markconfig } {
               eval $canvas itemconfigure marker $mconfig
            }
         }

#  Set instance variables to new values.
         set markitem $mitem
         set markconfig $mconfig
         set markcolour $colour
         set marksize $size
      }


#-----------------------------------------------------------------------
      public method refreshpoints {} {
#-----------------------------------------------------------------------
         foreach p $points {
            marker [ lindex $p 1 ] [ lindex $p 2 ] [ lindex $p 3 ] \
                   [ lindex $p 0 ]
         }
      }


#-----------------------------------------------------------------------
      public method jiggle {} {
#-----------------------------------------------------------------------
#  I don't have the first idea what's going on here, but writing output
#  to the screen seems to prevent the display from showing up squiffy
#  before you've jiggled it round using the scroll bars first.  This
#  has to be a bug, but I don't know what it's in - probably GWM canvas
#  item, the scrolledcanvas iwidget, or the canvas widget itself.
#  I've tried some other things (reading scrollbar position, writing
#  an empty string) but they don't seem to do the trick.  Jeez.
         puts ""
      }


#-----------------------------------------------------------------------
      public method activate {} {
#-----------------------------------------------------------------------
         configure -state "active"
      }


#-----------------------------------------------------------------------
      public method deactivate {} {
#-----------------------------------------------------------------------
         configure -state "inactive"
      }


#-----------------------------------------------------------------------
      public method display {} {
#-----------------------------------------------------------------------
      }


#-----------------------------------------------------------------------
      public method gwmname {} {
#-----------------------------------------------------------------------
         return $gwmname
      }


#-----------------------------------------------------------------------
      public method devname {} {
#-----------------------------------------------------------------------
         return "xw;$gwmname"
      }


#-----------------------------------------------------------------------
      public method canvas {} {
#-----------------------------------------------------------------------
         return $canvas
      }


#-----------------------------------------------------------------------
      public method maxcanvas {} {
#-----------------------------------------------------------------------
         set dim [ expr $zoomfactor * [ max [ lindex $gwmdims 0 ] \
                                            [ lindex $gwmdims 1 ] ] ]
         return [ expr $dim * 1.4 ]
      }


#-----------------------------------------------------------------------
      public method zoominc { factor inc } {
#-----------------------------------------------------------------------
         return [ $itk_component(zoom) zoominc $factor $inc ]
      }


#-----------------------------------------------------------------------
      public method childsite { } {
#-----------------------------------------------------------------------
         return $itk_component(childsite)
      }


########################################################################
#  Public procedures.
########################################################################

#-----------------------------------------------------------------------
      public proc max { num args } {
#-----------------------------------------------------------------------
         foreach x $args {
            if { $x > $num } { set num $x }
         }
         return $num
      }


#-----------------------------------------------------------------------
      public proc min { num args } {
#-----------------------------------------------------------------------
         foreach x $args {
            if { $x < $num } { set num $x }
         }
         return $num
      }


########################################################################
#  Private methods.
########################################################################

#-----------------------------------------------------------------------
      private method marker { vx vy { tags "" } { label "" } } {
#-----------------------------------------------------------------------
         set mitem $markitem
         set mitem [ lindex $markitem 0 ]
         set pos [ view2canv $vx $vy ]
         set cx [ lindex $pos 0 ]
         set cy [ lindex $pos 1 ]
         for { set i 0 } { $i < [ llength $markitem ] } { } {
            lappend mitem [ expr [ lindex $markitem [ incr i ] ] + $cx ]
            lappend mitem [ expr [ lindex $markitem [ incr i ] ] + $cy ]
         }
         set taglist ""
         if { $tags != "" } {
            eval lappend taglist $tags
         }
         set point [ eval $canvas create \
                       $mitem $markconfig -tags \[ list $taglist \] ]
         if { $label != "" && $uselabels } {
            $canvas create \
               text [ expr $cx + $marksize ] $cy \
                  -tags $taglist \
                  -anchor w \
                  -font [ list Helvetica [ expr -2 * $marksize ] ] \
                  -fill $markcolour \
                  -text $label 
         }
         return $point
      }


#-----------------------------------------------------------------------
      private method winch { } {
#-----------------------------------------------------------------------
#  This method is called if the toplevel window receives a configure
#  event, which will happen, for instance, if it is resized.
         configure -geometry [ winfo geometry $itk_interior ]
      }



########################################################################
#  Public variables.
########################################################################

#-----------------------------------------------------------------------
      public variable pixelsize 1 {
#-----------------------------------------------------------------------
         configure -zoomfactor [ expr $zoom / $pixelsize ]
      }


#-----------------------------------------------------------------------
      public variable zoomfactor 1 {
#-----------------------------------------------------------------------
         display
      }


#-----------------------------------------------------------------------
      public variable zoom { 1 } {
#-----------------------------------------------------------------------
         configure -zoomfactor [ expr $zoom / $pixelsize ]
      }


#-----------------------------------------------------------------------
      public variable watchstate "" {
#-----------------------------------------------------------------------
         set watchlevel [expr [info level] - 1]
         upvar #$watchlevel $watchstate wstate
         set wstate $state
      }


#-----------------------------------------------------------------------
      public variable state inactive {
#-----------------------------------------------------------------------
         if { $state == "inactive" || $state == "active" || $state == "done" } {
            if { $watchstate != "" } {
               upvar #$watchlevel $watchstate wstate
               set wstate $state
            }
         } else {
            error "Invalid value \"$state\" for state"
         }
         if { $state == "active" } {
            display
            foreach c $controls {
               $c configure -state normal
            }
         } elseif { $state == "inactive" || $state == "done" } {
            foreach c $controls {
               $c configure -state disabled
            }
         }
      }


#-----------------------------------------------------------------------
      public variable lutable { grey } {
#-----------------------------------------------------------------------
         set table [ lindex $lutable 0 ]
         if { [ llength $lutable ] > 1 } {
            set maptype [ lindex $lutable 1 ]
         } else {
            set maptype "linear"
         }
         set device [ devname ]
         if { $state == "active" } {
            iCCDRunTask lutable \
               "coltab=$table mapping=$maptype device=$device reset" \
               3 itk_component(viewarea)
         }
      }


#-----------------------------------------------------------------------
      public variable geometry "" {
#-----------------------------------------------------------------------
         wm geometry $itk_interior $geometry
      }


#-----------------------------------------------------------------------
      public variable maxpoints 0 {
#-----------------------------------------------------------------------
         $itk_component(marknum) configure -max $maxpoints
      }


#-----------------------------------------------------------------------
      public variable uselabels 1 {
#-----------------------------------------------------------------------
         if { $uselabels } {
            $itk_component(marknum) configure -hidden 0
         } else {
            $itk_component(marknum) configure -hidden 1
         }
      }


########################################################################
#  Private variables.
########################################################################

#  Instance Variables.
      private variable canvas          ;# Name of the canvas widget
      private variable controls        ;# List of control widgets in panel
      private variable gwmdims {0 0}   ;# Dimensions of the GWM item
      private variable gwmorigin {0 0} ;# View coordinate origin of GWM item
      private variable gwmname ""      ;# Name of the GWM item
      private variable markcolour ""   ;# Colour in which to draw a marker
      private variable markconfig ""   ;# Code for configuring a marker
      private variable markitem ""     ;# Code for drawing a marker
      private variable marksize ""     ;# Approx size in pixels of a marker
      private variable points {}       ;# List of points {index x y tags}
      private variable watchlevel 0    ;# Call stack level of calling code

   }


########################################################################
#  Widget resource management
########################################################################

   itk::usual Gwmview {
      keep -background -cursor -foreground -canvasbackground
   }
   option add *Gwmview.canvasBackground black widgetDefault


########################################################################
#  Constructor alias
########################################################################

   proc gwmview { pathname args } {
      uplevel Gwmview $pathname $args
   }

# $Id$
