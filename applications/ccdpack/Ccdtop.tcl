class Ccdtop {
#+
#  Name:
#     Ccdtop

#  Type of module:
#     [incr Tk] Mega-Widget

#  Purpose:
#     Provide a generic toplevel window for CCDPACK applications.

#  Description:
#     This class provides a mega-widget to be used as a general-purpose
#     toplevel.  It provides a control panel, a mechanism for displaying
#     help text, a mechanism for watching its status, and a childsite 
#     into which client widget can put their own content.

#  Public methods:
#
#     addcontrol control
#        Adds a control widget to the control panel.  The control
#        argument should be the pathname of a widget which has 
#        as an ancestor the path given by the 'panel' method of this
#        class, and which has a '-state' configuration variable with
#        valid 'normal' and 'disabled' values.
# 
#     childsite
#        Returns the path of a frame into which windows can be placed.
#
#     container component parent ?install?
#        This method creates a frame capable in terms of colormaps of 
#        holding a GWM widget.
#           - component  -- Itk component name of the container frame
#           - parent     -- Pathname of the parent window
#           - install    -- If true, the wm will use the new colormap for
#                           the widget when it has colormap focus
#     geomset
#        This method should be called when the initial contents and
#        geometry of the widget have been set up.  The window will be
#        revealed and its minimum dimensions will be set.
#
#     helptext text
#        Specifies a text string which can be presented to the user to
#        tell him what he should be doing.
#
#     panel
#        Returns the path of a frame into which controls should be placed.
#
#     Ccdtop also inherits all the public methods of itk::Toplevel.
 
#  Public variables (configuration options):
#
#     geometry = string
#        This gives the geometry in the normal X11 format.
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
#     Ccdtop also inherits all the public variables of itk::Toplevel
 
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

#-

#  Inheritance.
      inherit itk::Toplevel


########################################################################
#  Constructor.
########################################################################
      constructor { args } {

#  Hide the window until its state becomes active.
         wm withdraw $itk_interior

#  Consider whether we need to do something funny with the colormap here.
         if { [ lindex $args 0 ] == "-colormap" } {
            set args [ lreplace $args 0 0 ]
            container ccdtopinterior $itk_interior 1
            set interior $itk_component(ccdtopinterior)
            pack $interior
         } else {
            set interior $itk_interior
         }

#  Add the control panel.
         itk_component add panel {
            frame $interior.buttons
         }

#  Add childsite frame.
         itk_component add ccdtopchildsite {
            frame $interior.ccdtopchildsite
         }

#  Pack widgets into the hull.
         pack $itk_component(panel) -side bottom -fill none -expand 0
         pack $itk_component(ccdtopchildsite) -side bottom -fill both -expand 1

#  Do requested configuration.
         eval itk_initialize $args
      }


########################################################################
#  Public methods.
########################################################################

#-----------------------------------------------------------------------
      public method addcontrol { widget } {
#-----------------------------------------------------------------------
         lappend controls $widget
         pack $widget -side left
      }


#-----------------------------------------------------------------------
      public method childsite { } {
#-----------------------------------------------------------------------
         return $itk_component(ccdtopchildsite)
      }


#-----------------------------------------------------------------------
      public method container { component parent { install 0 } } {
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
      public method geomset { } {
#-----------------------------------------------------------------------

#  Arrange for geometry constraints to be met.
         update idletasks
         set wnow [ winfo width $itk_component(panel) ]
         set hnow [ winfo height $itk_interior ]
         wm minsize $itk_interior $wnow [ min $hnow $wnow ]

#  Set binding to handle window resize events.
         bind $itk_interior <Configure> [ code $this winch ]
      }


#-----------------------------------------------------------------------
      public method helptext { text } {
#-----------------------------------------------------------------------
      }


#-----------------------------------------------------------------------
      public method panel { } {
#-----------------------------------------------------------------------
         return $itk_component(panel)
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
            wm deiconify $itk_interior
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
      public variable geometry "" {
#-----------------------------------------------------------------------
         wm geometry $itk_interior $geometry
      }


########################################################################
#  Private variables.
########################################################################

#  Instance Variables.
      private variable controls        ;# List of control widgets in panel
      private variable watchlevel 0    ;# Call stack level of calling code

   }


########################################################################
#  Widget resource management
########################################################################

   itk::usual Ccdtop {
      keep -background -cursor -foreground
   }


########################################################################
#  Constructor alias
########################################################################

   proc ccdtop { pathname args } {
      uplevel Ccdtop $pathname $args
   }

# $Id$
