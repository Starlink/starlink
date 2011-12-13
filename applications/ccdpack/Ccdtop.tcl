class Ccdtop {
#+
#  Name:
#     Ccdtop

#  Purpose:
#     Provide a generic toplevel window for CCDPACK applications.

#  Language:
#     TCL

#  Type of Module:
#     [incr Tk] Mega-Widget

#  Description:
#     This class provides a mega-widget to be used as a general-purpose
#     toplevel.  It provides a control panel, a mechanism for displaying
#     help text, a mechanism for watching its status, and a childsite
#     into which client widget can put their own content.

#  Public Methods:
#     addcontrol widget grpname
#        Adds a control widget to the control panel.  The widget
#        argument should be the pathname of a widget which has
#        as an ancestor the path given by the 'panel' method of this
#        class, and which has a '-state' configuration variable with
#        valid 'normal' and 'disabled' values.  Such widgets should be
#        children of the window returned by the [panel] method.
#           - widget     -- Pathname of a control-type widget
#           - grpname    -- Unique tag identifying this group
#
#     addgroup grpname heading
#        Adds a grouping frame to the control panel.  This can be used
#        for grouping controls; the grpname specified here, which should
#        be unique, must be referred to when controls are added
#        with the addcontrol method.  The return value of this method
#        is the name of the group widget, which may be used to
#        modify the packing.
#           - grpname    -- Tag to identify the group
#           - heading    -- Short text string to label the group
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
#     groupwin grpname
#        Returns the name of the window given by the grpname argument.
#        This is th control group window as set up by addgrp.
#           - grpname    -- Tag to identify the group (see addgroup)
#
#     helptext text
#        Specifies a text string which can be presented to the user to
#        tell him what he should be doing.
#
#     panel
#        Returns the path of a frame into which controls should be placed.
#
#     waitpop
#        Must occur in a matching pair with the waitpush method.
#
#     waitpush msg
#        If the widget is not already in a "waiting" state, put it in
#        such a state, and present the msg argument to the user by way
#        of explanation.  If the widget is already in waiting state,
#        no action is taken.  A waitpush should be executed any time
#        that the widget is likely to be unresponsive to user activity
#        for a significant amount of time (especially if attempted user
#        activity could cause problems).  Between a waitpush and its
#        matching waitpop, a window will be displayed to the user which
#        will inform the user what's going on, and also grab all events
#        in the application, so that no user interaction will be possible.
#        Additionally, any window Configure events will not have their
#        usual effect of calling the winch method; but after the wait
#        state has finished, the winch method will be called if one
#        has been ignored in the mean time.
#
#        A matching waitpop must be executed for every waitpush after
#        the action has completed.
#
#     Ccdtop also inherits all the public methods of itk::Toplevel.
#
#     Public variables (configuration options):
#
#     geometry = string
#        This gives the geometry in the normal X11 format.
#
#     status = string
#        A value which gives the status of the object.  It may have the
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
#        using the 'watchstatus' public variable.
#
#     watchstatus = string
#        This gives a name of a variable in the caller's context which
#        will be kept up to date with the status of this object, i.e.
#        it will have the same value as the object's $status public
#        variable.  It is useful to configure this so that the variable
#        can be traced to watch for changes in status.
#
#     Ccdtop also inherits all the public variables of itk::Toplevel

#  Public Procedures:
#     bestvisual window visual ...
#        Returns a visual (in the form {visualtype depth}) which represents
#        the best choice among those indicated of the ones available in
#        the window specified.  There may be any number of visual arguments,
#        of the form {visualtype mindepth}; the first which can be
#        satisfied is returned.
#
#     max num ...
#        Gives the maximum of all the (numeric) arguments supplied.
#        Just here because it's useful.
#
#     mim num ...
#        Gives the minimum of all the (numeric) arguments supplied.
#        Just here because it's useful.

#  Copyright:
#     Copyright (C) 2006 Particle Physics & Astronomy Research Council.
#     All Rights Reserved.

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
#     {original_author_entry}

#  History:
#     {enter_changes_here}

#  Bugs:
#     {note_any_bugs_here}

#-

#  Inheritance.
      inherit itk::Toplevel


########################################################################
#  Constructor.
########################################################################
      constructor { args } {

#  Hide the window until its status becomes active.
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
      public method addcontrol { widget grpname } {
#-----------------------------------------------------------------------
         lappend controls $widget
         pack $widget \
            -side left \
            -padx 2 -pady 2 -fill both -expand 1 \
            -in [ $groups($grpname) childsite ]
      }


#-----------------------------------------------------------------------
      public method addgroup { grpname heading } {
#-----------------------------------------------------------------------
         incr ngroup
         itk_component add group$ngroup {
            iwidgets::labeledframe [ panel ].group$ngroup \
               -labeltext $heading -labelpos n -ipadx 2 -ipady 2
         }
         pack $itk_component(group$ngroup) -side left -fill y
         set groups($grpname) $itk_component(group$ngroup)
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
         if { [ winfo visual $parent ] == "pseudocolor" } {
            set visual [ bestvisual {truecolor 12} {staticgray 8} \
                                    {staticcolor 8} {pseudocolor 8} ]
            set pseudo [ regexp pseudo $visual ]
         } else {
            set visual $parent
            set pseudo 0
         }

#  If it's not going to be PseudoColor, we just need to create a frame.
         if { ! $pseudo } {
            itk_component add $component {
               frame $path -visual $visual
            }

#  It will be PseudoColor.  Create a frame with a new colormap, and
#  then create some windows in it which will force allocation of colours
#  we're going to need in the first few slots.
         } else {
            itk_component add $component {
               frame $path -visual $visual -colormap new
            }
            iwidgets::pushbutton $path.but -text T
            # button $path.but -text T
         }

#  If required, notify the window manager that this colormap should be the
#  one used for the toplevel window.
         if { $install && $visual != $parent } {
            wm colormapwindows $itk_interior $path
         }
      }


#-----------------------------------------------------------------------
      public method geomset { } {
#-----------------------------------------------------------------------

#  Only proceed if we have not done this already.
         if { [ wm minsize $itk_interior ] == {1 1} } {

#  Arrange for geometry constraints to be met.
            set wnow [ winfo width $itk_component(panel) ]
            set hnow [ winfo height $itk_interior ]
            wm minsize $itk_interior [ max 2 $wnow ] \
                                     [ max 2 [ min $hnow $wnow ] ]

#  Set binding to handle window resize events.
            bind $itk_interior <Configure> [ code $this winch ]
         }
      }


#-----------------------------------------------------------------------
      public method groupwin { grpname } {
#-----------------------------------------------------------------------
         return $groups($grpname)
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


#-----------------------------------------------------------------------
      public method waitpush { msg } {
#-----------------------------------------------------------------------
         if { [ incr waitlevel ] == 1 } {
            set waitwin [ waiter $itk_interior.waiter \
                                    -text "$msg ..." \
                                    -animate 0 ]
            set configact [ bind $itk_interior <Configure> ]
            if { $configact != "" } {
               set hadconfig 0
               bind $itk_interior <Configure> [ code set hadconfig 1 ]
            }
         } else {
            if { [winfo exists $waitwin] } {
               $waitwin configure -text "$msg..."
            }
         }
      }


#-----------------------------------------------------------------------
      public method waitpop { } {
#-----------------------------------------------------------------------
         if { [ incr waitlevel -1 ] == 0 } {
            update idletasks
            if { [winfo exists $waitwin] } {
               catch { destroy $waitwin }
            }
            if { $configact != "" } {
               bind $itk_interior <Configure> $configact
               if { $hadconfig } {
                  event generate $itk_interior <Configure>
                  set hadconfig 0
               }
            }
         }
      }


########################################################################
#  Public procedures.
########################################################################

#-----------------------------------------------------------------------
      public proc bestvisual { window args } {
#-----------------------------------------------------------------------
         foreach vgot [ winfo visualsavailable $window ] {
            set vis [ lindex $vgot 0 ]
            set depth [ lindex $vgot 1 ]
            if { [ array names visual $vis ] == "" || \
                 $visual($vis) < $depth } {
               set visual($vis) $depth
            }
         }
         foreach vwant $args {
            set vis [ lindex $vwant 0 ]
            set depth [ lindex $vwant 1 ]
            if { [ array names visual $vis ] != "" && \
                 $visual($vis) >= $depth } {
               return [ list $vis $visual($vis) ]
            }
         }
         return [ lindex [ winfo visualsavailable $window ] 0 ]
      }

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
         set newgeo [winfo geometry $itk_interior]
         if { $itk_option(-geometry) != $newgeo } {
            configure -geometry $newgeo
         }
      }



########################################################################
#  Public variables.
########################################################################

#-----------------------------------------------------------------------
      public variable watchstatus "" {
#-----------------------------------------------------------------------
         set watchlevel [expr [info level] - 1]
         upvar #$watchlevel $watchstatus wstatus
         set wstatus $status
      }


#-----------------------------------------------------------------------
      public variable status inactive {
#-----------------------------------------------------------------------
         if { $status == "inactive" || $status == "active" || $status == "done" } {
            if { $watchstatus != "" } {
               upvar #$watchlevel $watchstatus wstatus
               set wstatus $status
            }
         } else {
            error "Invalid value \"$status\" for status"
         }
         if { $status == "active" } {
            wm deiconify $itk_interior
            foreach c $controls {
               $c configure -state normal
            }
         } elseif { $status == "inactive" || $status == "done" } {
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
      private variable configact ""    ;# Binding code for Configure event
      private variable controls        ;# List of control widgets in panel
      private variable groups          ;# List of control groups in panel
      private variable hadconfig 0     ;# Have we missed a Configure event?
      private variable ngroup 0        ;# Number of control groups in panel
      private variable waitlevel 0     ;# Number of active waitpush calls
      private variable waitwin         ;# Name of the waiter window
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
