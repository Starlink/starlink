   itcl_class Ccd_gwm {

#+
#  Name:
#     Ccd_gwm

#  Type of Module:
#     [incr Tcl] class

#  Purpose:
#     Defines a scrollable, resizable gwm based widget.

#  Description:
#     This routine defines a widget class based around the gwm canvas
#     widget. It creates a canvas with scrollbars that may be
#     displayed into by graphics applications. If a graphics context
#     command is registered then it can be called to restore any
#     graphics if the canvas is resized, either interactively (via the
#     reconfiguration event) or if the configuration cantrolling the
#     canvas size are changed.

#  Invocations:
#
#        Ccd_gwm window [-option value]...
#
#     This command create an instance of a "scrollable gwm widget" and
#     returns a command "window" for manipulating it via the methods and
#     configuration options described below. Configuration options may
#     be appended to the command.
#
#        window configure -configuration_options value
#
#     Applies any of the configuration options (after the widget
#     instance has been created).
#
#        window method arguments
#
#     Performs the given method on this widget.

#  Configuration options:
#
#        -gwmname name
#
#     Defines the GWM widget name (as recognised by GNS). Defaults to 
#     xwindows.
#
#        -redraw boolean
#
#     Whether or not to resize the Gwm widget when the canvas size changes.
#
#        -drawcommand command
#
#      A command to invoke when the widget is resized. This should restore
#      the graphic context (i.e. re-draw the image to the new size and 
#      redraw any graphics).
#
#        -scrollregion bounds
#
#      Defines the bounds of the scrollable part of the widget. This is
#      the size of the canvas.
#
#        -width width
#    
#      Define the viewable width of the canvas (the bit you see).
#
#        -height height
#    
#      Define the viewable height of the canvas.
#
#        -tags tag
#
#      The canvas tag for the Gwm widget, defaults to Gwm.
#
#         -scrollbarplaces (right|left) (top|bottom)
#
#      Where to put the scrollbars.
#
#         -colours number
#
#      Number of colours to attempt for the Gwm widget. Defaults to 64.
#
#         -mincolours number
#
#      Minimum number of colours that are required. KAPPA lutcol 
#      requires 34 so this is the default value.
#      

#  Methods:
#     constructor [-option value]...
#        This method is invoked automatically by the class command and
#	 creates the "class " widget with a default configuration,
#	 except when overridden by command line options.
#     configure [-option value]...
#        Activates the configuration options. If no configuration value
#	 is given then the current value of any known option is returned
#	 in a form similar (but not identical to) the Tk widget command.
#     method do args
#        This method provides access to the canvas. The args are those
#        you would use in a command to control the canvas.
#     method zoom scale 
#        Increases the size of the Gwm widget by the factor scale. This 
#        causes the widget to be destroyed and re-created at the new size.
#        Consequently if will be blank unless a graphic context 
#        restoring procedure has been registered.
#     method sethelp docname label
#        Sets the help associated with the widget.
#     internal methods
#        _scrollregion, _reconfigure _autoscan and _cancelrepeat, should
#        not be used outside of the class definitions.
#        description

#  Inheritance:
#     This class inherits "Ccd_base" and its methods and configuration
#     options, which are not directly occluded by those specified here.

#  Authors:
#     PDRAPER: Peter Draper (STARLINK - Durham University)
#     {enter_new_authors_here}

#  History:
#     26-SEP-1995 (PDRAPER):
#     	 Original version.
#     {enter_changes_here}

#-

#  Inheritances:
      inherit Ccd_base

#.

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#  Construction creates a instance of the class and configures it with
#  the default and command-line options.
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      constructor { config } {

#  Create a frame widget. This must have the same name as the class
#  command.
         Ccd_base::constructor

#  Create the canvas widget to hold the gwm widget.
         canvas $oldthis.canvas -takefocus true -borderwidth 0
         set widgetnames($oldthis:canvas) $oldthis.canvas

#  Create the gwm widget. This needs to be the same size as the canvas.
         if { [catch {$oldthis.canvas create gwm 0 0 -tags $tags \
	                 -gwmname $gwmname \
                         -colours $colours -mincolours $mincolours}] } {

#  Failed to create Gwm widget.
	       destroy $oldthis.canvas
               $this delete
	       return
         }

#  Default height and width of the canvas are the same as the gwm widget.
         configure -width $width
         configure -height $height

#  Configure scrollbars.
         configure -scrollbarplaces $scrollbarplaces
         _scrollregion {}

#  Add bindings to control scroll & drag.
         set state(afterId) {}
         ::bind $oldthis.canvas <ButtonRelease-1> "$oldthis _cancelrepeat"
         ::bind $oldthis.canvas <B1-Leave> "$oldthis _autoscan"
         ::bind $oldthis.canvas <B1-Enter> "$oldthis _cancelrepeat"
         ::bind $oldthis.canvas <2> "$oldthis.canvas scan mark %x %y"
         ::bind $oldthis.canvas <B2-Motion> "$oldthis.canvas scan dragto %x %y"

#  Binding to catch and update from reconfiguration.
         ::bind $oldthis.canvas <Configure> "$oldthis _reconfigure %w %h"

      }

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#  Methods.
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#  Access to canvas methods.
      method do { args } {
         if $exists {
            eval $oldthis.canvas $args
         }
      }

#  Zoom method. This reconfigures the actual width and height (not the viewed
#  width and height). Assumes a redraw command has been registered.
      method zoom scale {
         set size [$oldthis.canvas cget -scrollregion]
         set x [expr [lindex $size 2] - [lindex $size 0]]
         set x [expr $x * $scale]
         set y [expr [lindex $size 3] - [lindex $size 1]]
         set y [expr $y * $scale]
         set redrawstate $redraw
         set redraw 1
         _reconfigure $x $y
         set redraw $redrawstate
      }

#  Set the help to show for the whole canvas.
      method sethelp { docname label } { 
         if $exists {
            Ccd_base::sethelp $oldthis $docname $label
         }
      }

#  Set the scrollregion of canvas to used width and height. If the
#  displayed width and height of the canvas haven't been set then set
#  them to the used region so that all is displayed. Note scrollregion
#  is the actual size of the canvas.
      method _scrollregion {bounds} {
         if $exists {
            if { $bounds == "" } {
               update idletasks
               set bounds [eval $oldthis.canvas bbox $tags]
               if { $width == "" } {
                  set width [expr [lindex $bounds 2] -[lindex $bounds 0]]
                  incr width
                  configure -width $width
               }
               if { $height == "" } {
                  set height [expr [lindex $bounds 3] -[lindex $bounds 1]]
                  incr height
                  configure -height $height
               }
            }
            $oldthis.canvas configure -scrollregion "$bounds"
         }
      }

#  Control reconfiguration of the canvas if its size changes. If the
#  size is changed and we have a graphic context commmand then active it.
      method _reconfigure { newwidth newheight } {
         if { $drawcommand != "" && $redraw } {

#  Destroy the gwm item.
            $oldthis.canvas delete Gwm

#  Resize the canvas.
            _scrollregion "0 0 $newwidth $newheight"

#  Recreate the gwm widget.
            $oldthis.canvas create gwm 0 0 -width $newwidth \
               -height $newheight -tags $tags -gwmname $gwmname

#  and redraw it.
            eval $drawcommand
         } else {
            set width $newwidth
            set height $newheight
         }
      }

#  Control the autoscan when dragged outside the window.
      method _autoscan {} {
         set xlow [winfo rootx $oldthis.canvas]
         set ylow [winfo rooty $oldthis.canvas]
         set xhigh [expr $xlow + $width]
         set yhigh [expr $ylow + $height]
         set y [winfo pointery $oldthis.canvas]
         set x [winfo pointerx $oldthis.canvas]
         set ok 0
         if {$y >= $yhigh} {
            $oldthis.canvas yview scroll 1 units
            set ok 1
         } elseif {$y < $ylow} {
            $oldthis.canvas yview scroll -1 units
            set ok 1
         }
         if {$x >= $xhigh} {
            $oldthis.canvas xview scroll 2 units
            set ok 1
         } elseif {$x < $xlow} {
            $oldthis.canvas xview scroll -2 units
            set ok 1
         }
         if { $ok } {
            set state(afterId) [after 50 $oldthis _autoscan]
         }
      }
      
#  Cancel auto repeat.
      method _cancelrepeat {} {
         after cancel $state(afterId)
         set state(afterId) {}
      }

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#  Configuration options:
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#  Name of gwm widget (xwindows, xwindows2 etc.)
      public gwmname xwindows {
      }

#  Control whether redraw command is attempted.
      public redraw 0 {}

#  Set the draw command.
      public drawcommand "" {}

#  Size of the scrollregion. Note this does not change the size of the Gwm
#  widget and is really just a way of getting at the true size.
      public scrollregion "" {
         _scrollregion $scrollregion
      }

#  Width and height of the displayed part of the canvas. Note its
#  actual size is set by the scrollregion configuration option. The default
#  display size is the same as that of the GWM widget.
      public width {} {
         if $exists {
            if { $width != "" } {
               $oldthis.canvas configure -width $width
            }
            if { $width == "" } {
               set bbox [$oldthis.canvas bbox $tags]
               configure -width [lindex $bbox 2]
            }
         }
      }
      public height {} {
         if $exists {
            if { $height != "" } {
               $oldthis.canvas configure -height $height
            } else {
               set bbox [$oldthis.canvas bbox $tags]
               configure -height [lindex $bbox 3]
            }
         }
      }

#  Tags for the Gwm widget.
      public tags Gwm {}

#  Create the scrollbars and put into the correct position.
      public scrollbarplaces { right bottom } {
         foreach side $scrollbarplaces {
            if { ! [ regexp (left|right|top|bottom) $side ] } {
               error "Unknown scrollbar placement \"$side\", should be top bottom left or right"
            }
         }

#  Only proceed if the object exists (this means that constructor has
#  been invoked).
         if $exists {


#  Unpack the canvas widet, as needs to be packed last.
            pack forget $oldthis.canvas

#  Delete all existing scrollbars and padding frames.
            if { [ winfo exists $oldthis.pad.hscroll ] } {
               destroy $oldthis.pad.hscroll
               destroy $oldthis.pad.bit
               destroy $oldthis.pad
               $oldthis.canvas configure -xscrollcommand {}
            }
            if { [ winfo exists $oldthis.vscroll ] } {
               destroy $oldthis.vscroll
               $oldthis.canvas configure -yscrollcommand {}
            }
            set vert [lsearch -regexp $scrollbarplaces (right|left)]
            set hori [lsearch -regexp $scrollbarplaces (top|bottom)]

#  Vertical scrollbar is just created.
            if { $vert != -1 } {
               scrollbar $oldthis.vscroll \
                  -command "$oldthis.canvas yview" -orient vertical
               set widgetnames($oldthis:vscroll) $oldthis.vscroll
               $oldthis.canvas configure -yscrollcommand "$oldthis.vscroll set"
            }

#  Horizontal scrollbar requires packing frames for indentation at corners.
            if { $hori != -1 } {
               frame $oldthis.pad

#  Get width of vertical scrollbar. Make corner frame same width.
               if { $vert != -1 } {
                  set padwidth [$oldthis.vscroll cget -width]
                  frame $oldthis.pad.bit -width $padwidth
                  set padside [lindex $scrollbarplaces $vert]
               } else {
                  frame $oldthis.pad.bit
                  set padside right
               }

               scrollbar $oldthis.pad.hscroll \
                  -command "$oldthis.canvas xview" -orient horizontal
               set widgetnames($oldthis:hscroll) $oldthis.pad.hscroll
               $oldthis.canvas configure -xscrollcommand "$oldthis.pad.hscroll set"
               set side [lindex $scrollbarplaces $hori]
            }

#  Now do packing. Place horizontal first to fill extent and get
#  padding frames to take up extra.
            if { $hori != -1 } {
               pack $oldthis.pad -fill x -side $side
               pack $oldthis.pad.bit -side $padside
               pack $oldthis.pad.hscroll -fill x -side $side
            }
            if { $vert != -1 } {
               pack $oldthis.vscroll -side [lindex $scrollbarplaces $vert] -fill y
            }
            pack $oldthis.canvas  -expand true -fill both
         }
      }


#  Number of colours. Changing this has no effect until a resize.
      public colours 64 { }
      
#  Minimum number of colours. KAPPA needs 34!
      public mincolours 34 { }

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#  Common and protected variables.  Common are visible to all instances
#  of this class, protected to just this instance (both are available
#  anywhere in the scope of this class and in derived classes).
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#  State of the autoscan.
   protected state


#  End of class defintion.
   }
# $Id$
