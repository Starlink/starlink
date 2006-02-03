   itcl::class Stylecontrol {
#+
#  Name:
#     Stylecontrol

#  Type of Module:
#     [incr Tk] Mega-Widget

#  Purpose:
#     Control widget for modifying image display style.

#  Description:
#     This control provides an interface using which the user may
#     modify the options used for displaying the image in a Gwmview
#     widget.  It returns a string which can be passed as a list of
#     ADAM parameter specifiers for the KAPPA DISPLAY task.

#  Public Methods:
#
#     clear
#        Resets all marker indices to the unused state.
#
#     next
#        Returns the index of the next unused marker.  This will normally
#        be the one displayed in the widget.  If there are no unused
#        markers left, it will return -1.
#
#     unuse index
#        Set the given marker index to the unused state.
#
#     use index
#        Set the given marker index to the used state.
#

#  Public Variables (Configuration Options):
#
#     max
#        The maximum index that the value variable may take.  If set to
#        zero, there is no upper limit.
#
#     value
#        The value of the ADAM 'style' parameter to be passed to the 
#        DISPLAY task.
#
#     Marknumcontrol also inherits all the public variables of the Control
#     widget.

#-

#  Inheritance:
      inherit Control


########################################################################
#  Constructor.
########################################################################
      constructor { args } {
         itk_component add control {
            frame [ childsite ].control
         }
         itk_component add style {
            menubutton $itk_component(control).style \
               -relief raised \
               -menu $itk_component(control).style.menu \
               -text "Grid"
         }
         pack $itk_component(style) -fill both -expand 1
         set menubutton $itk_component(style)
         itk_component add menu {
            menu $menubutton.menu
         }
         lappend atts { grid "Draw grid" "grid=0" "grid=1" }
         lappend atts \
            { numlab "Label axes" "numlab=0,majticklen=0,minticklen=0" \
              "numlab=1,labelling=interior,majticklen=0.015,minticklen=0.007" }
         set i 0
         foreach att $atts {
            set name [ lindex $att 0 ]
            set text [ lindex $att 1 ]
            set no [ lindex $att 2 ]
            set yes [ lindex $att 3 ]
            set elements($i,0) $no
            set elements($i,1) $yes
            $itk_component(menu) add checkbutton \
               -label $text \
               -variable [ scope elvar($i) ] \
               -onvalue $yes \
               -offvalue $no \
               -command [ code $this setval ]
            incr i
         }
         pack $itk_component(control) -fill both -expand 1
         configure -balloonstr "Axis plotting style"
         eval itk_initialize $args
         setval
      }


########################################################################
#  Public methods.
########################################################################


########################################################################
#  Public variables.
########################################################################

#-----------------------------------------------------------------------
      public variable state { normal } {
#-----------------------------------------------------------------------
         if { $state == "normal" } {
            $itk_component(style) configure -state normal
         } elseif { $state == "disabled" } {
            $itk_component(style) configure -state disabled
         }
      }


#-----------------------------------------------------------------------
      public variable value {drawgrid=0,numlab=0} {
#-----------------------------------------------------------------------

#  If we have been given a value for the style, modify the current status
#  of the checkbuttons to reflect this.
         set last [ $itk_component(menu) index end ]
         foreach el [ split $value "," ] {
            for { set i 1 } { $i <= $last } { incr i } {
               if { $el == [ $itk_component(menu) entrycget $i -onvalue ] || \
                    $el == [ $itk_component(menu) entrycget $i -offvalue ] } {
                  set elvar([ expr $i - 1 ]) $el
               }
            }
         }
      }



########################################################################
#  Private methods.
########################################################################

#-----------------------------------------------------------------------
      private method setval {} {
#-----------------------------------------------------------------------
         set val ""
         for { set i 0 } { $i < [ array size elvar ] } { incr i } {
            append val "$elvar($i),"
         }
         regsub {,$} $val {} val
         configure -value $val
      }



########################################################################
#  Private variables.
########################################################################

      private variable elements     ;# Style element configuration
      private variable elvar        ;# Values of style elements

   }


########################################################################
#  Widget resource management
########################################################################

   itk::usual Stylecontrol {
      keep -background -cursor -foreground -selectcolor
   }
   option add *Stylecontrol.selectColor #b03060 widgetDefault


########################################################################
#  Constructor alias
########################################################################

   proc stylecontrol { pathname args } {
      uplevel Stylecontrol $pathname $args
   }


# $Id$
