   class Stylecontrol {
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
            button $itk_component(control).style \
               -text "Style" \
               -command [ code $this dodialog ]
         }
         pack $itk_component(style)
         pack $itk_component(control)

         itk_component add styledialog {
            iwidgets::dialog $itk_interior.styledialog \
               -modality application
         } {
            usual 
            ignore -modality
         }
         
         $itk_component(styledialog) delete Help
         $itk_component(styledialog) delete Apply
         itk_component add checkbox {
            iwidgets::checkbox [ $itk_component(styledialog) childsite ].cb
         }
         lappend atts { drawaxes "Draw axes" "drawaxes=0" "drawaxes=1" }
         lappend atts { grid "Draw grid" "grid=0" "grid=1" }
         lappend atts { numlab "Label axes" "numlab=0" "numlab=1" }
         set i 0
         foreach att $atts {
            set name [ lindex $att 0 ]
            set text [ lindex $att 1 ]
            set no [ lindex $att 2 ]
            set yes [ lindex $att 3 ]
            $itk_component(checkbox) add $name \
                -text $text \
                -command update
            set elements($i,0) $no
            set elements($i,1) $yes
            incr i
         }
         pack $itk_component(checkbox)
         eval itk_initialize $args
         configure -value $value
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
      public variable value {drawaxes=1,drawgrid=0,numlab=0} {
#-----------------------------------------------------------------------

#  If we have been given a value for the style, modify the current status
#  of the checkbuttons to reflect this.
         set els [ split $value "," ]
         set last [ $itk_component(checkbox) index end ]
         for { set i 0 } { $i <= $last } { incr i } {
            if { [ lsearch -exact $els $elements($i,0) ] > -1 } {
               $itk_component(checkbox) deselect $i
            } elseif { [ lsearch -exact $els $elements($i,1) ] > -1 } {
               $itk_component(checkbox) select $i
            }
         }
      }



########################################################################
#  Private methods.
########################################################################

#-----------------------------------------------------------------------
      private method dodialog {} {
#-----------------------------------------------------------------------
         if { [ $itk_component(styledialog) activate ] } {
            set last [ $itk_component(checkbox) index end ]
            set value ""
            for { set i 0 } { $i <= $last } { incr i } {
               if { $i > 0 } {
                  append value ","
               }
               append value $elements($i,[ $itk_component(checkbox) get $i ])
            }
         }
         configure -value $value
      }


########################################################################
#  Private variables.
########################################################################

      private variable elements     ;# Style elements

   }


########################################################################
#  Widget resource management
########################################################################

   itk::usual Stylecontrol {
      keep -background -cursor -foreground
   }


########################################################################
#  Constructor alias
########################################################################

   proc stylecontrol { pathname args } {
      uplevel Stylecontrol $pathname $args
   }


# $Id$
