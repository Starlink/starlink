   class Checkcontrol {
#+
#  Name:
#     Checkcontrol

#  Type of Module:
#     [incr Tk] Mega-Widget

#  Purpose:
#     Viewer control containing a checkbutton.

#  Description:
#     This class provides a very simple control which just contains a 
#     checkbutton.  It inherits from the Control class, and so inherits
#     any appropriate behaviour from there.

#  Public Methods:
#
#     Checkcontrol inherits all the public methods of the Control widget.

#  Public Variables (Configuration Options):
#
#     label
#        Text with which to label the checkbutton.
#
#     value
#        A boolean giving the selected status of the checkbutton.
#
#     Checkcontrol also inherits all the appropriate public variables
#     of the Control widget.

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
         itk_component add checkbutton {
            checkbutton $itk_component(control).cb \
               -indicatoron 1 \
               -pady 0 \
               -variable [ scope checkval ] \
               -command [ code "$this setvalue" ]
         }
         pack $itk_component(checkbutton) -side left -fill both -expand 1
         pack $itk_component(control) -side left -fill both -expand 1
         configure -balloonstr {Toggle me}
         eval itk_initialize $args
      }


########################################################################
#  Public methods.
########################################################################

#-----------------------------------------------------------------------
      private method setvalue {} {
#-----------------------------------------------------------------------
         configure -value $checkval
      }


########################################################################
#  Public variables.
########################################################################

#-----------------------------------------------------------------------
      public variable value 0 {
#-----------------------------------------------------------------------
         set checkval $value
      }


#-----------------------------------------------------------------------
      public variable label {} {
#-----------------------------------------------------------------------
         $itk_component(checkbutton) configure -text $label
      }


#-----------------------------------------------------------------------
      public variable state { normal } {
#-----------------------------------------------------------------------
         if { $state == "normal" } {
            $itk_component(checkbutton) configure -state normal
         } elseif { $state == "disabled" } {
            $itk_component(checkbutton) configure -state disabled
         }
      }



########################################################################
#  Private variables.                                                  #
########################################################################

      private variable checkval    ;# Value of the checkbutton variable

  }


########################################################################
#  Widget resource management
########################################################################

   itk::usual Checkcontrol {
      keep -background -cursor -foreground
   }
   option add *Checkcontrol.selectColor #b03060 widgetDefault


########################################################################
#  Constructor alias
########################################################################

   proc checkcontrol { pathname args } {
      uplevel Checkcontrol $pathname $args
   }
   

# $Id$
