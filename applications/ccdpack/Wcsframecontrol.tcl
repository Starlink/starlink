   class Wcsframecontrol {
#+
#  Name:
#     Wcsframecontrol

#  Type of Module:
#     [incr Tk] Mega-Widget

#  Purpose:
#     Control widget for selecting a WCS frame.

#  Description:
#     This control provides an interface from which the user can select
#     a WCS frame from the ones available in an NDF's WCS component.

#  Public Methods:
#

#  Public Variables (Configuration Options):
#     ndf = ndf object
#        The ndf object with which the widget is associated.

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
         itk_component add wcsframe {
            iwidgets::optionmenu $itk_component(control).wcsframe \
               -command [ code $this setvalue ]
         }
         set omenu $itk_component(wcsframe)
         pack $omenu
         pack $itk_component(control)
         configure -balloonstr "World Coordinate System frame"
         eval itk_initialize $args
      }


########################################################################
#  Public variables.
########################################################################

#-----------------------------------------------------------------------
      public variable ndf "" {
#-----------------------------------------------------------------------
         $omenu delete 0 end
         if { ! [ catch { $ndf validndf } ] } {
            foreach domain [ $ndf frameatt domain ] {
               $omenu insert 0 $domain
            }
            configure -value CURRENT
            $omenu configure -state $state
         } else {
            configure -value ""
            $omenu configure -state disabled
         }
      }


#-----------------------------------------------------------------------
      public variable state { normal } {
#-----------------------------------------------------------------------
         if { $state == "normal" } {
            $omenu configure -state normal
         } elseif { $state == "disabled" } {
            $omenu configure -state disabled
         }
      }
 

#-----------------------------------------------------------------------
      public variable value "" {
#-----------------------------------------------------------------------
         if { $value != "" && ! [ catch { $ndf validndf } ] } {
            if { [ $ndf frameatt domain $value ] != $value } {
               set value [ $ndf frameatt domain $value ]
               configure -value $value
            }
            $omenu select $value
         }
      }


########################################################################
#  Private methods.
########################################################################

#-----------------------------------------------------------------------
      private method setvalue { } {
#-----------------------------------------------------------------------
         set val [ $omenu get ]
         configure -value $val
      }


########################################################################
#  Private variables.
########################################################################

      private variable omenu             ;# Path name of the optionmenu widget
      private variable omenubut          ;# Path name of menubutton component

   }


########################################################################
#  Widget resource management
########################################################################

   itk::usual Wcsframecontrol {
      keep -background -cursor -foreground 
   }


########################################################################
#  Constructor alias
########################################################################

   proc wcsframecontrol { pathname args } {
      uplevel Wcsframecontrol $pathname $args
   }
   

# $Id$
