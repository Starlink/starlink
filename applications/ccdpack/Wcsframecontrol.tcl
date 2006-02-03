   itcl::class Wcsframecontrol {
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
#     ndf = ndf/ndfset object
#        The ndf object or ndfset object with which the widget is 
#        associated.

#  Authors:
#     MBT: Mark Taylor (STARLINK)

#  History:
#     10-NOV-2000 (MBT):
#        Original version.
#     14-MAR-2001 (MBT):
#        Upgraded for use with Sets.

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
         pack $omenu -fill x -expand 1
         pack $itk_component(control) -fill both -expand 1
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
         if { ! [ catch { $ndf validndf } valid ] } { set hasvalid $valid }
         if { ! [ catch { $ndf validndfset } valid ] } { set hasvalid $valid }
         if { $hasvalid } {
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
         if { $value != "" && $hasvalid } {
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
      private variable hasvalid 0        ;# Do we have a valid ndf/ndfset?

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
