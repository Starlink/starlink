   class Percentilecontrol {
#+
#  Name:
#     Percentilecontrol

#  Type of Module:
#     [incr Tk] Mega-Widget

#  Purpose:
#     Control widget for selecting a pair of percentiles.

#  Description:
#     This control provides an interface from which the user can select
#     a pair of percentile values.

#  Public Methods:
#

#  Public Variables (Configuration Options):
#
#     allowcustom
#        If this variable has a non-zero value, there will be a mechanism
#        for the user to select a pair of values not in the choices list.
#
#     choices
#        A list of pairs giving the menu choices which can be selected
#        by the user.  Each pair is of the form {lo hi}, where
#        lo and hi are as in the value public variable.
#
#     value
#        A two-element list {lo hi} giving the low and high percentile
#        values.  0 <= lo <= hi <= 100.  This can be accessed under a 
#        different name using the -valuevar option.
#
#     Percentilecontrol also inherits all the public variables of the 
#     Control widget.

#-

#  Inheritance:
      inherit Control


########################################################################
#  Constructor.
########################################################################
      constructor { args } {

         itk_component add control {
            menubutton [ childsite ].control \
               -width 10 \
               -relief raised \
               -menu [ childsite ].control.menu
         }
         set menubutton $itk_component(control)
         itk_component add menu {
            menu $menubutton.menu
         }
         set menu $itk_component(menu)

         pack $itk_component(control)
         eval itk_initialize $args
      }



########################################################################
#  Public methods.
########################################################################


########################################################################
#  Public variables.
########################################################################

#-----------------------------------------------------------------------
      public variable allowcustom { 1 } {
#-----------------------------------------------------------------------
         if { $allowcustom } {

#  Construct the dialog box if this has not already been done.
            if { [ array names itk_component percentiledialog ] == "" } {
               itk_component add percentiledialog {
                  vectordialog $itk_interior.percdialog 2 \
                     -modality application \
                     -from { 0 0 } \
                     -to { 100 100 } \
                     -label { "Lower percentile" "Upper percentile" }
               }
               $itk_component(percentiledialog) buttonconfigure Apply \
                   -command [ code "$this configure -value \
                              \[ $itk_component(percentiledialog) getvec \]" ]
            }

#  If necessary add a menu item to the menu to invoke the custom dialog box.
            if { [ catch { $menu index "custom" } ] } {
               $menu add command \
                  -label "custom" \
                  -command [ code "$this configure -value \
                             \[ $itk_component(percentiledialog) activate \]" ]
            }

#  If we are being asked to withdraw the option, simply remove it from the
#  menu.
         } else {
            catch { $menu delete "custom" }
         }
      }


#-----------------------------------------------------------------------
      public variable choices { { 1 99 } { 5 95 } { 10 90 } } {
#-----------------------------------------------------------------------
#  Clear out the current contents of the menu.
         $menu delete 0 end

#  Add the items as selected.
         foreach ch $choices {
            set lo [ lindex $ch 0 ]
            set hi [ lindex $ch 1 ]
            if { $lo < 0 } { set lo 0 }
            if { $hi > 100 } { set hi 100 }
            $menu add command \
                -label [ present $lo $hi ] \
                -command [ code "$this configure -value \[ list $lo $hi \]" ]
         }

#  Reinstate the custom choice if required.
         if { $allowcustom } {
            configure -allowcustom $allowcustom
         }

      }


#-----------------------------------------------------------------------
      public variable state { normal } {
#-----------------------------------------------------------------------
         if { $state == "normal" } {
            $menubutton configure -state normal
         } elseif { $state == "disabled" } {
            $menubutton configure -state disabled
         }
      }
 

#-----------------------------------------------------------------------
      public variable value { 0 100 } {
#-----------------------------------------------------------------------
         set lo [ lindex $value 0 ]
         set hi [ lindex $value 1 ]
         if { $lo < 0 || $lo > $hi } { 
            set lo 0 
            configure -value [ list $lo $hi ] 
         }
         if { $hi > 100 || $hi < $lo } { 
            set hi 100 
            configure -value [ list $lo $hi ]
         }
         $menubutton configure -text [ present $lo $hi ]
      }


########################################################################
#  Private variables.
########################################################################

      private variable menu              ;# Path name of the menu widget
      private variable menubutton        ;# Path name of the menubutton widget


########################################################################
#  Private procedures.
########################################################################

#-----------------------------------------------------------------------
      private proc present { lo hi } {
#-----------------------------------------------------------------------
         if { abs( $lo + $hi - 100 ) < 1 } {
            return "$hi%"
         } else {
            return "$lo% - $hi% "
         }
      }
  }


########################################################################
#  Widget resource management
########################################################################

   itk::usual Percentilecontrol {
      keep -background -cursor -foreground 
   }


########################################################################
#  Constructor alias
########################################################################

   proc percentilecontrol { pathname args } {
      uplevel Percentilecontrol $pathname $args
   }
   

# $Id$
