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
            frame [ childsite ].control
         }
         itk_component add percentile {
            iwidgets::optionmenu $itk_component(control).percentile \
               -width 75 \
               -command [ code $this setvalue ]
         }
         set omenu $itk_component(percentile)
         set omenubut [ $omenu component menuBtn ]
         $omenubut configure -textvariable ""
         pack $omenu -fill both -expand 1
         pack $itk_component(control) -fill both -expand 1
         configure -balloonstr "Brightest/darkest image pixel cutoff"
         eval itk_initialize $args
      }



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
                     -master $itk_interior \
                     -from { 0 0 } \
                     -to { 100 100 } \
                     -label { "Lower percentile" "Upper percentile" }
                     
               }
               $itk_component(percentiledialog) buttonconfigure Apply \
                   -command [ code "$this configure -value \
                              \[ $itk_component(percentiledialog) getvec \]" ]
            }

#  If necessary add a menu item to the menu to invoke the custom dialog box.
            if { [ catch { $omenu index "custom" } ] } {
               addchoice "custom"
            }

#  If we are being asked to withdraw the option, simply remove it from the
#  menu.
         } else {
            catch { $omenu delete "custom" }
         }
      }


#-----------------------------------------------------------------------
      public variable choices { { 1 99 } { 5 95 } { 10 90 } } {
#-----------------------------------------------------------------------
#  Clear out the current contents of the menu.
         $omenu delete 0 end

#  Add the items as selected.
         foreach ch $choices {
            set lo [ lindex $ch 0 ]
            set hi [ lindex $ch 1 ]
            if { $lo < 0 } { set lo 0 }
            if { $hi > 100 } { set hi 100 }
            addchoice [ list2text $lo $hi ]
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
            $omenu configure -state normal
         } elseif { $state == "disabled" } {
            $omenu configure -state disabled
         }
      }
 

#-----------------------------------------------------------------------
      public variable value { 0 100 } {
#-----------------------------------------------------------------------
         set lo [ lindex $value 0 ]
         set hi [ lindex $value 1 ]
         if { $lo < 0 || $lo >= $hi || $hi > 100 } { 
            set lo 0 
            set hi 100
            configure -value [ list $lo $hi ] 
         }
         set text [ list2text $lo $hi ]
         if { [ catch { $omenu select $text } ] } {
            addchoice $text
            $omenu select $text
         }
         $omenubut configure -text "[ lindex $value 1 ]%"
      }


########################################################################
#  Private methods.
########################################################################

#-----------------------------------------------------------------------
      private method addchoice { text } {
#-----------------------------------------------------------------------
         set val [ text2list $text ]
         if { [ catch { $omenu index $text } ] == 0 } return
         if { $val == "custom" } {
            $omenu insert end $text
            return
         } else {
            set lo [ lindex $val 0 ]
            set hi [ lindex $val 1 ]
            for { set i 0 } { $i < [ $omenu index end ] } { incr i } {
               set ival [ text2list [ $omenu get $i ] ]
               if { $ival != "custom" } {
                  set ilo [ lindex $ival 0 ]
                  set ihi [ lindex $ival 1 ]
                  if { $hi > $ihi || $hi == $ihi && $lo < $ilo } {
                     $omenu insert [ expr $i ] $text
                     return
                  }
               }
            }
            if { [ $omenu get end ] == "custom" } {
               $omenu insert "custom" $text
            } else {
               $omenu insert end $text
            }
         }
      }


#-----------------------------------------------------------------------
      private method setvalue { } {
#-----------------------------------------------------------------------
         set val [ text2list [ $omenu get ] ]
         if { [ llength $val ] == 2 } {
            configure -value $val
         } else {
            $itk_component(percentiledialog) setvec $value
            $itk_component(percentiledialog) center $itk_interior
            configure -value [ $itk_component(percentiledialog) activate ]
         }
         $omenubut configure -text "[ lindex $value 1 ]%"
      }
   



########################################################################
#  Private variables.
########################################################################

      private variable omenu             ;# Path name of the optionmenu widget
      private variable omenubut          ;# Path name of menubutton component


########################################################################
#  Private procedures.
########################################################################

#-----------------------------------------------------------------------
      private proc list2text { lo hi } {
#-----------------------------------------------------------------------
         return "$lo% - $hi% "
      }


#-----------------------------------------------------------------------
      private proc text2list { text } {
#-----------------------------------------------------------------------
         if { [ regexp {^ *([0-9.]*)% *- *([0-9.]*)% *$} $text dummy lo hi ] } {
            return [ list $lo $hi ]
         } elseif { [ regexp {^ *([0-9.]*)% *$} $text dummy hi ] } {
            return [ list [ expr 100 - $hi ] $hi ]
         } else {
            return "custom"
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
