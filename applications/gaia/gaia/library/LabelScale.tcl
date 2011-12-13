#+
#  Name:
#     LabelScale

#  Type of Module:
#     [incr Tk] class

#  Purpose:
#     Defines a class of widget that labels a scale.

#  Description:
#     This class defines a label with a scale. Addresses the deficiency
#     in label positioning for a scale and adds arrows for auto increment
#     and decrement.

#  Invocations:
#
#        LabelScale object_name [configuration options]
#
#     This creates an instance of a LabelFontChooser object. The return is
#     the name of the object.
#
#        object_name configure -configuration_options value
#
#     Applies any of the configuration options (after the instance has
#     been created).
#
#        object_name method arguments
#
#     Performs the given method on this widget.

#  Configuration options:
#     See itk_option define declarations below.

#  Methods:
#     See descriptions with method declarations below

#  Inheritance:
#     LabelWidget

#  Copyright:
#     Copyright (C) 2009 Science and Technology Facilities Council
#     All Rights Reserved.

#  Licence:
#     This program is free software; you can redistribute it and/or
#     modify it under the terms of the GNU General Public License as
#     published by the Free Software Foundation; either version 2 of the
#     License, or (at your option) any later version.
#
#     This program is distributed in the hope that it will be
#     useful, but WITHOUT ANY WARRANTY; without even the implied warranty
#     of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
#     GNU General Public License for more details.
#
#     You should have received a copy of the GNU General Public License
#     along with this program; if not, write to the Free Software
#     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
#     02110-1301, USA

#  Authors:
#     PWD: Peter Draper (JAC - Durham University)
#     {enter_new_authors_here}

#  History:
#     08-JUN-2009 (PWD):
#        Original version.
#     {enter_further_changes_here}

#-

#.

itk::usual LabelScale {}

itcl::class gaia::LabelScale {

   #  Inheritances:
   #  -------------
   inherit util::LabelWidget

   #  Constructor:
   #  ------------
   constructor {args} {

      #  Frame for the scale widget.
      itk_component add scaleframe {
         frame $w_.scaleframe
      }

      #  Scale.
      itk_component add scale {
         scale $itk_component(scaleframe).scale \
            -showvalue 1 \
            -orient horizontal \
            -command [code $this scaleCmd]
      } {
         keep -background -foreground -length -resolution -digits
         rename -width -scaleWidth scaleWidth ScaleWidth
      }

      #  Left arrow button.
      itk_component add left {
         button $itk_component(scaleframe).left
      } {
         keep -background -foreground
      }
      bind $itk_component(left) <ButtonPress-1> [code $this start_increment -1]
      bind $itk_component(left) <ButtonRelease-1> [code $this stop_increment]

      #  Right arrow button.
      itk_component add right {
         button $itk_component(scaleframe).right
      } {
         keep -background -foreground
      }
      bind $itk_component(right) <ButtonPress-1> [code $this start_increment 1]
      bind $itk_component(right) <ButtonRelease-1> [code $this stop_increment]

      eval itk_initialize $args
   }

   #  Destructor:
   #  -----------
   destructor  {
   }

   #  Methods:
   #  --------

   #  Start incrementing if not already doing so.
   protected method start_increment {sign} {
      if { $afterId_ == {} && $itk_option(-state) == "normal" } {
         increment $sign
      }
   }

   #  Stop incrementing.
   protected method stop_increment {} {
      if { $afterId_ != {} } {
         after cancel $afterId_
         set afterId_ {}
      }
   }

   #  Increment (1) or decrement (-1) the value by the current increment
   protected method increment {sign} {
      set v [expr [get]+($sign*$itk_option(-increment))]
      if { $v >= $itk_option(-from) && $v <= $itk_option(-to) } {
         config -value $v
         if { $itk_option(-command) != {} } {
            set cmd $itk_option(-command)
            lappend cmd $v
            eval $cmd
         }
      }
      set afterId_ [after $itk_option(-delay) [code $this increment $sign]]
   }

   #  This method is called for changes in the scale widget. It does
   #  nothing unless the value has changed, since the Scale has a
   #  motion event that is constantly being invoked.
   protected method scaleCmd {newValue} {
      if { $itk_option(-value) != $newValue } {
         config -value $newValue
         if { $itk_option(-command) != {} } {
            eval $itk_option(-command) $newValue
         }
      }
   }

   #  Set the value (match scale interface).
   public method set {value} {
      configure -value $value
   }

   #  Configuration options: (public variables)
   #  ----------------------

   #  Widget orientation: horizontal or vertical
   itk_option define -orient orient Orient {horizontal} {
      pack $itk_component(scaleframe) -fill x -side $side_
      if { $itk_option(-show_arrows) } {
         pack $itk_component(left) -side left
         $itk_component(left) configure -bitmap left
      }
      pack $itk_component(scale) -side left -expand 1 -fill x -ipadx 1m
      if { $itk_option(-show_arrows) } {
         pack $itk_component(right) -side left
         $itk_component(right) configure -bitmap right
      }
   }

   #  Scale range -from.
   itk_option define -from from From 0 {
      $itk_component(scale) config -from $itk_option(-from)
   }

   #  Scale range -to.
   itk_option define -to to To 1 {
      $itk_component(scale) config -to $itk_option(-to)
   }

   #  Set the value for scale.
   itk_option define -value value Value 0 {
      $itk_component(scale) set $itk_option(-value)
   }

   #  Flag: if true, display left and right arrows for incrementing the value
   itk_option define -show_arrows show_arrows Show_arrows 0

   #  Amount to add or subtract for each button push
   itk_option define -increment increment Increment 1

   #  Command to execute when the value changes
   itk_option define -command command Command {}

   #  Set the state to normal or disabled (greyed out)
   itk_option define -state state State normal {
      $itk_component(scale) config -state $itk_option(-state)
      if { $itk_option(-state) == "normal" } {
         $itk_component(scale) config -foreground $itk_option(-foreground)
      } else {
         $itk_component(scale) config -foreground $itk_option(-disabledforeground)
      }
   }

   #  The pause in milliseconds for the animation delay.
   itk_option define -delay delay Delay 150

   #  Protected variables: (available to instance)
   #  --------------------

   #  State of increment command.
   protected variable afterId_ {}

   #  Common variables: (shared by all instances)
   #  -----------------

#  End of class definition.
}
