#+
#  Name:
#     LabelRule

#  Type of Module:
#     [incr Tk] class

#  Purpose:
#     Defines a class of separator with a label.

#  Description:
#     This class defines an widget that consists of a separator
#     with a label embedded inside it. It is for use when labelling
#     sections of controls.

#  Invocations:
#
#        LabelRule object_name [configuration options]
#
#     This creates an instance of a LabelRule object. The return is
#     the name of the object.
#
#        object_name configure -configuration_options value
#
#     Applies any of the configuration options (after the instance has
#     been created).
#
#        object_name method arguments
#
#     Performs the given method on this object.

#  Configuration options:
#
#        -leftwidth
#        -leftheight
#        -leftrelief
#        -leftborder
#        -rightwidth
#        -rightheight
#        -rightrelief
#        -rightborder
#
#     These options control the width, height, relief and borderwidth
#     of the rules shown to the left and right of the label. The label
#     is set using the -text option of LabelWidget.

#  Inheritance:
#     LabelWidget.

#  Copyright:
#     Copyright (C) 2000-2005 Central Laboratory of the Research Councils.
#     Copyright (C) 2006 Particle Physics & Astronomy Research Council.
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
#     PWD: Peter Draper (STARLINK - Durham University)
#     {enter_new_authors_here}

#  History:
#     08-SEP-1997 (PWD):
#        Original version.
#     {enter_further_changes_here}

#-

#.

itk::usual LabelRule {}

#  Default options for widget.
option add *LabelRule.leftWidth   15
option add *LabelRule.leftHeight  2
option add *LabelRule.leftRelief  sunken
option add *LabelRule.leftBorder  2

option add *LabelRule.rightHeight  2
option add *LabelRule.rightRelief  sunken
option add *LabelRule.rightBorder  2

itcl::class gaia::LabelRule {

   #  Inheritances:
   #  -------------
   inherit util::LabelWidget

   #  Constructor:
   #  ------------
   constructor {args} {

      #  Create the frames that act as the rulers.
      itk_component add left {
         frame $w_.left
      } {
         keep -background
         rename -width -leftwidth leftWidth LeftWidth
         rename -height -leftheight leftHeight LeftHeight
         rename -relief -leftrelief leftRelief LeftRelief
         rename -borderwidth -leftborder leftBorder LeftBorder
      }

      itk_component add right {
         frame $w_.right
      } {
         keep -background
         rename -width -rightwidth rightWidth RightWidth
         rename -height -rightheight rightHeight RightHeight
         rename -relief -rightrelief rightRelief RightRelief
         rename -borderwidth -rightborder rightBorder RightBorder
      }

      pack $itk_component(left) -side left -ipadx 1 -fill x
      pack $itk_component(label) -side left -ipadx 1
      pack $itk_component(right) -side left -ipadx 1 -fill x -expand 1

      #  Evaluate all options..
      eval itk_initialize $args

   }

   #  Destructor:
   #  -----------
   destructor  {
   }
}
