#+
#  Name:
#     ScrollArrows

#  Type of Module:
#     [incr Tk] class

#  Purpose:
#     Defines a widget for controlling positions using an orthogonal
#     pair of arrows.

#  Description:

#     This class creates a widget consisting of two orthogonal
#     arrows. It is intended for using in positioning other widgets in
#     two dimensions (like fine adjusting the scroll position of an
#     image).

#  Invocations:
#
#        ScrollArrows object_name [configuration options]
#
#     This creates an instance of a ScrollArrows object. The return is
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
#     See itk_option definitions below.

#  Methods:
#     See method declarations below.

#  Inheritance:
#     FrameWidget

#  Copyright:
#     Copyright (C) 1998-2005 Central Laboratory of the Research Councils.
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
#     12-NOV-1998 (PWD):
#        Original version.
#     {enter_further_changes_here}

#-

#.

itk::usual ScrollArrows {}

itcl::class gaia::ScrollArrows {

   #  Inheritances:
   #  -------------
   inherit util::FrameWidget

   #  Constructor.
   constructor {args} {
      eval itk_initialize $args

      #  Create the button and add the appropriate arrows.
      itk_component add up {
         button $w_.up -bitmap up_arrow \
      } {
         keep -background -foreground
      }
      itk_component add down {
         button $w_.down -bitmap down_arrow
      } {
         keep -background -foreground
      }
      itk_component add left {
         button $w_.left -bitmap left_arrow
      } {
         keep -background -foreground
      }
      itk_component add right {
         button $w_.right -bitmap right_arrow
      } {
         keep -background -foreground
      }

      #  Add bindings to control the scroll.
      bind $itk_component(up)    <ButtonPress-1> [code $this start_increment 0 -1]
      bind $itk_component(up)    <ButtonRelease-1> [code $this stop_increment]
      bind $itk_component(down)  <ButtonPress-1> [code $this start_increment 0 1]
      bind $itk_component(down)  <ButtonRelease-1> [code $this stop_increment]
      bind $itk_component(left)  <ButtonPress-1> [code $this start_increment -1 0]
      bind $itk_component(left)  <ButtonRelease-1> [code $this stop_increment]
      bind $itk_component(right) <ButtonPress-1> [code $this start_increment 1 0]
      bind $itk_component(right) <ButtonRelease-1> [code $this stop_increment]

      #  Now position them.
      blt::blttable $w_
      blt::blttable $w_ $itk_component(up)    0,1
      blt::blttable $w_ $itk_component(left)  1,0
      blt::blttable $w_ $itk_component(right) 1,2
      blt::blttable $w_ $itk_component(down)  2,1
   }

   #  Destructor:
   #  -----------
   destructor  {
   }

   #  Methods:
   #  --------

   #  Start incrementing if not already doing so.
   method start_increment {xinc yinc} {
      if { $afterId_ == {} } {
         increment $xinc $yinc
      }
   }

   #  Stop incrementing.
   method stop_increment {} {
      if { $afterId_ != {} } {
         after cancel $afterId_
         set afterId_ {}
      }
   }

   #  Increment or decrement X and Y values by the current increment.
   method increment {xinc yinc} {
      set xinc [expr $xinc*$itk_option(-increment)]
      set yinc [expr $yinc*$itk_option(-increment)]
      if {$itk_option(-command) != {}} {
         set cmd $itk_option(-command)
         lappend cmd $xinc $yinc
         eval $cmd
      }
      set afterId_ [after $itk_option(-delay) [code $this increment $xinc $yinc]]
   }

   #  Configuration options: (public variables)
   #  ----------------------

   #  Incremental step.
   itk_option define -increment increment Increment 1 {}

   #  Command to invoke when X and Y axes are changed. Note the
   #  increment for X and Y are append to this command.
   itk_option define -command command Command {} {}

   #  Delay (ms) between updates when arrow is continually pressed.
   itk_option define -delay delay Delay 50 {}

   #  Protected variables: (available to instance)
   #  --------------------

   #  Id of after command.
   protected variable afterId_ {}

   #  Common variables: (shared by all instances)
   #  -----------------


#  End of class definition.
}
