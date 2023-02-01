#+
#  Name:
#     LabelCubeFileChooser

#  Type of Module:
#     [incr Tk] class

#  Purpose:
#     Defines a class for choosing a cube.

#  Description:
#     This class defines a labelled entry field with two buttons. The first
#     button, if pressed, displays a FileChooser for selecting a name from
#     existing files. The second button is a menu button with a list of the
#     cubes that have been already opened. This also supports a Back and
#     Forward concept to move between cubes recently opened and closed.
#     The FileChooser is re-used to keep the context.

#  Invocations:
#
#        LabelCubeFileChooser object_name [configuration options]
#
#     This creates an instance of a LabelCubeFileChooser object. The return is
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
#     LabelFileChooser.

#  Copyright:
#     Copyright (C) 2007 Science and Technology Facilities Council.
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
#     23-OCT-2007 (PWD):
#        Original version.
#     {enter_further_changes_here}

#-

#.

itk::usual LabelCubeFileChooser {}

itcl::class gaia3d::LabelCubeFileChooser {

   #  Inheritances:
   #  -------------
   inherit gaia::LabelFileChooser

   #  Constructor:
   #  ------------
   constructor {args} {

      #  Create the object to manage the cube list and history. Note this
      #  requires some methods of the GaiaCube and GaiaSpecCoords classes are
      #  implemented as dummies to honour the interfaces. Also need a
      #  cubeaccessor so that cube size can be stored with history.
      set history_ [gaia::GaiaCubeHistory \#auto -gaia_cube $this]

      #  Add a menu button to the widget.
      set menu $itk_component(eframe).go.m
      itk_component add go {
         menubutton $itk_component(eframe).go -text "Go" \
            -menu $menu
      } {
         keep -indicatoron -borderwidth -state
         rename -relief -buttonrelief buttonRelief ButtonRelief
         rename -width -buttonwidth buttonWidth ButtonWidth
         rename -anchor -buttonanchor buttonAnchor ButtonAnchor
         ignore -disabledforeground -font
      }

      itk_component add menu {
         menu $menu \
            -postcommand [code $history_ update_history_menu $menu]
      } {
         ignore -disabledforeground

      }
      pack $itk_component(go) -side right

      #  Now handle unprocessed configurations.
      eval itk_initialize $args
   }

   #  Destructor:
   #  -----------
   destructor  {
   }

   #  Methods:
   #  --------

   #  Add a cube to the history. Should be called after a cube has been
   #  accepted.
   public method add_to_history {name} {
      $history_ add_history $name
      $history_ record_last_cube
   }

   #  GaiaCube implementation.
   public method get_spec_coords {} {
      return $this
   }
   public method add_menu_short_help {args} {
      return {}
   }
   public method get_cubeaccessor {} {
      return $itk_option(-cubeaccessor)
   }

   #  GaiaSpecCoords implementation. Does nothing.
   public method get_system {} {
      return "default default"
   }
   public method set_system {args} {
      #  Do nothing.
   }

   #  Configuration options: (public variables)
   #  ----------------------

   #  A cubeaccessor. Required.
   itk_option define -cubeaccessor cubeaccessor CubeAccessor {}

   #  GaiaCube configuration item.
   itk_option define -cube cube Cube {} {
      configure -value $itk_option(-cube)
   }

   #  Orientation.
   itk_option define -orientation orientation Orientation right {
      if { $itk_option(-orientation) == "vertical" } {
         pack $itk_component(eframe) -side bottom -fill x -expand 1 -padx 1m
      } else {
         pack $itk_component(eframe) -side right -fill x -expand 1 -padx 1m
      }
   }

   #  Protected variables: (available to instance)
   #  --------------------

   #  The cube history object.
   protected variable history_ {}

   #  Common variables: (shared by all instances)
   #  -----------------

#  End of class definition.
}
