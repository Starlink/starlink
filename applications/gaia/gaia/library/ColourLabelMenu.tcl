#+
#  Name:
#     ColourLabelMenu

#  Type of Module:
#     [incr Tcl] class

#  Purpose:
#     Add facilities for choosing a "standard" colour from a LabelMenu,
#     or choose a bespoke colour using a swatch.

#  Description:
#     This class adds colours to a LabelMenu that can be selected by the
#     user. An extra entry "custom" is also provided that allows the
#     user to select and extend the list of colours using a generic
#     RGB swatch. The custom facility is optional.

#  Invocations:
#
#        ColourLabelMenu object_name [configuration options]
#
#     This creates an instance of a ColourLabelMenu object. The return is
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

#  Methods:

#  Inheritance:
#     This object inherits no other classes.

#  Copyright:
#     Copyright (C) 2001-2005 Central Laboratory of the Research Councils.
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
#     02-APR-2001 (PWD):
#        Original version.
#     {enter_further_changes_here}

#-

#.

itcl::class gaia::ColourLabelMenu {

   #  Inheritances:
   #  -------------

   #  Nothing

   #  Constructor:
   #  ------------
   #  Need the LabelMenu option otherwise there would be nothing to do.
   constructor {menu args} {
      configure -menu $menu
      eval configure $args
      init_
   }

   #  Destructor:
   #  -----------
   destructor  {
      if { [winfo exists $swatch_] } {
         destroy $swatch
      }
   }

   #  Methods and Procs:
   #  ------------------

   #  Choose and return a customized colour using a swatch
   #  dialog. This is a proc so that is can be reused. The return is the
   #  new colour.
   public proc choose_custom_colour {} {
      utilReUseWidget gaia::ColourSwatch $swatch_ -withdraw 1
      if { [$swatch_ activate] } {
         return [$swatch_ get]
      }
      return ""
   }

   #  Add a customised colour to the end of the list. If no index is
   #  given then one is chosen and returned.
   public method add_custom_colour {new_colour {index -1} } {
      remove_customizer_
      set index [gaia::AstColours::add_custom_colour $index $new_colour]
      add_menu_colour_ $new_colour $index
      add_customizer_
      return $index
   }

   #  Select a colour. Invokes the command to issue a creator
   #  callback and makes the colour current.
   public method select_colour {index args} {
      set colour [gaia::AstColours::lookup_colour $index]
      $menu configure -value $colour
      if { $change_cmd != {} } {
         eval $change_cmd $index
      }
   }

   #  Create the menu items.
   protected method init_ {} {
      add_standard_items_
      add_customizer_
   }

   #  Create and add the standard colour items to the menu.
   protected method add_standard_items_ {} {

      #  Now add all the colours to it.
      set count [gaia::AstColours::standard_count]
      for {set i 0} {$i < $count} {incr i} {
         add_menu_colour_ [gaia::AstColours::lookup_colour $i] $i
      }
   }

   #  Add a colour to the menu. The arguments are the colour name and
   #  the related index. The button has its background set to the colour.
   protected method add_menu_colour_ {name index} {
      $menu add \
         -label {    } \
         -value $name \
         -background $name \
         -command [code $this select_colour $index]
   }

   #  Add the customizer option to the menu. This should always be last.
   protected method add_customizer_ {} {
      if { $show_custom } {
         $menu add \
            -label {Custom} \
            -command [code $this get_custom_colour_]
      }
   }

   #  Remove the customizer option from the menu. This should always
   #  be the last entry.
   protected method remove_customizer_ {} {
      if { $show_custom } {
         $menu delete end
      }
   }

   #  Choose a custom colour and append it to the current list.
   protected method get_custom_colour_ {} {
      set new_colour [choose_custom_colour]
      if { $new_colour != {} } {
         set index [add_custom_colour $new_colour]
         select_colour $index
      }
   }

   #  Configuration options: (public variables)
   #  ----------------------

   #  The LabelMenu that we populate.
   public variable menu {}

   #  Command to invoke each time a colour is selected. The index of
   #  the selected colour is appended.
   public variable change_cmd {}

   #  Whether to show the in menu customizer. Can only set once at
   #  beginning.
   public variable show_custom 1

   #  Extra colours that should be included in the menu.
   public variable extra_colours {} {
      foreach c $extra_colours_ {
         add_custom_colour $c
      }
   }

   #  Protected variables: (available to instance)
   #  --------------------

   #  Common variables: (shared by all instances)
   #  -----------------

   #  Name of the ColourSwatch widget.
   common swatch_ ".menuswatch"
}
