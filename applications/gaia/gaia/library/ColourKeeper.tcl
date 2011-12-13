#+
#  Name:
#     ColourKeeper

#  Type of Module:
#     [incr Tcl] class

#  Purpose:
#     Add facilities for choosing a "standard" colour from a menu of
#     some kind, or choose a bespoke colour using a swatch.

#  Description:
#     This class creates a menu of colours that can be selected by the
#     user. An extra entry "custom" is also provided that allows the
#     user to select and extend the list of colours using a generic
#     RGB swatch.

#  Invocations:
#
#        ColourKeeper object_name [configuration options]
#
#     This creates an instance of a ColourKeeper object. The return is
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

itcl::class gaia::ColourKeeper {

   #  Inheritances:
   #  -------------

   #  Nothing

   #  Constructor:
   #  ------------
   constructor {menu args} {
      configure -menu $menu
      eval configure $args
      init_
   }

   #  Destructor:
   #  -----------
   destructor  {
   }

   #  Methods:
   #  --------

   #  Create the menu items.
   protected method init_ {} {
      add_standard_items_
      add_customizer_
   }

   #  Create and add the standard colour items to the menu.
   protected method add_standard_items_ {} {

      #  Now add all the colours to it.
      foreach {index name} $standard_colours_ {
         add_menu_colour_ $name $index
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
      $menu add \
         -label {Custom...} \
         -command [code $this choose_custom_colour_]
   }

   #  Remove the customizer option from the menu. This should always
   #  be the last entry.
   protected method remove_customizer_ {} {
      $menu delete end
   }

   #  Choose a custom colour and append it to the current list.
   protected method choose_custom_colour_ {} {
      set new_colour [tk_chooseColor -initialcolor white \
                         -parent [winfo toplevel $menu] \
                         -title {Choose a colour}]
      if { $new_colour != {} } {
         set index [add_custom_colour_ $new_colour]
         select_colour $index $new_colour
      }
   }

   #  Add a customised colour to the end of the list.
   protected method add_custom_colour_ {new_colour} {
      remove_customizer_
      add_menu_colour_ $new_colour [incr count_]
      add_customizer_
      lappend custom_colours_ $count_ $new_colour
      return $count_
   }

   #  Select a colour. Invokes the command to issue a creator
   #  callback.
   public method select_colour {index args} {
      if { $change_cmd != {} } {
         eval $change_cmd [lookup_index $index]
      }
   }

   #  Lookup a colour by its index value. Returns white if unknown.
   public method lookup_colour {index} {
      foreach {lindex colour} {
         if { $lindex == $index } {
            return $colour
         }
      }
      return white
   }

   #  Configuration options: (public variables)
   #  ----------------------

   #  The menu that we must attach to.
   public variable menu {}

   #  Command to invoke each time a colour is selected.
   public variable change_cmd {}

   #  Extra colours that should be included in the menu. Note these
   public variable extra_colours {} {
      foreach c $extra_colours_ {
         add_custom_colour_ $c
      }
   }

   #  Protected variables: (available to instance)
   #  --------------------

   #  Custom colours and their indices.
   protected variable custom_colours_ {}

   #  Number of colours in the menu (no more anyway).
   protected variable count_ 0

   #  Common variables: (shared by all instances)
   #  -----------------

   #  The "standard" colours and their indices.
   common standard_colours_ {
      0 "#fff" 1 "#000" 2 "#f00" 3 "#0f0" 4 "#00f" 5 "#0ff" 6 "#f0f"
      7 "#ff0" 8 "#f80" 9 "#8f0" 10 "#0f8" 11 "#08f" 12 "#80f"
      13 "#f08" 14 "#512751275127" 15 "#a8b4a8b4a8b4" }

}
