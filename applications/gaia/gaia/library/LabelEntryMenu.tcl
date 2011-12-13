#+
#  Name:
#     LabelEntryMenu

#  Type of Module:
#     [incr Tcl] class

#  Purpose:
#     Defines the class of labelled entry widgets with a menu of known
#     options.

#  Description:
#     This class creates a labelled entry widget with an additional
#     menubutton. A menu of known options are associated with the
#     menubutton and will be entered into the entry widget when
#     selected, unless a special "other" option is chosen in which
#     case any string can be typed.

#  Configuration options:

#  Methods:

#  Inheritance:
#     This class inherits LabelMenu. It is also based on LabelMenu.

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
#     26-AUG-1997 (PWD):
#     	 Original version.
#     {enter_changes_here}

#-

itk::usual LabelEntryMenu {}

itcl::class gaia::LabelEntryMenu {
   inherit util::LabelEntry

   #  Create a LabelEntryMenu.
   constructor {args} {
      set menu $w_.mb.m
      itk_component add mb {
         menubutton $w_.mb -menu $menu
      } {
         keep -indicatoron -borderwidth -state
         rename -relief -buttonrelief buttonRelief ButtonRelief
         rename -width -buttonwidth buttonWidth ButtonWidth
         rename -anchor -buttonanchor buttonAnchor ButtonAnchor
         ignore -disabledforeground -font
      }

      itk_component add menu {
         menu $menu
      } {
         ignore -disabledforeground
      }

      set default_bg_ [$itk_component(mb) cget -background]

      #  Trace var for setting the entry field.
      global ::$w_.var
      set $w_.var ""
      trace variable $w_.var w [code $this update_entryfield]

      eval itk_initialize $args
   }

   #  Destructor.
   destructor {
      global ::$w_.var
      catch {unset $w_.var}
   }


   #  Remove all of the items in the menu.
   method clear {} {
      $itk_component(menu) delete 0 end
   }


   #  Add a separator item to the menu.
   method add_separator {} {
      $itk_component(menu) add separator
   }

   #  Add an item to the menu.
   #  The args may be the options:
   #     -label <label for menuitem>
   #     -bitmap <bitmap for menuitem>
   #     -command <cmd to execute when item is selected>
   #     -background <color of menu item and button when chosen>
   #     -font <font of menu item and button when chosen>
   method add {args} {
      global ::$w_.var

      # Command to add menu item.
      set cmd "$itk_component(menu) add radiobutton"

      foreach {opt arg} "$args" {
         switch -exact [string range $opt 1 end] {
            label {
               lappend cmd -label $arg
               set name $arg
            }
            bitmap {
               lappend cmd -bitmap $arg
               set name $arg
            }
            command {
               lappend cmd -command $arg
            }
            bg -
            background {
               if {"$arg" == ""} {
                  set mb_color $default_bg_
               } else {
                  set mb_color $arg
               }
               lappend cmd -background $arg -activebackground $arg
            }
            font {
               lappend cmd -font $arg
            }
            value {
               set value $arg
            }
            default {
               lappend cmd $opt $arg
            }
         }
      }
      if { ! [info exists value] } {
         set value [incr unique_]
      }

      if { ! [info exists name] } {
         set name $value
      }

      set values_($name) $value

      lappend cmd -variable $w_.var -value $value
      eval $cmd

      #  Set default value to first item.
      if {"[set $w_.var]" == ""} {
         set $w_.var $value
      }
   }

   #  Return true if the LabelEntryMenu contains the given item
   #  (specified by the label or bitmap name)

   method contains {name} {
      return [info exists values_($name)]
   }

   #  Update the entry field.

   method update_entryfield {args} {
      global ::$w_.var
      if {"[set $w_.var]" != ""} {
         $itk_component(entry) delete 0 end
         $itk_component(entry) insert 0 [set $w_.var]
      }
   }


   #  Called when "$variable_" has changed, via trace, to update menu.

   method variable_changed {args} {
      global $variable_
      config -value [set $variable_]
      $itk_component(entry) configure -text [set $variable_]
   }


   #  Options.

   #  Set the selected value (referenced by label or bitmap)
   #  note: this.var is the trace variable and values_ holds the
   #  valid values. If an unknown label or bitmap is supplied,
   #  all radiobuttons are unselected.
   itk_option define -value value Value {} {
      global ::$w_.var
      if { [info exists values_($itk_option(-value))] } {
         set v $values_($itk_option(-value))
      } else {
         set v ""
      }
      catch {set $w_.var $v}
   }

   #  Global variable linked to menu.
   itk_option define -variable variable Variable {} {
      set variable_ $itk_option(-variable)
      trace variable $variable_ w [code $this variable_changed]
   }

   #  Widget orientation: horizontal or vertical.
   itk_option define -orient orient Orient {horizontal} {
      pack $itk_component(mb) \
         -side $side_ -fill x -expand 1 -padx 1m -ipadx 1m
   }

   # -- protected variables --

   #  Default background color.
   protected variable default_bg_

   #  Array(label or bitmap name) of values for selecting a radiobutton.
   protected variable values_

   #  Trace variable name.
   protected variable variable_

   #  Unique number.
   protected variable unique_ 0
}



