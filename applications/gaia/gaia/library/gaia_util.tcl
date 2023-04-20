#+
#  Name:
#     gaia_util

#  Purpose:
#     Utility procs.

#  Type of Module:
#     Tcl/Tk script.

#  Description:
#     Utility procedures for GAIA. Includes procedure for
#     parsing an NDF filename and a replacement for filename_dialog
#     (in tclutil).

#  Copyright:
#     Copyright (C) 1996-2005 Central Laboratory of the Research Councils.
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
#     PWD: Peter Draper (STARLINK)
#     {enter_new_authors_here}

#  History:
#     21-NOV-1996 (PWD):
#        Original version
#     {enter_changes_here}

#-


#  Define a useful procedure for handling image names with slices.
#  --- NOTE use GaiaImageName class now ---
proc fileName {image} {

   # return the proper filename of an image and any slice information.
   set i1 [string last {(} $image]
   set i2  [string last {)} $image]
   if { $i1 > -1 && $i2 > -1 } {
      set slice [string range $image $i1 $i2]
      incr i1 -1
      set image [string range $image 0 $i1]
   } else {
      set slice ""
   }
   set image2 "$image"
   if { [file extension $image] == "" } {
      set image "${image}.sdf"
   }
   return [list $image $slice]
}

#  Get a file name from the user and return it or the empty string.
#  The optional arguments are the directory (first time only), the
#  filter value, the parent widget and an optional list of file types
#  (suffixes) to display in a menu. Modified to check if default types
#  are changed, before resetting.
proc filename_dialog {{dir "."} {filter "*"} {parent ""} {types ""}} {
   set w .fs
   if {![winfo exists $w]} {
      util::FileSelect $w -dir $dir -filter $filter -transient 1 \
         -withdraw 1 -filter_types "$types"
   } else {

      #  Only reconfigure if default types are changed.
      set curtypes [$w cget -filter_types]
      if { "$curtypes" != "$types" } {
         $w config -filter $filter -filter_types "$types"
      }
   }
   if {"$parent" != ""} {
      wm transient $w [winfo toplevel $parent]
   }
   if {[$w activate]} {
      return [$w get]
   }
}

#  Warning dialog with addition option to execute some command
#  associated with a choice button (like don't show warnings of this
#  class again).
proc option_dialog {realmsg optionmsg optioncommand optionstate {parent ""}} {
    if {"$parent" != ""} {
        if {"[set parent [winfo toplevel $parent]]" == "."} {
            set parent ""
        }
    }
    set w $parent.option_dialog
    catch {destroy $w}
    [gaia::OptionDialog $w \
        -title Warning \
        -text "Warning: $realmsg" \
        -transient 1 \
        -bitmap warning \
        -option_text $optionmsg \
        -option_state $optionstate \
        -option_cmd $optioncommand] activate
}

#  Implement a GAIA version so we can inhibit the raise.
proc gaiaReUseWidget {type w raise args} {
   if {[winfo exists $w]} {
      uplevel "$w config $args"
      if { $raise } {
         utilRaiseWindow $w
      }
   } else {
      uplevel "$type $w $args"
   }
}

#  Apply a binding to all children (useful for meta-widgets that don't offer a
#  bind method).
proc gaiaBindChildren {win sequence script} {
   ::bind $win $sequence $script
   foreach subWin [winfo children $win] {
      gaiaBindChildren $subWin $sequence $script
   }
}
