#+
#  Name:
#     GaiaQueryResult

#  Type of Module:
#     [incr Tk] class

#  Purpose:
#     Defines a class for extending the abilities of SkyQueryResult.

#  Description:
#     This class extends SkyQueryResult adding the facilities required
#     for GAIA. Currently this is just the changes necessary to remove
#     the dependence on long_name for local catalogues. This is
#     replaced by url which always has the correct name, even for
#     "foreign catalogues".

#  Invocations:
#
#        GaiaQueryResult object_name [configuration options]
#
#     This creates an instance of a GaiaQueryResult object. The
#     return is the name of the object.
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
#     See the "itk_option define" declarations below.

#  Methods:
#     See the method declarations below.

#  Inheritance:
#     skycat::SkyQueryResult

#  Copyright:
#     Copyright (C) 1998 Central Laboratory of the Research Councils
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
#     30-OCT-1998 (PWD):
#        Original version.
#     {enter_further_changes_here}

#-

#.

itk::usual GaiaQueryResult {}

# A GaiaQueryResult widget is defined as a SkyQueryResult (see skycat
# and cat packages) with overrides for supporting foreign catalogue
# access.

itcl::class gaia::GaiaQueryResult {
   inherit skycat::SkyQueryResult

   #  Constructor
   constructor {args} {
      eval itk_initialize $args
   }

   #  Remove the currently selected rows from a local catalog file.
   public method remove_selected {} {
      set file [{*}$astrocat url]
      set info [get_selected]

      if {[llength $info] == 0} {
         error_dialog "No rows are selected" $w_
         return;
      }

      if {! [confirm_dialog "Remove selected objects?" $w_]} {
         return
      }

      if {[catch {{*}$astrocat remove $file $info $equinox $headings_} msg]} {
         error_dialog $msg $w_
         return
      }
   }

   #  Save the currently selected rows to the local catalog file.
   public method save_selected {} {
      set file [{*}$astrocat url]
      set info [get_selected]

      if {[llength $info] == 0} {
         error_dialog "No rows are selected" $w_
         return;
      }

      if {! [confirm_dialog "Save selected rows?" $w_]} {
         return
      }

      save_to_file $file $info $headings_ 0
   }

   #  This method is called with the data for a new object to add to a
   #  local catalog. The row is added to the local catalog file and the
   #  command is evaluated.
   public method enter_object {command info} {
      if {[check_row $info]} {
         return
      }

      set id [lindex $info [{*}$astrocat id_col]]
      if {! [confirm_dialog "Enter new object with id $id ?" $w_]} {
         return
      }

      set file [{*}$astrocat url]
      save_to_file $file [list $info] $headings_ 1

      #  Eval caller supplied command after change.
      eval $command
   }

   #  This method is called with the new data to replace the data for
   #  the selected object/row in the local catalog. The command is
   #  evaluated when done.
   public method replace_object {command old_data new_data} {
      if {[check_row $new_data]} {
         return
      }

      if {"$old_data" == "$new_data"} {
         info_dialog "No changes were made" $w_
         return
      }

      set file [{*}$astrocat url]
      set id [lindex $new_data 0]

      if {! [confirm_dialog "Update object with id $id ?" $w_]} {
         return
      }

      # remove old data
      if {[catch {{*}$astrocat remove $file [list $old_data] $equinox $headings_} msg]} {
         error_dialog $msg $w_
         return
      }

      # replace with new data
      if {[save_to_file $file [list $new_data] $headings_ 1] != 0} {
         return
      }

      # change command to replace new object if changed again
      $w_.ef config -command [code $this replace_object $command $new_data]

      # eval caller supplied command after change
      eval $command
   }

   #  Override TableList::new_info so we can define the sizes of the
   #  columns using C code to speed things up. Also stop use of WCS
   #  info, this also speeds things up.
   public method new_info {} {
      #  Determine the sizes of the columns, provided that -format
      #  isn't set.
      if { ! $formats_flag_ } {
         configure -sizes [{*}$astrocat csize $itk_option(-info)]
      }
      util::TableList::new_info
   }

   #  Override print to avoid bug. This method shouldn't exist.
   public method print {fd} {
      util::TableList::print $fd
   }

   #  Save the results to a named file.
   public method save_to_named_file {filename} {
      save_to_file $filename $info_ $headings_
   }

   #  Configuration options (public variables):
   #  =========================================
}
