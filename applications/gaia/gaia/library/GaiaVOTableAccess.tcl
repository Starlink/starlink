#+
#  Name:
#     GaiaVOTableAccess

#  Type of Module:
#     [incr Tcl] class

#  Purpose:
#     Handles VOTable dataset access in GAIA.

#  Description:
#     Wrapper class for accessing a VOTable and its constituent Tables
#     in GAIA. Requires the GaiaVO package. The interface is like that
#     of GaiaNDAccess so that they are somewhat interoperable.

#  Invocations:
#
#        GaiaVOTableAccess object_name [configuration options]
#
#     This creates an instance of a GaiaVOTableAccess object. The return is
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
#     See below.

#  Methods:
#     See below.

#  Inheritance:
#     This object inherits no other classes.

#  Copyright:
#     Copyright (C) 2008 Science and Technology Facilities Council.
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
#     21-JUL-2008 (PWD):
#        Original version.
#     {enter_further_changes_here}

#-

#.

itcl::class gaia::GaiaVOTableAccess {

   #  Inheritances:
   #  -------------

   #  Nothing

   #  Constructor:
   #  ------------

   #  One argument, the specification of the dataset. NDF or FITS file.
   constructor { args } {

      #  Check for GAIAVO.
      if { ! [check_for_gaiavo] }  {
         error "No GAIAVO support available. Cannot handle VOTables"
      }

      #  Evaluate any options, should be the VOTable name.
      eval configure $args
   }

   #  Destructor:
   #  -----------
   destructor  {
      close
   }

   #  Methods:
   #  --------

   #  Open the VOTable file.
   protected method open_ {} {
      close
      set handle_ [::gaiavotable::open $dataset]
   }

   #  Close the VOTable, if open, returns 1 in that case.
   public method close {} {
      if { $handle_ != {} } {
         ::gaiavotable::close $handle_
         set handle_ {}
         set hdu_ -1
         return 1
      }
      return 0
   }

   #  HDU access method. Returns number of TABLEs (each table is an HDU) and
   #  meta-data describing them. Also provides for the switching of the HDU and
   #  querying the current HDU. The args can be "list" or "listheadings" and
   #  "get <n> filename" to save a table to a disk file.
   public method hdu {args} {
      switch -exact [lindex $args 0] {
         list {
            return [::gaiavotable::list $handle_]
         }
         listheadings {
            return [::gaiavotable::listheadings $handle_]
         }
         get {
            return \
               [::gaiavotable::save $handle_ [lindex $args 1] [lindex $args 2]]
         }
         default {
            error "Unknown subcommand: $[lindex $args 0]"
         }
      }
   }

   #  Check for the presence of GAIAVO. Only done once per-session.
   public proc check_for_gaiavo {} {
      if { $have_gaiavo_ == -1 } {
         set have_gaiavo_ 0
         if { [package versions GaiaVO] != "" } {
            if { [ catch {
               package require GaiaVO
               set have_gaiavo_ 1 } msg ] } {
               info_dialog "Failed to load GAIAVO: $msg"
            }
         }
      }
      return $have_gaiavo_
   }

   #  Configuration options: (public variables)
   #  ----------------------

   #  Name of the dataset as supplied by the user.
   public variable dataset {} {
      if { $dataset != {} } {
         open_
      }
   }

   #  Protected variables: (available to instance)
   #  --------------------

   #  The handle to the opened dataset.
   protected variable handle_ {}

   #  Common variables: (shared by all instances)
   #  -----------------

   #  Set when GAIAVO package is available. Need for VOTable support.
   common have_gaiavo_ -1

#  End of class definition.
}
