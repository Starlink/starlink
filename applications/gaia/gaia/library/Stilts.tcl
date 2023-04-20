#+
#  Name:
#     Stilts

#  Type of Module:
#     [incr Tcl] class

#  Purpose:
#     Invokes STILTS command to do things with tables.

#  Description:
#     Provides methods for invoking the external STILTS script.
#     It is assumed to be present in $::env(STILTS_DIR).
#
#     Note not currently used. Kept for reference and possible
#     future utility.

#  Configuration options:
#
#        -debug
#
#     Boolean flag which controls whether the "-debug" flag is to be
#     applied when STILTS is executed.  See SUN/256.
#
#        -verbose
#
#     Boolean flag which controls whether the "-verbose" flag is to be
#     applied when STILTS is executed.  See SUN/256.

#  Copyright:
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
#     MBT: Mark Taylor
#     PWD: Peter W. Draper
#     {enter_new_authors_here}

#  History:
#     8-AUG-2006 (MBT):
#        Original version.
#     18-AUG-2006 (PWD):
#        Extend to use GaiaForeignExec to queue the request and run in
#        a background process.
#     {enter_further_changes_here}

#-

itcl::class gaia::Stilts {

   constructor {args} {
      eval configure $args
      set sbin {}
      if {[info exists ::env(STILTS_DIR)]} {
         set sbin $::env(STILTS_DIR)/stilts
      }
      if {[file isfile $sbin] && [file executable $sbin]} {
         set stilts_bin_ $sbin
      }
   }

   #  Destructor.
   destructor {
      if { $stilts_app_ != {} } {
         $stilts_app_ delete_sometime
      }
   }

   #  Returns true if a working STILTS binary has been found, and hence
   #  the execute method is expected to work.
   public method is_working {} {
      return [expr {$stilts_bin_ != ""}]
   }

   #  Executes a STILTS command.  The stilts_cmd argument is the name of
   #  the STILTS command to execute, e.g. "tpipe".  See SUN/256 for
   #  more information.
   public method execute {stilts_cmd args} {
      if {[is_working]} {
         if { $stilts_app_ == {} } {
            set stilts_app_ [gaia::GaiaForeignExec \#auto \
                                -notify [code $this completed_] \
                                -application $stilts_bin_]
         }
         eval $stilts_app_ runwith $stilts_flags_ $stilts_cmd $args
      } else {
         set warning {No executable file ${STILTS_DIR}/stilts}
         if {!$warned_} {
            set warned_ 1
            error_dialog $warning
         }
         error $warning
      }
   }

   #  Called when the last execute command has completed.
   protected method completed_ {} {
      if { $notify_cmd != {} } {
         eval $notify_cmd
      }
   }

   #  Ensures that the stilts_flags_ variable is up to date.
   protected method configure_flags_ {} {
      set sf ""
      if {$debug} {
         lappend sf -debug
      }
      if {$verbose} {
         lappend sf -verbose
      }
      set stilts_flags_ $sf
   }

   #  Configuration options: (public variables)
   #  ----------------------

   public variable debug {0} {
      configure_flags_
   }

   public variable verbose {0} {
      configure_flags_
   }

   #  Command to execute when a command completes.
   public variable notify_cmd {}

   #  Protected variables: (available to instance)
   #  --------------------

   #  Path to STILTS executable.
   protected variable stilts_bin_ {}

   #  The controller class for running the STILTS executable.
   protected variable stilts_app_ {}

   #  Flags array for STILTS command.
   protected variable stilts_flags_ {}

   #  Protected common varabiables:
   #  -----------------------------

   #  Whether a warning about non-functional STILTS has been posted yet.
   protected common warned_ 0
}
