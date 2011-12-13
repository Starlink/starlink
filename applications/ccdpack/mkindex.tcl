#+
#  Name:
#     mkindex.tcl

#  Purpose:
#     Make the tclIndex file.

#  Language:
#     TCL

#  Type of Module:
#     Tcl script.

#  Invocation:
#     ccdwish mkindex.tcl

#  Description:
#     This is a script which can be sourced within the ccdwish shell to
#     generate the tclIndex file.  It just runs the standard Tcl
#     auto_mkindex proc on the relevant files in the current directory.
#     These files are pretty much all the files with .tcl, .itcl or .itk
#     extensions, with the exception of those which are themselves
#     designed to be sourced rather than simply defining procs.
#
#     Note that it must be invoked within the directory for which the
#     tclIndex file is to be built.  It must also be invoked with a
#     valid setting of the DISPLAY environment variable (since otherwise
#     ccdwish cannot start up properly).

#  Copyright:
#     Copyright (C) 2001 Central Laboratory of the Research Councils.
#     All Rights Reserved.

#  Licence:
#     This program is free software; you can redistribute it and/or
#     modify it under the terms of the GNU General Public License as
#     published by the Free Software Foundation; either version 2 of
#     the License, or (at your option) any later version.
#
#     This program is distributed in the hope that it will be
#     useful, but WITHOUT ANY WARRANTY; without even the implied
#     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
#     PURPOSE. See the GNU General Public License for more details.
#
#     You should have received a copy of the GNU General Public License
#     along with this program; if not, write to the Free Software
#     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
#     02110-1301, USA

#  Authors:
#     MBT: Mark Taylor (STARLINK)
#     {enter_new_authors_here}

#  History:
#     27-JUN-2001 (MBT):
#        Original version.
#     {enter_further_changes_here}

#  Bugs:
#     {note_any_bugs_here}

#-

#  Local constants:
      set exclude_files {
         CCDMain.tcl
         CCDGeometryMain.tcl
         CCDFileMonitorMain.tcl
         CCDBindings.tcl
         CCDOptions.tcl
         mkindex.tcl
         ccdalign.tcl
         idicurs.tcl
         pairndf.tcl
         raisehack.tcl
         ccdpack_red.tcl
         ccdpack_reg.tcl
         ccdpack_res.tcl
         ccdpack_scr.tcl
      }

#  Withdraw the top level window for tidiness.  A secondary purpose of
#  this is that if wish graphical initialisation fails (which will
#  happen if $DISPLAY is not set properly, and which would cause the
#  index file to be written in a deficient way), this call will fail
#  and cause the script to bomb out in a visible way.
      wm withdraw .

#  Initialise the glob pattern list.
      set patterns {}

#  Work out whether [incr Tcl] is present.
      set hasItcl 0
      set hasItk 0
      foreach ext [info loaded] {
         set extname [lindex $ext 1]
         if { $extname == "Itcl" } { set hasItcl 1 }
         if { $extname == "Itk" } { set hasItk 1 }
      }

#  Work out a list of files to read when constructing the index.
      set files [glob {[a-z]*.tcl} {[a-z]*.tk}]
      if { $hasItcl && $hasItk } {
         foreach f [glob {[A-Z]*.tcl} {[A-Z]*.tk} *.itcl *.itk] {
            lappend files $f
         }
      } {
         lappend exclude_files itcl.tcl itk.tcl
      }

#  Generate a list of the .tcl files to use; all of them apart from
#  named exceptions.
      foreach file $files {
         if { [lsearch -exact $exclude_files $file] == -1 } {
            lappend patterns $file
         }
      }

#  Invoke the command which generates the index.
      eval auto_mkindex . $patterns

#  Exit the interpreter.
      exit
# $Id$
