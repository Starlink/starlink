   proc CCDGetFileUpdate { namebox dirbox dirent filterent images } {
#+
#  Name:
#     CCDGetFileUpdate

#  Purpose:
#     Updates the contents of the filename and directory name listboxes
#     used by the routines CCDGetFileName(s).

#  Language:
#     TCL

#  Type of Module:
#     Tcl/Tk

#  Arguments:
#     namebox = string
#        Listbox in which filenames should be displayed.
#     dirbox = string
#        Listbox in which directory names should be displayed.
#     dirent = string
#        Entry widget from which directory name is read.
#     filterent = string
#        Entry widget from which file filter is read.
#     images = boolean
#        If true, then the intention is to display image files into the
#        namebox; if false the intention is to display all files.
#        Currently the difference is that if images is true any HDS
#        container file which is encountered is passed to ndgexpand
#        in order to look inside it for NDF structures.

#  Implementation Status:
#     The images argument is only treated specially for HDS container
#     files.  It would be nice to have it do the same thing for
#     Multi-Extension FITS files.  This is not easy however.

#  Global Variables:
#     CCDcurrentdirectory = string (read)
#        Name of the current directory.
#     CCDndfcontainers = array (write)
#        An array giving the name of the HDS container file for each
#        NDF which has been encountered (will only be affected if
#        images is true).

#  Copyright:
#     Copyright (C) 1994 Science & Engineering Research Council.
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
#     {original_author_entry}

#  History:
#     2-MAR-1994 (PDRAPER):
#        Original version.
#     14-JUN-2001 (MBT):
#        Added the images argument and associated functionality.
#     {enter_changes_here}

#  Bugs:
#     {note_any_bugs_here}

#-

#  Global parameters.
      global CCDcurrentdirectory
      global CCDndfcontainers

#.

#  Delete present contents of listboxes.
      $namebox clear 0 end
      $dirbox  clear 0 end

#  Get the current directory.
      set moveto [$dirent get]

#  If this is ../ then get the real name. Check for special case of
#  being at / (the root directory). Do not move up from this position.
      if { $moveto == "../" } {
         set moveto [ file dirname $CCDcurrentdirectory ]
      }

#  Expand . to the real directories.
      if { $moveto == "." } { set moveto [pwd] }
      set CCDcurrentdirectory $moveto

#  First add ../ to the directory list so we can get up in the world.
      $dirbox insert end "../"

#  Now add new contents expanding filespec to list of names. Test for the
#  presence of directories, these go into the directory box.
      if { $moveto != "/" } { set moveto "$moveto/" }
      set newfiles "$moveto[$filterent get]"

#  Look for directories. These do not use the file filter.
      foreach name [lsort -dictionary [glob -nocomplain "$moveto*" ] ] {
         if { [file isdirectory $name] } {
            $dirbox insert end $name
         }
      }

#  Now for normal file which do need the file filter.
      foreach filename [lsort -dictionary [glob -nocomplain $newfiles ] ] {
         if { ! [file isdirectory $filename] } {

#  In general use the filename as it is, but in the case where we are
#  specifically looking for images, treat HDS container files specially
#  by passing them to ndgexpand.  This may find zero, one or more
#  NDF structures in each file.
            if { $images && [ regsub {.sdf$} $filename "" clipped ] } {
               set supdatalist [ ndgexpand -sup $clipped ]
               foreach supdata $supdatalist {
                  set hdspath [ lindex $supdata 1 ]
                  set basename [ lindex $supdata 3 ]
                  set dirname [ lindex $supdata 4 ]
                  set ndfname [ lindex $supdata 5 ]
                  set shortname "$basename$hdspath"
                  set dir $CCDcurrentdirectory
                  if { $dir == "/" } {
                     set CCDndfcontainers($ndfname) "/$basename"
                  } else {
                     set CCDndfcontainers($ndfname) "$dir/$basename"
                  }
                  $namebox insert end $shortname
               }
            } else {
               set shortname [ file tail $filename ]
               $namebox insert end $shortname
            }
         }
      }

#  Now set the current directory.
      $dirent clear 0 end
      $dirent insert end $CCDcurrentdirectory

#  End of procedure.
   }
# $Id$
