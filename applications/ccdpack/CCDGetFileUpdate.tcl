   proc CCDGetFileUpdate { namebox dirbox dirent filterent } {
#+
#  Name:
#     CCDGetFileUpdate 

#  Type of Module:
#     Tcl/Tk

#  Purpose:
#    Updates the contents of the filename and directory name listboxes
#    used by the routines CCDGetFileName(s).

#  Authors:
#     PDRAPER: Peter Draper (STARLINK - Durham University)
#     {enter_new_authors_here}

#  History:
#     2-MAR-1994 (PDRAPER):
#     	 Original version.
#     {enter_changes_here}

#-

#  Global parameters.
      global CCDcurrentdirectory

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
      foreach name [lsort [glob -nocomplain "$moveto*" ] ] {
         if { [file isdirectory $name] } { 
            $dirbox insert end $name
         }
      } 

#  Now for normal file which do need the file filter.
      foreach name [lsort [glob -nocomplain $newfiles ] ] {
         if { ! [file isdirectory $name] } { 
            set shortname [ file tail $name ]
            $namebox insert end $shortname
         }
      } 

#  Now set the current directory.
      $dirent clear 0 end
      $dirent insert end $CCDcurrentdirectory

#  End of procedure.
   }
# $Id$
