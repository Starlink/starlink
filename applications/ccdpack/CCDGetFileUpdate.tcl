   proc CCDGetFileUpdate { namebox dirbox dirent filterent images } {
#+
#  Name:
#     CCDGetFileUpdate 

#  Type of Module:
#     Tcl/Tk

#  Purpose:
#    Updates the contents of the filename and directory name listboxes
#    used by the routines CCDGetFileName(s).

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

#  Implementation status:
#     The images argument is only treated specially for HDS container
#     files.  It would be nice to have it do the same thing for 
#     Multi-Extension FITS files.  This is not easy however.

#  History:
#     2-MAR-1994 (PDRAPER):
#     	 Original version.
#     14-JUN-2001 (MBT):
#        Added the images argument and associated functionality.
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

#  In general use the filename as it is, but in the case where we are
#  specifically looking for images, treat HDS container files specially
#  by passing them to ndgexpand.  This may find zero, one or more 
#  NDF structures in each file.
            if { $images && [ regsub {.sdf$} $name "" clipped ] } {
               set namelist [ ndgexpand $clipped ]
            } else {
               set namelist $name
            }
            foreach iname $namelist {
               set shortname [ file tail $iname ]
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
