   proc CCDLocateHelpFile { document label } {
#+
#  Name:
#     CCDLocateHelpFile

#  Type of Module:
#     Tcl procedure.

#  Purpose:
#     Locates an HTML help file given a document name and a label.

#  Description:
#     This routine checks the directories $CCDstarhtml for a directory
#     $document.htx and an index file htx.index within this. If this is 
#     located it checks for the existence of a label. If this is located 
#     the name of the associated document is returned, otherwise a 
#     blank string is returned. The document index file is assumed to 
#     have the format
#
#        < filename.html label
#
#     where filename.html is the name of the file in the document in
#     which the hypertext anchor label is found.

#  Arguments:
#     document = string (read)
#        Name of the document in the HTML directory. It is assumed
#        that a document is located in a sub-directory named after it
#        and has an index file of the form described above.
#     label = string (read)
#        The HTML anchor of the part of the document to be returned.

#  Authors:
#     PDRAPER: Peter Draper (STARLINK - Durham University)
#     {enter_new_authors_here}

#  History:
#     21-MAR-1995 (PDRAPER):
#     	 Original version.
#     3-NOV-1995 (PDRAPER):
#        Patched to work with HTX version 1.1-1. Index files now
#        located within document directory. Should use the showme
#        command instead of this routine,
#          catch {showme -l docname label} 
#        will return exit status of showme (1 for failure). The
#        search directories are HTX_PATH.
#     {enter_further_changes_here}

#-

#  Global variables.
      global CCDstarhtml         ;# Locations of the Starlink HTML documents
      global env
#.

#  Try to locate the document directory.
      set filename ""
      foreach directory [split $CCDstarhtml ":" ] {
         if { [file isdirectory $directory/$document.htx] } {
            set index_file $directory/$document.htx/htx.index
            if { [file readable $index_file] } {

#  Found the index file. Look for the label.
               set f [open $index_file r]
               while { [gets $f line] >= 0 } {
                  scan $line "%s %s %s" sign filename thislabel
                  if { [info exists thislabel] } {
                     if { $thislabel == $label && $sign == {<}} {
                        break
                     }
                  } else {
                     
#  If blank label could be reference to whole document.
                     if { $label == "" } {
                        if { [info exists filename] } {
                           break
                        }
                     }
                  }
                  set thislabel ""
                  unset thislabel
                  set filename ""
               }
               
#  Construct filename.
               if { "$filename" != "" } {
                  set filename "$directory/$document.htx/$filename\#xref_$label"
               }
            }

#  If we have found it break out of this loop.
            if { $filename != "" } { break }
         }
      }

#  End of procedure.
   return $filename
   }
# $Id$
