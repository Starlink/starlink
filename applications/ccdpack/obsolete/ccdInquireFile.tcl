proc ccdInquireFile { filter } {

#+
#  Name:
#     ccdInquireFile

#  Type of Module:
#     Tcl procedure.

#  Purpose:
#     Inquires for the name of an existing file from the user.

#  Description:
#     This routine prompts the user for the name of an existing
#     file. Prior to prompting for the name all files which match the
#     given filter are first displayeda and assigned numbers. The user
#     may then use one of these numbers to accept the file or he may
#     view the contents or change to another directory. If he
#     chooses to move directory then all the files in the new
#     directory that match the filter are shown. Control over the
#     file filter is also given.

#  Arguments:
#     filter = string (read)
#        Filter used to select amongst exiting files.

#  Return value:
#     ccdInquireFile = string
#        The name of the selected file, this may be blank.

#  Authors:
#     PDRAPER: Peter Draper (STARLINK - Durham University)
#     {enter_new_authors_here}

#  History:
#     21-SEP-1995 (PDRAPER):
#        Original version.
#     {enter_further_changes_here}

#-

#.

#  Get current directory.
   set originaldir [pwd]

#  Loop until a choice is made.
   set again 1
   while { $again } {

#  Use the file filter to get a list of names.
      set files [glob -nocomplain $filter]

#  Show a list of the files.
      puts "
  List of files matching filter $filter
  in directory [pwd]
      "
      set n 0
      if { "$files" != "" } {
         foreach f $files {
            puts " $n) $f"
            incr n
         }
         puts ""
      } else {
         puts "
  No files match pattern ([pwd]/$filter)
         "
      }

#  And get the option. This can be select a file, view its contents,
#  change directory or the file filter.
      set option [ccdInquire "Choose a number, (v)iew contents, change the (d)irectory or (f)ile filter"]
      switch -regexp $option {
         {^v.*|^V.*} {

#  View file contents, which file?
            set index [ccdInquire "View contents of which file"]
            set file ""
            catch {set file [lindex $files $index]}
            if { $file == "" } {
               puts stderr "
  You must select a number in the range 0-[expr $n -1]
               "
            } else {
               ccdViewFile $file
            }
         }
         {^d.*|^D.*} {

#  Request to change directory.
            set newdir [ccdInquire "New directory"]
            if { $newdir != "" } {
               if { [file isdirectory $newdir] } {
                  cd $newdir
               } else {
                  puts stderr "
  No such directory ($newdir)
                  "
               }
            }
         }
         {^f.*|^F.*} {

#  Modify the file filter.
            set newfilter [ccdInquire "New file filter"]
            if { $newfilter != "" } {
               set filter $newfilter
            }
         }
         default {

#  Must have chosen a number or nothing.
            if { $option != "" } {
               if { ! [catch { set file [lindex $files $option]}] } {
                  if { $file == "" } {
                     if { $n != 0 } {
                        puts stderr "
  You should select a number in the range 0-[expr $n -1]
                        "
                     } else {
                        puts stderr "
  There are no files available for selection
                        "
                     }
                  } else {
                     set file "[pwd]/$file"
                     set again 0
                  }
               } else {

#  Must be unrecognised return.
                  if { $n != 0 } {
                     puts stderr "
  Unrecognised option \"$option\". Either give a number in the range 0-[expr $n -1]
  or one of the characters v, d or f.
                     "
                  } else {
                     puts stderr "
  Unrecognised option \"$option\" choose one of the characters v, d or f.
                     "
                  }
               }
            } else {
               set again 0
               set file ""
            }
         }
      }
   }

#  Finish in same directory as we started.
   cd $originaldir

#  Return the name of the selected file.
   return $file

#  End of procedure.
}
# $Id$
