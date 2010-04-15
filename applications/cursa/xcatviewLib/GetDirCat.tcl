proc GetDirCat { } {

#+ GetDirCat
#
#  Get the name of the current directory, a list of subdirectories in
#  this directory and a list of catalogues in this directory.  These
#  quantities are stored in global variables.
#
#  Method:
#   Get the current directory.
#   Initialise the subdirectory list.
#   Initialise the list of catalogues.
#   For each file in the current directory
#     If the file is a directory then
#       Append the current file to the list of subdirectories.
#     end if
#     If the file is a catalogue then
#       Append the current file to the list of catalogues.
#     end if
#   end for
#   Sort the subdirectory list.
#   Sort the list of catalogues.
#   Insert the list of directories into the appropriate listbox.
#   Insert the list of catalogues into the appropriate listbox.
#   Insert the current directory into the text box.


#  Author:
#   ACD: A C Davenhall (Leicester).
#
#  History:
#   6/10/94  (ACD): Original version.
#   10/5/95  (ACD): Added handling of HST GSC FITS tables, which have
#      file type '.gsc' or '.GSC'.
#   27/2/96  (ACD): Modified to handle file types '.FITS' and '.fits'
#      for FITS tables.
#   30/7/96  (ACD): Added types '.TXT' and '.txt' for small text lists.
#   11/11/97 (ACD): Added sorting of the lists of catalogues and
#      subdirectories, so that they are displayed in alphabetical order.
#   23/11/99 (ACD): Added types '.TAB' and '.tab' for small tab-separated
#      tables.
#-

#
# Get the current directory.

   set currentDir [pwd]

#
# Initialise the lists of subdirectories and catalogues.

   set subdirectoryList  ""
   set cataloguesList    ""

#
# Examine each file in the directory.

   foreach file [glob -nocomplain $currentDir/*] {

#
#    Check if the file is a directory and if so then append it to the
#    list of directories.

      if {[file isdirectory $file]} then {
         set subdirectoryList [lappend subdirectoryList $file]
      }

#
#    Check if the file is a catalogue, and if so then append it to the
#    list of catalogues.  Note that the various catalogue file types
#    are checked for separately.

      if {[string match *.fit $file]} then {
         set cataloguesList [lappend cataloguesList $file]
      }

      if {[string match *.FIT $file]} then {
         set cataloguesList [lappend cataloguesList $file]
      }

      if {[string match *.fits $file]} then {
         set cataloguesList [lappend cataloguesList $file]
      }

      if {[string match *.FITS $file]} then {
         set cataloguesList [lappend cataloguesList $file]
      }

      if {[string match *.gsc $file]} then {
         set cataloguesList [lappend cataloguesList $file]
      }

      if {[string match *.GSC $file]} then {
         set cataloguesList [lappend cataloguesList $file]
      }

      if {[string match *.sdf $file]} then {
         set cataloguesList [lappend cataloguesList $file]
      }

      if {[string match *.TXT $file]} then {
         set cataloguesList [lappend cataloguesList $file]
      }

      if {[string match *.txt $file]} then {
         set cataloguesList [lappend cataloguesList $file]
      }

      if {[string match *.TAB $file]} then {
         set cataloguesList [lappend cataloguesList $file]
      }

      if {[string match *.tab $file]} then {
         set cataloguesList [lappend cataloguesList $file]
      }
   }

#
# Sort the lists of subdirectory and catalogues.

   set subdirectoryList [lsort $subdirectoryList]
   set cataloguesList   [lsort $cataloguesList]

#
# Insert the list of directories into the appropriate listbox.  Any
# previous contents of the listbox are deleted prior to writing the
# new contents.

   .getcatalogue.spec.dir.list delete 0 end

   set numItems [llength $subdirectoryList]

   set counter 0

   while {$counter < $numItems} {
      set currentDir [lrange $subdirectoryList $counter $counter]
      .getcatalogue.spec.dir.list insert end $currentDir
      set counter [expr $counter + 1]
   }

#
# Insert the list of catalogues into the appropriate listbox.  Any
# previous contents of the listbox are deleted prior to writing the
# new contents.  Note that the preceding directory specification is stripped
# from the catalogue # name.

   .getcatalogue.spec.cats.list delete 0 end

   set numItems [llength $cataloguesList]

   set counter 0

   while {$counter < $numItems} {
      set currentCat [lrange $cataloguesList $counter $counter]

      set nameStart [string last / $currentCat]
      set nameStart [expr $nameStart + 1]
      set catName [string range $currentCat $nameStart end]

      .getcatalogue.spec.cats.list insert end $catName
      set counter [expr $counter + 1]
   }

#
# Insert the current directory into the text box.

   set currentDir [pwd]
   .getcatalogue.current.dir delete 0 end
   .getcatalogue.current.dir insert 0 $currentDir

}
