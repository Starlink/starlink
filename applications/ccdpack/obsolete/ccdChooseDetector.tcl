proc ccdChooseDetector { } {

#+
#  Name:
#     ccdChooseDetector

#  Type of Module:
#     Tcl procedure.

#  Purpose:
#     Choose a detector from from the known list

#  Description:
#     This routine is used to select from a known list of
#     telescopes/detector combinations. The configurations are either
#     CCDSETUP-like files (which describe the CCD characteristics) or
#     an FITS Import Control Table. The two are differentiated by
#     their contents (CCDSETUP-like files have word = statements).
#
#     These files must have a data type .DAT and be stored either in
#     the directory $CCDPACK_DIR (global CCDdir)  or the directory
#     $CCDPACK_CONFIG. The CCDPACK_CONFIG variable is intended for
#     use by users.

#  Arguments:
#     None

#  Return:
#     ccdChooseDetector = integer (read)
#       Either the number of the detector choosen or -1 if none are
#       chosen. The detector number indexes the global array CCDdetectors.

#  Global variables:
#     CCDdetectors = array (write)
#        The element of this array are indexed by a number ($n)and key
#        strings: 
#
#           CCDdetectors($n,name) = short name of the file
#           CCDdetectors($n,file) = full name of the file
#           CCDdetectors($n,type) = the file type (table) or (setup)
#           CCDdetectors($n,comment) = comment read from file

#  Authors:
#     PDRAPER: Peter Draper (STARLINK - Durham University)
#     {enter_new_authors_here}

#  History:
#     21-SEP-1995 (PDRAPER):
#     	 Original version.
#     {enter_further_changes_here}

#-

#  Global variables.
   global env
   global CCDdir
   global CCDdetectorcache
   global CCDdetectors
#.

#  Initialisation.

#  Set the directories to search.
   if { ! [info exists CCDdetectorcache] } {
      set CCDdetectorcache "$CCDdir "
   }
   if { [info exists env(CCDPACK_CONFIG)] } {
      if { ! [ string match "*$env(CCDPACK_CONFIG)*" $CCDdetectorcache] } {
	 append CCDdetectorcache "$env(CCDPACK_CONFIG) "
      }
   }

#  Scan directories for *.DAT files.
   set n 0
   set namelen 0
   foreach dir $CCDdetectorcache {
      if { [file isdirectory $dir] } {
	 set files [glob -nocomplain "$dir/*.DAT"]
	 if { $files != "" } {
	    foreach file $files {
	       set awk [exec awk {{print $2}} $file 2> /dev/null]
	       if { [string match "*=*" $awk] } {

#  Restoration file.
		  set type "(setup)"
	       } else {

#  Import control table (maybe!).
		  set type "(table)"
	       }

#  Look for a comment.
	       set comment [exec head -1 $file 2> /dev/null]
	       if { $comment == "\#" || $comment == "" } {
		  set comment "No description available"
	       }

#  Record the detectors.
	       set CCDdetectors($n,file) $file
               set CCDdetectors($n,name) [file tail $file]
	       set CCDdetectors($n,comment) $comment
	       set CCDdetectors($n,type) $type
	       set newlen [string length $CCDdetectors($n,name)]
               if { $newlen > $namelen } {
                  set namelen $newlen
	       }
               incr n
	    }
	 }
      }
   }

#  Now output these.
   if { $n == 0 } {
      puts stderr "
  Sorry no detectors seem to be available on this system. You'll have 
  to attempt the reduction without any help.
                  "
      set option -1
   } else {
      set again 1
      while { $again } {

#  Now we can introduce this section.
         puts ""
         puts " Listing of known detector files."
         puts " "

#  Output names are identified by a number.
	 puts [format "       %-*s \t %s %s" \
			  $namelen "   Name" "Description" "(type)"]
	 puts ""
	 for { set i 0 } { $i < $n } { incr i } {
	    puts [format "   %d) %-*s \t %s %s" $i $namelen \
                            $CCDdetectors($i,name) $CCDdetectors($i,comment) \
                            $CCDdetectors($i,type)]
	 }
	 puts ""
	 set option [ccdInquire "Choose a number or (v)iew file contents"]
	 if { $option == "V" || $option == "v" } {
	    set option [ccdInquire "View contents of which file"]
	    if { [info exists CCDdetectors($option,file) ] } {
               ccdViewFile $CCDdetectors($option,file)
	    } else {
               puts stderr "  You must select a number in the range 0-[expr $n -1]"
	    }
	 } else {
            if { $option == "" || $option == "!" } { 
               set again 0
               set option -1
            } elseif { [info exists CCDdetectors($option,file) ] } {
               set again 0
	    } else {
               puts stderr "  You must select a number in the range 0-[expr $n -1]"
	    }
	 }
      }
   }
   
#  Return the index of the chosen file.
   return $option

#  End of procedure.
}
# $Id$
