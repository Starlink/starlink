#!/usr/bin/tclsh
#
#  Lists the prefixes of starlink libraries used by any of the .f files
#  in the current directory.

   foreach file [glob "*.f"] {
      set f [open $file r]
      while { [gets $f line] != -1 } { 
         if { [regexp {^      +CALL +([A-Z][A-Z][A-Z]1?_)} $line match pref] == 1 } {
            set p($pref) 1
         }
      }
      close $f
   }

   puts [lsort [array names p]]
