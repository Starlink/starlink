#!/usr/bin/tclsh

#  Open the input file containing the kplsearch template, and find 
#  the placeholders for the routine list.
   set scrin [open kplsearch r]

#  Open the output file to contain the full script.
   set scrout [open kplsearch.new w]

#  Copy the input file to the output file until the placeholder
#  for the routine list is found.
   while { [gets $scrin line] != -1 } {
      if { [regexp {^<purposes placeholder>} $line] } { 
         break 
      } {
         puts $scrout $line
      }          
   }

#  Get a sorted list of all .f files in the current directory.
   set files [lsort [glob "*.f"]]

#  For each .f file...
   foreach file $files {

#  Extract the name and purpose fields from the file prologue.
      set inid [open $file r]
      set step 0
      set name ""
      set purpose ""

      while { [gets $inid line] != -1 } {
         set line [string trim $line]

         if { $step == 0 && [regexp {^\* +Name:} $line] } {
            set step 1      

         } elseif { $step == 1 } {
            if { [regexp {^\* +([^ ]+)} $line match name] } {
               set step 2
            } else {
               break
            }

         } elseif { $step == 2 && [regexp {^\* +Purpose:} $line] } {
            set step 3

         } elseif { $step == 3 } {
            if { [regexp {^\* +(.+)$} $line match text] } {
               append purpose "[string trim $text] "
            } else {
               set step 4
               break
            }
         }
      }

      close $inid

#  Report an error if either name or purpose was not found.
      if { $name == "" } {
         puts ""
         puts ">>> No routine name extracted from $file prologue."
         puts ""

      } elseif { $purpose == "" } {
         puts ""
         puts ">>> No routine purpose extracted from $file prologue."
         puts ""

#  If both found, write a suitable string to the output script.
      } else {
         puts $scrout "   set purpose($name) \"$purpose\""
      }
   }

#  Copy the rest of the input file to the output file.
   while { [gets $scrin line] != -1 } {
      puts $scrout $line
   }

#  Close the input and output files.
   close $scrin
   close $scrout

#  Rename the new script.
   file rename -force kplsearch.new kplsearch 

#  End.
   exit
