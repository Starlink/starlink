#!/usr/bin/tclsh
   puts "Adding routine information into sun238"

#  Open the input tex file containing the fixed part, and the
#  placeholders for the routine list and routine descriptions.
   set sunin [open sun238.master r]

#  Open the output tex file to contain the full document.
   set sunout [open sun238.tex w]

#  Copy the input text file to the output tex file until the placeholder
#  for the routine list is found.
   while { [gets $sunin line] != -1 } {
      if { [regexp {^% Insert routine list} $line] } { 
         break 
      } {
         puts $sunout $line
      }          
   }

#  Insert the list into the output text file. Use all prologues in 
#  prolat.tex.
   set latin [open prolat.tex r]
   set step 0
   while { [gets $latin line] != -1 } {

#  Skip blank lines.
      set line [string trim $line]
      if { $line == "" } { continue }

#  Step 0: Look for a line beginning "\sstroutine"
      if { $step == 0 } {
         if { [regexp {^\\sstroutine\{} $line] } { 
            incr step
         }

#  Step 1: Write out the routine name.
      } elseif { $step == 1 } {
         puts $sunout "\\noteroutine\{$line\}\{"
         incr step

#  Step 2: Look for a line equal to close brace,open brace.
      } elseif { $step == 2 } {
         if { [regexp {^\}\{} $line] } { 
            incr step
         }

#  Step 3: Write out the purpose text. End when a line is found equal to
#  close brace,open brace.
      } elseif { $step == 3 } {
         if { [regexp {^\}\{} $line] } { 
            puts $sunout "\}"
            set step 0
         } else {
            puts $sunout $line
         }
      }
   }

#  Close the prolat file.
   close $latin

#  Copy the input text file to the output tex file until the placeholder
#  for the routine descriptions is found.
   while { [gets $sunin line] != -1 } {
      if { [regexp {^% Insert routine descriptions} $line] } { 
         break 
      } {
         puts $sunout $line
      }          
   }

#  Insert the prolat file.
   set latin [open prolat.tex r]
   while { [gets $latin line] != -1 } {
      puts $sunout $line
   }
   close $latin

#  Copy the rest of the input text file to the output tex file.
   while { [gets $sunin line] != -1 } {
      puts $sunout $line
   }

#  Close the input and output tex files.
   close $sunin
   close $sunout
