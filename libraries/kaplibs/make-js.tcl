#!/usr/bin/tclsh
  
#  Reads all .f files in the current directory and produces an output
#  file containing JavaScript code which initializes an array of objects
#  in which each object decribes the fields of a .f routine prologue. The
#  output file is called <prefix>.js where <prefix> is the first supplied
#  command line argument.
#
#  The current directory should contain a file htx.index describing the
#  hypertext docs, as produced by the hlink command.

#  Proecedure Definitions...
#  =========================
   proc clean { lines } {
      foreach line $lines {
         append ret "[string trim $line] "
      }
      regsub -all {[^A-Za-z0-9 ]} $ret " " ret
      return "\"[string trim $ret]\""
   }

   proc args { lines } {

      set lind 10000000
      foreach line $lines {

         set new 0
         if { [regexp {^( *)([A-Za-z0-9-]+).*=} $line match tmp newarg] } {
            set ind [string length $tmp]
            if { $ind < $lind } {
               set new 1
            }
         } 

         if { $new } {
            set arg [string trim [string toupper $newarg]]

         } else {
            if { [regexp {^( *)[^ ]} $line match tmp] } {
               lappend body($arg) $line
               set lind [string length $tmp]
            }
         }         
      }

      set args [array names body]
      set narg [llength $args]

      set ret "{\n"
      set i 0
      foreach arg $args {
         incr i
         append ret "         $arg: [clean $body($arg)]"
         if { $i < $narg } { append ret ",\n" }
      }

      return "$ret}"

   }

#  Main Entry...
#  =============

#  Initialize
   set names ""

#  Ensure we have a prefix for the output file.
   if { [llength $argv] != 1 } {
      puts "make-js.tcl: Usage: make-js.tcl <prefix>"
      exit
   }
   set pref [lindex $argv 0]

#  Read in the htx.index file produced by running hlink on the hypertext
#  docs produced by star2html. Locate the html file which holds each
#  routine.
   set htx [open "htx.index" r]
   while { [gets $htx line] != -1 } {
      if { [regexp {^< +([^ ]+) +([^ ]+) *$} $line match file name] } { 
         set url([string toupper $name]) $file
      } 
   }
   close $htx

#  Open the output file, and write the header.
   set out [open "$pref.js" w]
   puts $out "   var routines = new Object();\n"

#  Loop round every .f file in the current directory.
   foreach file [lsort [glob "*.f"]] {
      puts "$file ..."

#  Open it.
      set in [open $file r]

#  Prepare for the reading loop.
      set lb 1
      set first 1

#  Loop round reading trimmed lines from the file.
      while { [gets $in line] != -1 } {
         set line [string trim $line]

#  The prologue ends when a string like "*-" is found.
         if { [regexp {^\* *-+ *$} $line] } { break }

#  A new section is marked by a line like "*   Name:". See if this
#  line has the format of a section header. If so copy the section name
#  into variable "newsect".
         set new [regexp {^\* *([^:]+) *: *$} $line match newsect] 

#  Unless this is the first section, the line must be preceeded by a 
#  blank comment line to be a valid section header.
         if { !$first && !$lb } { set new 0 }
                                  
#  If this line marks the start of a new section, store the upper case name 
#  of the section.
         if { $new } {
            set sect [string toupper $newsect]
            set first 0

#  Otherwise, if the line is a comment line, store the text of the line 
#  (minus the comment character).
         } elseif { !$first } {
            if { [regexp {^\*(.*)$} $line match text] } {
               lappend body($sect) $text            
            }
         }

#  Note if this line was a blank comment line or not.
         if { $line == "" || $line == "*" } {
            set lb 1
         } else {
            set lb 0
         }
      }

#  We can only use this routine if it has a name.
      if { [info exists body(NAME)] } {
         set name [string toupper [string trim [lindex $body(NAME) 0] ] ]

#  We only use this routine if another prologue with this name has not
#  already been found, and if it has a url.
         if { [lsearch -exact $names $name] == -1 && [info exists url($name)] } {

            puts $out "   routines.$name = {"
            puts $out "      URL: \"$url($name)\","
   
            if { [info exists body(PURPOSE)] } {
               puts $out "      purpose: [clean $body(PURPOSE)],"
            }
   
            if { [info exists body(DESCRIPTION)] } {
               puts $out "      description: [clean $body(DESCRIPTION)],"
            }
   
            if { [info exists body(ARGUMENTS)] } {
               puts $out "      arguments: [args $body(ARGUMENTS)],"
            }
   
            puts $out "      name: [clean $name] };\n"
            lappend names $name
         }
      }

      catch { unset body }

   }

   close $out

   exit
