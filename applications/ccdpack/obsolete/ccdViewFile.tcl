proc ccdViewFile { file } {

#+
#  Name:
#     ccdViewFile

#  Type of Module:
#     Tcl procedure.

#  Purpose:
#     Displays the contents of a file on standard output.

#  Description:
#     This routine displays the name and contents of a file. It
#     prompts for confirmation that they have finished reading it 
#     before proceeding.

#  Arguments:
#     file = filename (read)
#        The name of the file to be listed

#  Authors:
#     PDRAPER: Peter Draper (STARLINK - Durham University)
#     {enter_new_authors_here}

#  History:
#     21-SEP-1995 (PDRAPER):
#        Original version.
#     {enter_further_changes_here}

#-

#.

#  List the contents into a variable trapping any possible problems.
   catch {
      set contents [exec -keepnewline cat $file]
   }

#  If it has contents display them.
   if { $contents != "" } {
      puts ""
      set buffer "   Contents of file $file:"
      puts "$buffer"
      set stringlen [string length $buffer]
      set stringlen [expr $stringlen -3]
      set buffer ""
      for { set i 0 } { $i < $stringlen } { incr i } { 
         append buffer = 
      }
      puts "   $buffer"
      puts ""
      puts $contents
      puts ""
      puts "   $buffer"
      puts ""
      ccdInquire "Press return to continue"
   } else { 
      puts stderr "  Cannot list the contents of file $file."
   }
}
# $Id$
