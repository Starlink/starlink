proc ccdPagedOutput { output } {

#+
#  Name:
#     ccdPagedOutput

#  Type of Module:
#     Tcl procedure.

#  Purpose:
#     Writes output to stdout.

#  Description:
#     This routine writes the value of the variable "$output" to 
#     standard output. If uses the UNIX command more to ensure that 
#     all the output can be viewed a page at a time if it exceeds 
#     the length of a screen.

#  Arguments:
#     output = string (read)
#        The string to write out to the user, ensuring that all its
#        contents are viewable.

#  Notes:
#     There is a potential problem with more on different UNIXes.
#     The OSF/1 version pauses at the end of a file regardless, this
#     is disabled by the -e flag which isn't present on Solaris or
#     SunOS who fortunately ignore it.

#  Authors:
#     PDRAPER: Peter Draper (STARLINK - Durham University)
#     {enter_new_authors_here}

#  History:
#     22-SEP-1995 (PDRAPER):
#        Original version.
#     {enter_further_changes_here}

#-

#.

#  Open a pipe to more.
   set pipe [open "|more -de" w]

#  Send the contents of output to it.
   puts $pipe "$output"

#  Close the pipe forcing more to flush its buffers.
   close $pipe

#  End of procedure.
}
# $Id$
