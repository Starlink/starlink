   proc CCDIssueError { message } {
#+
#  Name:
#     CCDIssueError

#  Type of Module:
#     Tcl/Tk shell script

#  Purpose:
#     Issues an error using a dialog box if called from a Tk script.
#     Otherwise the error is output to standard output.

#  Arguments
#     message = string (Given)
#        The message to with error.

#  Authors:
#     PDRAPER: Peter Draper (STARLINK - Durham University)
#     {enter_new_authors_here}

#  History:
#     21-FEB-1994 (PDRAPER):
#     	 Original version.
#     31-AUG-1995 (PDRAPER):
#        Removed cluttering message about calling routine.
#     {enter_changes_here}

#-

#  See if this is a Tk application or not.
   global tk_version
   if {[info exists tk_version ]} {
      set is_tk 1
   } else {
      set is_tk 0
   }

#  Now issue the error.
   if { $is_tk } {

#  Check that error window doesn't already exist, if it does wait for
#  it to go away before proceeding.
      if { [winfo exists .ccdissueerror] } { 
         tkwait window .ccdissueinfo
      }
      CCDDialog .ccdissueerror "Error..." "$message" error
   } else {
      puts "Error. $message"
   }

#  All done.
   return      
   }
# $Id$
