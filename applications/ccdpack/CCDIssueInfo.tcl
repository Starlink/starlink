   proc CCDIssueInfo { message } {
#+
#  Name:
#     CCDIssueInfo.tcl

#  Type of Module:
#     Tcl/Tk shell script

#  Purpose:
#     Issues an informational message a dialog box if called from a Tk
#     script.  Otherwise the message is just output to standard output.
#     If the Tk option is selected an OK button is shown, this needs to
#     be pressed to continue.

#  Arguments
#     message = string (Given)
#        The message to inform user.

#  Authors:
#     PDRAPER: Peter Draper (STARLINK - Durham University)
#     {enter_new_authors_here}

#  History:
#     22-FEB-1994 (PDRAPER):
#     	 Original version.
#     2-MAR-1994 (PDRAPER):
#     	 Now named CCDIssueInfo.
#     13-MAY-1999 (PDRAPER):
#        Modified to use a window that is a child of .topwin (needed
#        to control transient behaviour).
#     {enter_further_changes_here}

#-

#  See if this is a Tk application or not.
   global tk_version
   if {[info exists tk_version ]} {
      set is_tk 1
   } else {
      set is_tk 0
   }

#  Now issue the information message.
   if { $is_tk } {

#  Check that informational window doesn't already exist, if it does
#  wait for it to go away before proceeding.
      if { [winfo exists .topwin.ccdissueinfo] } { 
         tkwait window .topwin.ccdissueinfo
      }
      CCDDialog .topwin.ccdissueinfo "Information" "$message" info
   } else {
      puts "Error. $dialogmess"
   }

#  All done.
   return      
   }
# $Id$
