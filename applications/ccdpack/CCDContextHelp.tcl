   proc CCDContextHelp { X Y } {
#+
#  Name:
#     CCDContextHelp

#  Type of Module:
#     Tcl/Tk procedure.

#  Purpose:
#     Provides access to Meta-widgets context sensitive help.

#  Description:
#     This routine should be used to trap context sensitive help
#     bindings (such as F1 and Help keypresses). It uses the position
#     information associated with the binding to findout the name of
#     the widget that the mouse pointer is over. It then traverses the
#     tree of widgets until it encounters a widget with class Ccd_
#     (this is the prefix given to all Meta widget classes). It then
#     queries this Meta-widget for the name of the document and label
#     to use as help. It passes these to the appropriate procedures
#     for expansion into a full file name and requests that the help
#     is shown in a browser.

#  Arguments:
#     X = number (read)
#        X position of the mouse pointer when help event occurred
#        (the binding %X value). 
#     Y = number (read)
#        Y position of the mouse pointer when help event occurred
#        (the binding %Y value). 

#  Authors:
#     PDRAPER: Peter Draper (STARLINK - Durham University)
#     {enter_new_authors_here}

#  History:
#     21-MAR-1995 (PDRAPER):
#     	 Original version.
#     {enter_further_changes_here}

#-

#  Global variables.

#.

#  Get the name of the window that the event occurred over.
      set seehelp 1
      set eventwindow [winfo containing $X $Y]
      if { "$eventwindow" != "" } { 

#  Traverse widget tree until we meet a Meta-widget.
	 set window $eventwindow
         while { ! [string match Ccd_* [winfo class $window] ] } {
            set window [winfo parent $window]
            if { "$window" == "" } { 
               set seehelp 0
               break
            }
	 }
	 if { $seehelp } { 

#  Now query Meta-widget about any help it might have.
            set doclabel [$window showhelp $eventwindow]
	    if { "$doclabel" == "" } {
               set seehelp 0
            } else { 

#  Expand docname and label into full name.
               set fullname [CCDLocateHelpFile \
				[lindex $doclabel 0 ] \
				[lindex $doclabel 1 ] ]
               if { "$fullname" == "" } { 
                  set seehelp 0 
               } else {

#  Ask for the help to be displayed.
		  CCDShowHelp $fullname
               }
            }
	 }
      } else {
         set seehelp 0
      }

#  If no help was found say so.
      if { ! $seehelp } {
	 CCDIssueInfo "Sorry no help is available for that region"
      }

#  End of procedure.
   }
# $Id$
