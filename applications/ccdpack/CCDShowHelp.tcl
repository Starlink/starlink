   proc CCDShowHelp { filename } {
#+
#  Name:
#     CCDShowHelp

#  Type of Module:
#     Tcl procedure.

#  Purpose:
#     Displays a help file in a WWW browser.

#  Description:
#     This routine controls the display of help pages in a HTML WWW
#     browser. The argument is simply the URL of a local file to be
#     displayed. The type of browser used is controlled by the
#     CCDbrowser variable. This should be set to the name of the
#     executable (short name if on the PATH otherwise a full name).
#     If CCDbrowser isn't set it defaults to "Mosaic".

#  Arguments:
#     filename = string (read)
#        The URL of the help page to be displayed.

#  Authors:
#     DLT: D L Terrett (Starlink, RAL)
#     PDRAPER: Peter Draper (STARLINK - Durham University)
#     {enter_new_authors_here}

#  History:
#     29-NOV-1993 (DLT):
#     	 Original version.
#     21-MAR-1995 (PDRAPER):
#        Brought into CCDPACK from Xadam (was named gethelp).
#     22-MAR-1995 (PDRAPER):
#        Added facility to use netscape (1.1) as well as Mosaic.
#     17-APR-1997 (PDRAPER):
#        Modified to show a message when an existing browser is
#        found (which is not started here). This is meant to 
#        drawn the users attention in case the browser is iconified.
#     {enter_further_changes_here}

#  Copyright:
#     Copyright (C) 1993 Science & Engineering Research Council

#-

#  Global variables.
      global CCDbrowser
      global netscapepid
      global mosaicpid
      global showhelp_started
#.

#  Check the browser to use.
      if { ! [info exists CCDbrowser] } { 
         set CCDbrowser netscape
      }
      switch -glob $CCDbrowser {
	 *[Mm]osaic* {

#  Use Mosaic. This relies on the remote-command mechanisms prior to CCI.
	    set mosaicpid 0
	    catch {
	       set in [open ~/.mosaicpid r]
	       gets $in mosaicpid
	       close $in
	    }
	    if { $mosaicpid != 0 } {
	       set fid [open /tmp/Mosaic.$mosaicpid w]
	       puts $fid "goto"
	       puts $fid "file://localhost/$filename"
	       close $fid
	       if { [catch {exec kill -USR1 $mosaicpid}] } {
		  set mosaicpid 0
	       }
	    } 
	    if { $mosaicpid == 0 } {
	       exec  $CCDbrowser $filename &
               set showhelp_started 1
               CCDIssueInfo "Starting up $CCDbrowser"
	    } else {
               if { ! [info exists showhelp_started] } { 
                  set showhelp_started 1
                  CCDIssueInfo "Help is displayed in $CCDbrowser"
               }
            }
	 }

	 *[Nn]etscape* {
	       
#  Use Mozilla. This uses the NCAPIs methods as of netscape 1.1b1.
#  Attempt to make browser goto the required page. If this fails then the 
#  browser has exited for some reason, so restart it.
            if { ! [info exists netscapepid] } { set netscapepid 1 }
	    if { [catch {exec $CCDbrowser -remote openFILE($filename)}] } {
	       set netscapepid 0
	    }
            if { $netscapepid == 0 } { 
               set netscapepid [exec $CCDbrowser $filename &]
               CCDIssueInfo "Starting up $CCDbrowser"
               set showhelp_started 1
	    } else {
               if { ! [info exists showhelp_started] } { 
                  set showhelp_started 1
                  CCDIssueInfo "Help is displayed in $CCDbrowser"
               }
            }
	 }
      }

#  End of procedure.
   }
# $Id$
