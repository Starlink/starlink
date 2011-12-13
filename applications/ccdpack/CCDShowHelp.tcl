   proc CCDShowHelp { filename } {
#+
#  Name:
#     CCDShowHelp

#  Purpose:
#     Displays a help file in a WWW browser.

#  Language:
#     TCL

#  Type of Module:
#     Tcl procedure.

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

#  Copyright:
#     Copyright (C) 1993 Science & Engineering Research Council

#  Licence:
#     This program is free software; you can redistribute it and/or
#     modify it under the terms of the GNU General Public License as
#     published by the Free Software Foundation; either version 2 of
#     the License, or (at your option) any later version.
#
#     This program is distributed in the hope that it will be
#     useful, but WITHOUT ANY WARRANTY; without even the implied
#     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
#     PURPOSE. See the GNU General Public License for more details.
#
#     You should have received a copy of the GNU General Public License
#     along with this program; if not, write to the Free Software
#     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
#     02110-1301, USA

#  Authors:
#     DLT: D L Terrett (Starlink, RAL)
#     PDRAPER: Peter Draper (STARLINK - Durham University)
#     MBT: Mark Taylor (Starlink, Bristol)
#     {enter_new_authors_here}

#  History:
#     29-NOV-1993 (DLT):
#        Original version.
#     21-MAR-1995 (PDRAPER):
#        Brought into CCDPACK from Xadam (was named gethelp).
#     22-MAR-1995 (PDRAPER):
#        Added facility to use netscape (1.1) as well as Mosaic.
#     17-APR-1997 (PDRAPER):
#        Modified to show a message when an existing browser is
#        found (which is not started here). This is meant to
#        draw the users attention when the browser is iconified.
#     22-JUL-2003 (MBT):
#        Added the ability to use Mozilla.
#     31-JAN-2006 (PDRAPER):
#        Added firefox (another mozilla/netscape variant).
#     {enter_further_changes_here}

#  Bugs:
#     {note_any_bugs_here}

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
      switch -regexp $CCDbrowser {
	 [Mm]osaic {

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

         [Nn]etscape|[Mm]ozilla|[Ff]irefox {

#  Use Mozilla/variant. This uses the NCAPIs methods as of netscape 1.1b1.
#  Attempt to make browser goto the required page. If this fails then the
#  browser has exited for some reason, so restart it.
            if { ! [info exists netscapepid] } { set netscapepid 1 }
	    if { [catch {exec $CCDbrowser -noraise -remote openURL(file://localhost/$filename)}] } {
	       set netscapepid 0
	    }
            if { $netscapepid == 0 } {
               set netscapepid [exec $CCDbrowser file://localhost/$filename &]
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
