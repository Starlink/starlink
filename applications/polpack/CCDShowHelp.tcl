   proc CCDShowHelp { url } {
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
#     url = string (read)
#        The URL of the help page to be displayed.

#  Copyright:
#     Copyright (C) 1998 Central Laboratory of the Research Councils

#  Authors:
#     DLT: D L Terrett (Starlink, RAL)
#     PDRAPER: Peter Draper (STARLINK - Durham University)
#     DSB: David Berry (STARLINK- Manchester University)
#     {enter_new_authors_here}

#  History:
#     29-NOV-1993 (DLT):
#     	 Original version.
#     21-MAR-1995 (PDRAPER):
#        Brought into CCDPACK from Xadam (was named gethelp).
#     22-MAR-1995 (PDRAPER):
#        Added facility to use netscape (1.1) as well as Mosaic.
#     29-JUN-1997 (DSB):
#        Brought into POLPACK from CCDPACK. Calls to CCDIssueInfo changed
#        to Message. Check environment variable HTX_BROWSER instead of
#        Tcl variable CCDbrowser to determine the browser to use.
#        Supplied argument changed from a file name to a URL (and the
#        netscape remote openFILE command changed to openURL).
#     6-JUL-1998 (DSB):
#        Modified to use HTX_BROWSER as supplied instead of only using
#        "netscape" or "mosaic".
#     13-APR-2007 (DSB):
#        Add firefox and make it the default browser.
#     16-FEB-2015 (DSB):
#        Add chrome and remove mosaic.
#     {enter_further_changes_here}

#-

#  Global variables.
      global env
      global netscapepid
      global mosaicpid
#.

#  Check the browser to use. If HTX_BROWSER doesn't exist use firefox.
#  If it does, use it.
      if { ! [info exists env(HTX_BROWSER)] } {
         set CCDbrowser firefox
      } {
         set CCDbrowser $env(HTX_BROWSER)
      }

      switch -regexp $CCDbrowser {

         [Cc]hrome|google-chrome {
            exec $CCDbrowser $url &
         }

         [Nn]etscape|[Mm]ozilla|[Ff]irefox {

#  Use Mozilla or variant. This uses the NCAPIs methods as of netscape 1.1b1.
#  Attempt to make browser goto the required page. If this fails then the
#  browser has exited for some reason, so restart it.
            if { ! [info exists netscapepid] } { set netscapepid 1 }
	    if { [catch {exec $CCDbrowser -remote openURL($url)} mess] } {
	       set netscapepid 0
	    }
            if { $netscapepid == 0 } {
               if { [catch { set netscapepid [exec $CCDbrowser $url &]} mess] } {
                  Message "Failed to start $CCDbrowser - $mess"
               }
	    }
	 }
      }
   }
