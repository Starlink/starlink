   proc CCDEditPrefs { topwin } {
#+
#  Name:
#     CCDEditPrefs

#  Type of Module:
#     Tcl/Tk procedure.

#  Purpose:
#     Allows the GUI preferences to be edited.

#  Description:
#     Display the current xreduce preferences and allows them to be
#     edited. Each element of the array CCDprefs is displayed in a
#     labelled entry with no explanation so this requires a little
#     understanding of how Tk and xreduce work.

#  Arguments:
#     topwin = window (read)
#        Name of the top-level window to receive this form.

#  Authors:
#     PDRAPER: Peter Draper (STARLINK - Durham University)
#     {enter_new_authors_here}

#  History:
#     25-MAY-1995 (PDRAPER):
#     	 Original version.
#     {enter_further_changes_here}

#-

#  Global variables.
      global CCDprefs; # all the preferences.

#.

#  Create top-level window and add menubar.
      Ccd_toplevel $topwin -title {xreduce}
      Ccd_helpmenubar $topwin.menubar -standard 0
      pack $topwin.menubar -fill x

#  Test CCDprefs for contents and start to create the required
#  labelled entry widgets.
      if { [ array exists CCDprefs ] } {
         set i 0
         foreach oneof [array names CCDprefs] {
            Ccd_labent $topwin.labent$i \
	       -text "Preference: $oneof" \
	       -textvariable CCDprefs($oneof)
               pack $topwin.labent$i -fill x
            incr i
	 }
      }

#  End of procedure.
   }
# $Id$
