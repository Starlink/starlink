proc CCDSetGenGlobals { Topwin args } {

#+
#  Name:
#     CCDSetGenGlobals

#  Purpose:
#     Allows user to set general reduction and configuration parameters.

#  Language:
#     Tcl/Tk

#  Description:
#     This routine displays parameters concerned with the general
#     configuration of CCDPACK. The user may restore the values of
#     these parameters from a CCDSETUP-like restoration file or
#     interactively modify the current values. The option to save the
#     current setup to a disk-file is also available. To exit the user
#     must select "OK".

#  Arguments:
#     Top= window (read)
#        The name of the top-level widget to contain this form.
#     args = strings (read)
#        Command to execute if application is run successfully.

#  Returned Value:
#     True (1) if global values are set, false (0) otherwise.

#  Global Parameters:
#     CCDglobalpars = array (write)
#        The global parameters selected by the user are return in this
#        array which is indexed by the name of the CCDPACK global
#        parameters (i.e. CCDglobalpars(LOGTO) is the LOGTO parameter).

#  Copyright:
#     Copyright (C) 1994 Science & Engineering Research Council.
#     Copyright (C) 1995, 2000-2001 Central Laboratory of the Research
#     Councils. Copyright (C) 2006 Particle Physics & Astronomy
#     Research Council. All Rights Reserved.

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
#     PDRAPER: Peter Draper (STARLINK - Durham University)
#     MBT: Mark Taylor (STARLINK)
#     {enter_new_authors_here}

#  History:
#     22-FEB-1994 (PDRAPER):
#        Original version.
#     21-APR-1994 (PDRAPER):
#        Changed to use mega-widgets.
#     22-MAR-1995 (PDRAPER):
#        Added help system.
#     24-MAY-1995 (PDRAPER):
#        Changed to only use a restricted set of more "general"
#        parameters rather than any CCD specific ones.
#     1-JUN-1995 (PDRAPER):
#        Removed built-in keyboard traversal.
#     21-OCT-1995 (PDRAPER):
#        No longer runs any application so just use OK to exit.
#     16-MAY-2000 (MBT):
#        Upgraded for Tcl8.
#     22-JUN-2001 (MBT):
#        Added USESET parameter.
#     1-JAN-2006 (PDRAPER):
#        Changed to use new meta-widget names (s/Ccd_/Ccd::/g).
#     {enter_changes_here}

#  Bugs:
#     {note_any_bugs_here}

#-


#  Global parameters:
   global CCDglobalpars

#  Local constants:
   set params "LOGTO LOGFILE SATURATE SETSAT GENVAR PRESERVE"

#.

#  Set any undefined variables.
   if { ![info exists CCDglobalpars(LOGTO)] } {
      set CCDglobalpars(LOGTO) BOTH
   }
   if { ![info exists CCDglobalpars(LOGFILE)] } {
      set CCDglobalpars(LOGFILE) CCDPACK.LOG
   }
   if { ![info exists CCDglobalpars(SATURATE)] } {
      set CCDglobalpars(SATURATE) FALSE
   }
   if { ![info exists CCDglobalpars(SETSAT)] } {
      set CCDglobalpars(SETSAT) TRUE
   }
   if { ![info exists CCDglobalpars(GENVAR)] } {
      set CCDglobalpars(GENVAR) FALSE
   }
   if { ![info exists CCDglobalpars(PRESERVE)] } {
      set CCDglobalpars(PRESERVE) TRUE
   }
   if { ![info exists CCDglobalpars(USESET)] } {
      set CCDglobalpars(USESET) FALSE
   }

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#  Widget creation.
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#  Top-level widget.
   CCDCcdWidget Top top \
      Ccd::toplevel $Topwin -title "General reduction parameters"

#  Menubar.
   CCDCcdWidget Menu menu Ccd::helpmenubar $Top.menubar

#  Frame for containing all entries.
   CCDTkWidget Frame frame frame $top.center

#  Radioarray for KEEPLOG value.
   CCDCcdWidget Keeplog keeplog \
      Ccd::radioarray $Frame.keeplog \
                   -label "Keep application log:" \
                   -variable CCDglobalpars(LOGTO)

#  Labelled entry for LOGFILE value.
   CCDCcdWidget Logfile logfile \
      Ccd::labent $Frame.logfile \
                   -text "Name of logfile:" \
                   -textvariable CCDglobalpars(LOGFILE)

#  Radioarray for USESET value.
   CCDCcdWidget Useset useset \
      Ccd::radioarray $Frame.useset \
                 -label "Images are grouped into Sets:" \
                 -variable CCDglobalpars(USESET) \
                 -standard 1 -adampars 1

#  Radioarray for SATURATE value.
   CCDCcdWidget Satur satur \
      Ccd::radioarray $Frame.saturate \
                 -label "Look for saturated pixels:" \
                 -variable CCDglobalpars(SATURATE) \
                 -standard 1 -adampars 1

#  Radioarray for SETSAT value.
   CCDCcdWidget Setset setsat \
      Ccd::radioarray $Frame.setsat\
                  -label "Flag saturated pixels using BAD value:" \
                  -variable CCDglobalpars(SETSAT) \
                  -standard 1 -adampars 1

#  Radioarray for GENVAR value.
   CCDCcdWidget Genvar genvar \
      Ccd::radioarray $Frame.genvar \
                  -label "Generate data errors:" \
                  -variable CCDglobalpars(GENVAR) \
                  -standard 1 -adampars 1

#  Radioarray for PRESERVE value.
   CCDCcdWidget Preserve preserve \
      Ccd::radioarray $Frame.preserve \
                    -label "Preserve data types:" \
                    -variable CCDglobalpars(PRESERVE) \
                    -standard 1 -adampars 1

#  Add choice bar for getting out etc.
   CCDCcdWidget Choice choice Ccd::choice $Top.choice -standard 0

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#  Widget configuration.
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#  File items to accept window and exit interface.
   $Menu addcommand File {Accept Window} "$Choice invoke OK"
   $Menu addcommand File {Exit} CCDExit

#  Add option to menu to access a file and read in the contents into the
#  global parameters CCDglobalpars
   $Menu addcommand Options \
      {Restore setup ....} \
      "global CCDimportexists
       global CCDimportfile
       global CCDimportfilter
       set CCDimportfilter \"*.DAT\"
       CCDGetFileName $Top.restore \"Read restoration file\" 0
       if { \$CCDimportexists } {
          CCDReadRestoreFile \"\$CCDimportfile\"
       }
      "

#  Add options to menu to write the values of the current CCDglobalpars to a
#  named file.
   $Menu addcommand Options \
      {Save setup ....} \
      "global CCDimportavail
       global CCDimportfile
       global CCDimportfilter
       set CCDimportfilter \"*.DAT\"
       CCDNewFileName $Top.getname \"Save restoration file\"
       if { \"\$CCDimportavail\" } {
          if { \[ CCDSaveRestoreFile \"\$CCDimportfile\" \] } {
             CCDIssueInfo \"Saved current setup to file \$CCDimportfile\"
          }
       }
      "

#  Add menu option to select Logfile from existing files.
   $Menu addcommand Options \
      {Select logfile from existing files ....} \
      "global CCDimportexists
       global CCDimportfile
       CCDGetFileName $Top.restore \"Select existing logfile\" 0
       if { \$CCDimportexists } {
          $Logfile clear 0 end
          $Logfile insert 0 \"\$CCDimportfile\"
       }
      "

#  Add buttons to Keeplog. The real application choices are BOTH or TERMINAL.
#  (we need at least TERMINAL output to watch progress etc of tasks).
   $Keeplog addbutton true  BOTH
   $Keeplog addbutton false TERMINAL

#  Add button to Choice. OK check thats the values are ok.
   $Choice addbutton OK \
      "if { \[CCDDoSetGlobals $Top general\] } {
          eval $args
          $Top kill $Top
       }
      "
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#  Widgets and components created so add help.
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   $Top sethelp ccdpack CCDSetGenGlobalsWindow
   $Menu sethelpitem {On Window} ccdpack CCDSetGenGlobalsWindow
   $Menu sethelp all ccdpack CCDSetGenGlobalsMenu

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#  Pack all widgets.
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   pack $menu       -fill x
   pack $choice     -side bottom -fill x
   pack $keeplog    -fill x
   pack $logfile    -fill x
   pack $useset     -fill x
   pack $satur      -fill x
   pack $setsat     -fill x
   pack $genvar     -fill x
   pack $preserve   -fill x
   pack $frame      -fill x -expand true

#  Wait for this procedure to exit.
   CCDWindowWait $Top

#  End of procedure.
}

# $Id$
