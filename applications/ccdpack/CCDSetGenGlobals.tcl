proc CCDSetGenGlobals { Top args } {

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

#  Global parameters:
#     CCDglobalpars = array (write)
#        The global parameters selected by the user are return in this
#	 array which is indexed by the name of the CCDPACK global
#	 parameters (i.e. CCDglobalpars(LOGTO) is the LOGTO parameter).

#  Authors:
#     PDRAPER: Peter Draper (STARLINK - Durham University)
#     {enter_new_authors_here}

#  History:
#     22-FEB-1994 (PDRAPER):
#     	 Original version.
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
#     {enter_changes_here}

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

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#  Widget creation.
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#  Top-level widget.
   Ccd_toplevel $Top -title {General reduction parameters}

#  Menubar.
   set Menu [Ccd_helpmenubar $Top.menubar]

#  Frame for containing all entries.
   set Frame [frame $Top.center]

#  Radioarray for KEEPLOG value.
   set Keeplog [Ccd_radioarray $Frame.keeplog \
                   -label {Keep application log:} \
                   -variable {CCDglobalpars(LOGTO)}]

#  Labelled entry for LOGFILE value.
   set Logfile [Ccd_labent $Frame.logfile \
                   -text {Name of logfile:} \
                   -textvariable {CCDglobalpars(LOGFILE)}]

#  Radioarray for SATURATE value.
   set Satur [Ccd_radioarray $Frame.saturate \
                 -label {Look for saturated pixels:} \
                 -variable {CCDglobalpars(SATURATE)}\
                 -standard 1 -adampars 1]

#  Radioarray for SETSAT value.
   set Setsat [Ccd_radioarray $Frame.setsat\
                  -label {Flag saturated pixels using BAD value:} \
                  -variable {CCDglobalpars(SETSAT)} \
                  -standard 1 -adampars 1]

#  Radioarray for GENVAR value.
   set Genvar [Ccd_radioarray $Frame.genvar \
                  -label {Generate data errors:} \
                  -variable {CCDglobalpars(GENVAR)} \
                  -standard 1 -adampars 1]

#  Radioarray for PRESERVE value.
   set Preserve [Ccd_radioarray $Frame.preserve \
                    -label {Preserve data types:} \
                    -variable {CCDglobalpars(PRESERVE)} \
                    -standard 1 -adampars 1]

#  Add choice bar for getting out etc.
   set Choice [Ccd_choice $Top.choice -standard 0]

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
       CCDGetFileName $Top.restore \"Read restoration file\"
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
       CCDGetFileName $Top.restore \"Select existing logfile\"
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
   pack $Menu       -fill x
   pack $Choice     -side bottom -fill x
   pack $Keeplog    -fill x
   pack $Logfile    -fill x
   pack $Satur      -fill x
   pack $Setsat     -fill x
   pack $Genvar     -fill x
   pack $Preserve   -fill x
   pack $Frame      -fill x -expand true

#  Wait for this procedure to exit.
   CCDWindowWait $Top

#  End of procedure.
}

# $Id$
