   proc CCDReduceExtras { Top } {
#+
#  Name:
#     CCDReduceExtras
   
#  Type of Module:
#     Tcl/Tk procedure.
   
#  Purpose:
#     Allows "extra" parameters to be set.
   
#  Description:
#     This procedure allows "extra" parameters to do with controlling a
#     reduction to be changed. These parameters are less relevant
#     parameters such as the extensions to give to NDF names the name of
#     the script to use and its type.

#  Arguments:
#     Top = window (read)
#        The name of the top-level window for this form.

#  Global parameters:
#     CCDglobalpars = array (read and write)
#        The values of the parameters to be set. 
   
#  Authors:
#     PDRAPER: Peter Draper (STARLINK - Durham University)
#     {enter_new_authors_here}

#  Notes:
#     Choice of type of script to run removed as ICL will not run 
#     run a non-tty.
   
#  History:
#     18-MAY-1994 (PDRAPER):
#        Original version.
#     27-MAR-1995 (PDRAPER):
#        Added logfile.
#     1-JUN-1995 (PDRAPER):
#        Removed built-in keyboard traversal.
#     23-AUG-1995 (PDRAPER):
#        Converted to coding style.
#     {enter_changes_here}
   
#-
   
#  Global parameters:
      global CCDglobalpars
#.

#----------------------------------------------------------------------------
#  Widget creation.
#----------------------------------------------------------------------------
   
#  Create a top-level window.
      Ccd_toplevel $Top -title "Reduce additional parameters"

#  Add a menubar.
      set Menu [Ccd_helpmenubar $Top.menubar -standard 1]


#  Intermediary frame extensions. Extension to debiassed NDF names.
      set Debext [Ccd_labent $Top.debext \
                     -text {Extension of debiassed frames:} \
                     -textvariable CCDglobalpars(DEBIASEXT)]

#  Extension to dark corrected NDF names.
      set Darkext [Ccd_labent $Top.darkext \
                      -text {Extension of dark corrected frames:} \
                      -textvariable CCDglobalpars(DARKEXT)]

#  Extension to flash corrected NDF names.
      set Flashext [Ccd_labent $Top.flashext \
                       -text {Extension of flash corrected frames:} \
                       -textvariable CCDglobalpars(FLASHEXT)]

#  Extension to flatfielded NDF names.
      set Flatext [Ccd_labent $Top.flatext \
                      -text {Extension of flatfielded frames:} \
                      -textvariable CCDglobalpars(FLATEXT)]

#  Names of the master NDFs: Bias.
      set Masterbias [Ccd_labent $Top.masterbias \
                         -text {Name of master bias:} \
                         -textvariable CCDglobalpars(MASTERBIAS)]

#  Dark
      set Masterdark [Ccd_labent $Top.masterdark \
                         -text {Name of master dark:} \
                         -textvariable CCDglobalpars(MASTERDARK)]

#  Flash.
      set Masterflash [Ccd_labent $Top.masterflash \
                          -text {Name of master flash:} \
                          -textvariable CCDglobalpars(MASTERFLASH)]

#  Flat prefix.
      set Masterflat [Ccd_labent $Top.masterflat \
                         -text {Prefix name for master flatfields:} \
                         -textvariable CCDglobalpars(MASTERFLAT)]

#  Get the name of the script.
      set Scriptname [Ccd_labent $Top.scriptname \
                         -text {Name of script:} \
                         -textvariable CCDglobalpars(SCRIPTNAME)]

#  Get the type of script.
      set Scripttype [Ccd_radioarray $Top.scripttype \
                         -label {Script type:} \
                         -variable CCDglobalpars(SCRIPTTYPE)]

#  Get the name of the logfile for the reduce job.
      set Exelogfile [Ccd_labent $Top.exelogfile \
                         -text {Name of log file for background job:} \
                         -textvariable CCDglobalpars(EXELOGFILE)]

#  Add choice bar for getting out.
      set Choice [Ccd_choice $Top.choice -standard 0]

#----------------------------------------------------------------------------
#  Widget configuration.
#----------------------------------------------------------------------------

#  Menu.
#  File items to accept window and exit interface.
      $Menu addcommand File {Accept Window} "$Choice invoke OK"
      $Menu addcommand File {Exit} CCDExit

#  Add an option to view the names of the NDFs to be processed.
      $Menu addcommand Options {View frames...} \
         "global CCDallndfs 
          CCDViewLists $Top.view {All available frames} CCDallndfs
         "

#  Scripttype.
#  Add known options for script type.
#      $Scripttype addbutton {C-shell} {CSH}
#      $Scripttype addbutton {ICL} {ICL}

#  Choice.
#  Ok button and just proceeds.
      $Choice addbutton {OK} "$Top kill $Top"

#----------------------------------------------------------------------------
#  Associate help.
#----------------------------------------------------------------------------
      $Top sethelp ccdpack CCDReduceExtrasWindow
      $Menu sethelpitem {On Window} ccdpack CCDReduceExtrasWindow
      $Menu sethelp all ccdpack CCDReduceExtrasMenu

#----------------------------------------------------------------------------
#  Pack all widgets.
#----------------------------------------------------------------------------
      pack $Menu -fill x
      pack $Choice -side bottom -fill x
      pack $Debext $Darkext $Flashext $Flatext -fill x
      pack $Masterbias $Masterdark $Masterflash $Masterflat -fill x
#      pack $Scriptname $Scripttype $Exelogfile -fill x
      pack $Scriptname $Exelogfile -fill x

#  End of procedure.
   }
# $Id$
