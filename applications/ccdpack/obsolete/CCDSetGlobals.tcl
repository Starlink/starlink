   proc CCDSetGlobals { topwin } {

#+
#  Name:
#     CCDSetGlobals

#  Purpose:
#     Allows user to set the CCDPACK global parameters.

#  Language:
#     Tcl/Tk

#  Description:
#     This routine displays all the CCDPACK global parameters in a
#     top-level widget. The user may then restore the values of these
#     parameters from a CCDSETUP-like restoration file or interactively
#     modify the current values. The option to save the current setup
#     to a disk-file is also available. To exit the user must select
#     one of the options "Accept" or "Cancel", "Accept" sets the global
#     variables and exits this form. "Cancel" resets the global values 
#     and returns. 

#  Arguments:
#     topwin = window (read)
#        The name of the top-level widget to contain this form.

#  Returned Value:
#     No value is returned.

#  Global parameters:
#     CCDglobalpars = array (write)
#        The global parameters selected by the user are return in this
#	 array which is indexed by the name of the CCDPACK global
#	 parameters (i.e. CCDglobalpars(ADC) is the ADC factor).

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
#     1-JUN-1995 (PDRAPER):
#        Removed built-in keyboard traversal.
#     {enter_changes_here}

#-


#  Global parameters:
      global CCDglobalpars

#.

#  Create a top-level widget. This contains the whole of the parameter
#  set associated with this procedure.
      Ccd_toplevel $topwin -title {Global parameters}

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#  define upper menu bar
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      Ccd_helpmenubar $topwin.menubar

#  This menu option accesses a file and reads in the contents into the
#  global parameters CCDglobalpars
      $topwin.menubar addcommand Options \
         {Restore setup ....} \
         " global CCDimportexists
           global CCDimportfile
           CCDGetFileName $topwin.restore \"Read restoration file\"
           if { \$CCDimportexists } { 
              CCDReadRestoreFile \"\$CCDimportfile\" 
           }
         "

#  This menu option writes the values of the current CCDglobalpars to a
#  named file.
      $topwin.menubar addcommand Options \
         {Save setup ....} \
         " global CCDimportavail
	   global CCDimportfile
           CCDNewFileName $topwin.getname \"Save restoration file\"
           if { \"\$CCDimportavail\" != {} } {
              if { \[ CCDSaveRestoreFile \"\$CCDimportfile\" \] } {
                 CCDIssueInfo \"Saved current setup to file \$CCDimportfile\"
              }
           }
         "

#  Define help on this window.
      $topwin.menubar sethelp {On Window} ccdpack CCDSetGlobals

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#  Create a frame which will contain the parameter regions.
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      frame $topwin.base

#  Define KEEPLOG region.
      Ccd_radioarray $topwin.base.keeplog \
         -label {Keep application log:} \
         -variable {CCDglobalpars(LOGTO)}
      $topwin.base.keeplog addbutton {True}  {BOTH}
      $topwin.base.keeplog addbutton {False} {NEITHER}
      $topwin.base.keeplog sethelp sun139 CCDlogsystem

#  Define LOGFILE region.
      Ccd_labent $topwin.base.logfile \
         -text {Name of logfile:} \
         -textvariable {CCDglobalpars(LOGFILE)}
      $topwin.base.logfile sethelp sun139 CCDlogsystem

#  Define EXTENT region.
      Ccd_labent $topwin.base.extent \
         -text {Extent of useful detector area:} \
         -textvariable {CCDglobalpars(EXTENT)}
      $topwin.base.extent sethelp sun139 CCDextent

#  Define DIRECTION region.
      Ccd_radioarray $topwin.base.direction \
         -label {Readout direction:} \
         -variable {CCDglobalpars(DIRECTION)}
      $topwin.base.direction addbutton {x} {X}
      $topwin.base.direction addbutton {y} {Y}
      $topwin.base.direction sethelp sun139 CCDdirection

#  Define BOUNDS region.
      Ccd_labent $topwin.base.bounds  \
         -text {Bounds of bias strips (1 or 2 pairs):} \
         -textvariable {CCDglobalpars(BOUNDS)}
      $topwin.base.bounds sethelp sun139 CCDbounds

#  Define ADC region.
      Ccd_labent $topwin.base.adc \
         -text {Analogue-to-digital conversion factor:} \
         -textvariable {CCDglobalpars(ADC)}
      $topwin.base.adc sethelp sun139 CCDADC

#  Define RNOISE region.
      Ccd_labent $topwin.base.rnoise \
         -text {Readout noise (ADUs):} \
         -textvariable {CCDglobalpars(RNOISE)}
      $topwin.base.rnoise sethelp sun139 CCDrnoise

#  Define MASK region.
      Ccd_labent $topwin.base.mask \
         -text {Defect mask (ARD or NDF) :} \
         -textvariable {CCDglobalpars(MASK)}
      $topwin.base.mask sethelp sun139 CCDmask

#  Define DEFERRED region.
      Ccd_labent $topwin.base.deferred \
         -text {Deferred charge (usually zero):} \
         -textvariable {CCDglobalpars(DEFERRED)}
      $topwin.base.deferred sethelp sun139 CCDdeferred

#  Define SATURATE region.
      Ccd_radioarray $topwin.base.saturate \
         -label {Look for saturated pixels:} \
         -variable {CCDglobalpars(SATURATE)} \
         -standard 1
      $topwin.base.saturate sethelp sun139 CCDsaturate

#  Define SATURATION region.
      Ccd_labent $topwin.base.saturation \
         -text {Saturated pixel value (ADUs):} \
         -textvariable {CCDglobalpars(SATURATION)}
      $topwin.base.saturation sethelp sun139 CCDsaturate

#  Define SETSAT region.
      Ccd_radioarray $topwin.base.setsat\
         -label {Flag saturated pixels using BAD value:} \
         -variable {CCDglobalpars(SETSAT)} \
         -standard 1
      $topwin.base.setsat sethelp sun139 CCDsaturate

#  Define GENVAR region. Use an standard radiobutton array.
      Ccd_radioarray $topwin.base.genvar \
         -label {Generate variances:} \
          -standard 1 \
          -variable {CCDglobalpars(GENVAR)}
      $topwin.base.genvar sethelp sun139 CCDgenvar

#  Define PRESERVE region.
      Ccd_radioarray $topwin.base.preserve \
         -label {Preserve NDF data types:} \
         -standard 1 \
         -variable {CCDglobalpars(PRESERVE)}
      $topwin.base.preserve sethelp sun139 CCDpreserve

#  Pack all frame widgets with the various regions in.
      pack $topwin.base.keeplog    -fill x
      pack $topwin.base.logfile    -fill x
      pack $topwin.base.extent     -fill x
      pack $topwin.base.direction  -fill x
      pack $topwin.base.bounds     -fill x
      pack $topwin.base.adc        -fill x
      pack $topwin.base.rnoise     -fill x
      pack $topwin.base.mask       -fill x
      pack $topwin.base.deferred   -fill x
      pack $topwin.base.saturate   -fill x
      pack $topwin.base.saturation -fill x
      pack $topwin.base.setsat     -fill x
      pack $topwin.base.genvar     -fill x
      pack $topwin.base.preserve   -fill x

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#  Add a choice bar to lower section
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      Ccd_choice $topwin.choice -standard 0

#  Accept runs the CCDSETUP application and exits.
      $topwin.choice addbutton Accept \
         "CCDDoSetGlobals $topwin $topwin.base.keeplog True
         $topwin kill $topwin"
      $topwin.choice sethelp Accept ccdpack Accept

#  Reset button clear current setup and restores default. Note need
#  to set CCDglobalpars to "" before unset as this is only way to clear
#  entry widgets.
      $topwin.choice addbutton \
         {Reset} \
         "foreach element \[ array name CCDglobalpars \] {
               set CCDglobalpars(\$element) {}
            }
            unset CCDglobalpars
         "
      $topwin.choice sethelp Reset ccdpack Reset

#  Cancel button does reset and exit.
      $topwin.choice addbutton Cancel \
         " unset CCDglobalpars
           $topwin kill $topwin 
         "
      $topwin.choice sethelp Cancel ccdpack Cancel

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#  Pack the top-level widget and the main frames (these now appear).
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      pack $topwin.menubar -fill x
      pack $topwin.choice  -side bottom -fill x
      pack $topwin.base    -fill x -expand true

#  Wait for this procedure to exit, set a default focus etc.
      CCDWindowWait $topwin $topwin.base.keeplog {True}

#  End of procedure.
   }

# $Id$
