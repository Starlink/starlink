   proc CCDSetCCDGlobals { Topwin args } {

#+
#  Name:
#     CCDSetGlobals

#  Purpose:
#     Allows user to set the CCD related global parameters.

#  Language:
#     Tcl/Tk

#  Description:
#     This routine displays all the CCDPACK CCD related global
#     parameters in a top-level widget. The user may then restore the
#     values of these parameters from a CCDSETUP-like restoration file
#     or interactively modify the current values. The option to save
#     the current setup to a disk-file is also available. To exit the
#     user must select "OK".

#  Arguments:
#     Top = window (read)
#        The name of the top-level widget to contain this form.
#     args = string(s) (read)
#        Extra arguments that will be executed as a command when the
#        CCDSETUP application is run (for resetting state buttons
#        etc.). 

#  Returned Value:
#     No value is returned.

#  Global parameters:
#     CCDglobalpars = array (write)
#        The global parameters selected by the user are return in this
#	 array which is indexed by the name of the CCDPACK global
#	 parameters (i.e. CCDglobalpars(ADC) is the ADC factor).

#  Authors:
#     PDRAPER: Peter Draper (STARLINK - Durham University)
#     MBT: Mark Taylor (STARLINK)
#     {enter_new_authors_here}

#  History:
#     22-FEB-1994 (PDRAPER):
#     	 Original version.
#     21-APR-1994 (PDRAPER):
#        Changed to use mega-widgets.
#     22-MAR-1995 (PDRAPER):
#        Added help system.
#     25-MAY-1995 (PDRAPER):
#        Stripped down to CCD related parameters only.
#     17-AUG-1995 (PDRAPER):
#        Recoded to new conventions.
#     29-SEP-1995 (PDRAPER):
#        Added CCD geometry stuff.
#     19-OCT-1995 (PDRAPER):
#        Added bias level entry. This was seen a reduce option 
#        but not available here.
#     16-MAY-2000 (MBT):
#        Upgraded for Tcl8.
#     {enter_changes_here}

#-


#  Global parameters:
      global CCDglobalpars

#  Local constants:
      set params "EXTENT DIRECTION BOUNDS ADC RNOISE MASK DEFERRED SATURATION ZERO"

#.

#------------------------------------------------------------------------------
#  Widget Creation
#------------------------------------------------------------------------------

#  Top-level widget.
      CCDCcdWidget Top top \
         Ccd_toplevel $Topwin -title "CCD characteristic parameters"

#  Menubar.
      CCDCcdWidget Menu menu Ccd_helpmenubar $Top.menubar

#  Frame for containing the parameter regions.
      CCDTkWidget Frame frame frame $top.center

#  Labelled entry for EXTENT value.
      CCDCcdWidget Extent extent \
         Ccd_labent $Frame.extent \
                     -text "Extent of useful detector area:" \
                     -textvariable CCDglobalpars(EXTENT)

#  Radioarray for DIRECTION value.
      CCDCcdWidget Direct direct \
         Ccd_radioarray $Frame.direction \
                     -label "Readout direction:" \
                     -variable CCDglobalpars(DIRECTION)

#  Labelled entry for BOUNDS value.
      CCDCcdWidget Bounds bounds \
         Ccd_labent $Frame.bounds \
                     -text "Bounds of bias strips (1 or 2 pairs):" \
                     -textvariable CCDglobalpars(BOUNDS)

#  Labelled entry for ADC value.
      CCDCcdWidget Adc adc \
         Ccd_labent $Frame.adc \
                  -text "Analogue-to-digital conversion factor:" \
                  -textvariable CCDglobalpars(ADC)

#  Labelled entry for RNOISE value.
      CCDCcdWidget Rnoise rnoise \
         Ccd_labent $Frame.rnoise \
                     -text "Readout noise (ADUs):" \
                     -textvariable CCDglobalpars(RNOISE)

#  Labelled entry for MASK value.
      CCDCcdWidget Mask mask \
         Ccd_labent $Frame.mask \
                   -text "Defect mask:" \
                   -textvariable CCDglobalpars(MASK)

#  Labelled entry for DEFERRED value.
      CCDCcdWidget Deferred deferred \
         Ccd_labent $Frame.deferred \
                       -text "Deferred charge (usually zero):" \
                       -textvariable CCDglobalpars(DEFERRED)

#  Labelled entry for SATURATION value.
      CCDCcdWidget Satur satur \
         Ccd_labent $Frame.saturation \
                    -text "Saturated pixel value (ADUs):" \
                    -textvariable CCDglobalpars(SATURATION)

#  Labelled entry for bias level.
      CCDCcdWidget Zero zero \
         Ccd_labent $Frame.bias \
                   -text "Bias level (ADUs):" \
                   -textvariable CCDglobalpars(ZERO)

#  Choice bar for OK etc.
      CCDCcdWidget Choice choice Ccd_choice $Top.choice -standard 0

#------------------------------------------------------------------------------
#  Widget configuration
#------------------------------------------------------------------------------
#  File items to accept window and exit interface.
      $Menu addcommand File {Accept Window} "$Choice invoke OK"
      $Menu addcommand File {Exit} CCDExit

#  Add menu option to access a file and reads in the contents into the
#  global parameters CCDglobalpars
      $Menu addcommand Options \
         {Restore setup ....} \
         " global CCDimportexists
           global CCDimportfile
           global CCDimportfilter
           set CCDimportfilter \"*.DAT\"
           CCDGetFileName $Top.restore \"Read restoration file\"
           if { \$CCDimportexists } { 
              CCDReadRestoreFile \"\$CCDimportfile\" 
           }
         "

#  Add menu option to write the values of the current CCDglobalpars to a
#  named file.
      $Menu addcommand Options \
         {Save setup ....} \
         " global CCDimportavail
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

#  Add menu option to display and/or define CCD geometry.
      $Menu addcommand Options {Display/define CCD geometry...} \
         "CCDGeometry $Top.geom"

#  Add menu option to select MASK from existing files.
      $Menu addcommand Options \
         {Select MASK from existing files ....} \
         " global CCDimportexists
           global CCDimportfile
           CCDGetFileName $Top.restore \"Select MASK (NDF or ARD file)\"
           if { \$CCDimportexists } { 
              $Mask clear 0 end
              $Mask insert 0 \"\$CCDimportfile\" 
           }
         "

#  Add buttons showing available directions.
      $Direct addbutton {x} {X}
      $Direct addbutton {y} {Y}


#  Add OK to choice bar. This runs the CCDSETUP application and exits.
      $Choice addbutton OK \
         "global CCDglobalpars
          if { ! \[info exists CCDglobalpars(MASK)\] } { 
             set CCDglobalpars(MASK) \"!\"
          } elseif { \$CCDglobalpars(MASK) == \"\" } {
             set CCDglobalpars(MASK) \"!\"
          }
          if { \[CCDDoSetGlobals $Top CCD\] } {
             eval $args
             $Top kill $Top
          }
         "

#  Reset button clears the  current setup and restores default. Note need
#  to set CCDglobalpars to "" before unset as this is only way to clear
#  entry widgets.
      $Choice addbutton Reset \
         "foreach element \"$params\" {
             set CCDglobalpars(\$element) {}
             unset CCDglobalpars(\$element)
          }
         "
#------------------------------------------------------------------------------
#  All widget and subparts created so add help.
#------------------------------------------------------------------------------
      $Top sethelp ccdpack CCDSetCCDGlobalsWindow
      $Menu sethelpitem {On Window} ccdpack CCDSetCCDGlobalsWindow
      $Menu sethelp all ccdpack CCDSetCCDGlobalsMenu
      $Adc sethelp sun139 CCDADC
      $Bounds sethelp sun139 CCDbounds
      $Deferred sethelp sun139 CCDdeferred
      $Direct sethelp sun139 CCDdirection
      $Extent sethelp sun139 CCDextent
      $Mask sethelp sun139 datamasks
      $Rnoise sethelp sun139 CCDrnoise
      $Satur sethelp sun139 CCDsaturate
      $Zero sethelp sun139 CCDbiaslevel

#------------------------------------------------------------------------------
#  Pack all widgets.
#------------------------------------------------------------------------------
      pack $extent    -fill x
      pack $direct    -fill x
      pack $bounds    -fill x
      pack $adc       -fill x
      pack $rnoise    -fill x
      pack $mask      -fill x
      pack $deferred  -fill x
      pack $satur     -fill x
      pack $zero      -fill x
      pack $menu      -fill x
      pack $choice    -side bottom -fill x
      pack $frame     -fill x -expand true

#  Wait for this procedure to exit.
      CCDWindowWait $Top

#  End of procedure.
   }

# $Id$
