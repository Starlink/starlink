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

#  Global Parameters:
#     CCDglobalpars = array (write)
#        The global parameters selected by the user are return in this
#        array which is indexed by the name of the CCDPACK global
#        parameters (i.e. CCDglobalpars(ADC) is the ADC factor).
#     CCDgloprefix = string (read and write)
#        Keeps track of the Set Index to which the currently displayed
#        entry panel refers.  If Sets are in use it will be the current
#        Set Index followed by a comma, otherwise it will be the empty
#        string.  It is used to index the elements of CCDglobalpars;
#        e.g. the ADC value corresponding to the panel currently
#        displayed can be accessed as $CCDglobalpars(${CCDgloprefix}ADC).
#     CCDsetindices = list of integers (read and write)
#        The NDF Set Index values that we know about.

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
#     22-JUN-2001 (MBT):
#        Upgraded for Sets.
#     1-JAN-2006 (PDRAPER):
#        Changed to use new meta-widget names (s/Ccd_/Ccd::/g).
#     {enter_changes_here}

#  Bugs:
#     {note_any_bugs_here}

#-


#  Global parameters:
      global CCDgloprefix
      global CCDglobalpars
      global CCDsetindices

#  Local constants:
      set params \
         "EXTENT DIRECTION BOUNDS ADC RNOISE MASK DEFERRED SATURATION ZERO"

#.

#  If we are using CCDPACK Sets, work out how many different Set Index
#  values we have (hence how many panels are required to get values).
      if { $CCDglobalpars(USESET) == "TRUE" } {
         CCDGetSetIndices $Topwin 1
         if { $CCDglobalpars(USESET) == "TRUE" } {
            set useset [expr [llength $CCDsetindices] > 1]
         } else {
            set useset 0
         }
      } else {
         set useset 0
      }


#------------------------------------------------------------------------------
#  Widget Creation
#------------------------------------------------------------------------------

#  Top-level widget.
      CCDCcdWidget Top top \
         Ccd::toplevel $Topwin -title "CCD characteristic parameters"

#  Menubar.
      CCDCcdWidget Menu menu Ccd::helpmenubar $Top.menubar

#  Frame for containing the parameter regions.
      CCDTkWidget Frame frame frame $top.center

#  Buttons for selecting Set Index.  If we are using Sets, then allow
#  for selecting a different window for each one.  Otherwise, just
#  create a dummy frame here which will have no effect.
      if { $useset } {
         CCDCcdWidget Setswitch setswitch \
            Ccd::reveal $Frame.switch \
                       -placebar top -stack array -columns 4 -in $Frame.switch
         set setindices $CCDsetindices
      } else {
         CCDTkWidget Setswitch setswitch \
            frame $frame.dummy -borderwidth 0
         set setindices {0}
      }

#  Loop over all the entry panels to be constructed.  If we are not using
#  Sets, there will only be one of these.
      foreach sindex $setindices {

#  Frame to contain all the entry widgets.
         CCDTkWidget Panel($sindex) panel($sindex) \
            frame $setswitch.panel$sindex

#  If using Sets, add a selection button for this panel (if not using Sets,
#  this is the only panel which will be revealed anyway).
         if { $useset } {
            $Setswitch addbutton "Set Index $sindex" $Panel($sindex)
            set prefix "$sindex,"
         } else {
            set prefix ""
         }

#  Labelled entry for EXTENT value.
         CCDCcdWidget Extent($sindex) extent($sindex) \
            Ccd::labent $Panel($sindex).extent$sindex \
                     -text "Extent of useful detector area:" \
                     -textvariable CCDglobalpars(${prefix}EXTENT)

#  Radioarray for DIRECTION value.
         CCDCcdWidget Direct($sindex) direct($sindex) \
            Ccd::radioarray $Panel($sindex).direction$sindex \
                     -label "Readout direction:" \
                     -variable CCDglobalpars(${prefix}DIRECTION)

#  Labelled entry for BOUNDS value.
         CCDCcdWidget Bounds($sindex) bounds($sindex) \
            Ccd::labent $Panel($sindex).bounds$sindex \
                        -text "Bounds of bias strips (1 or 2 pairs):" \
                        -textvariable CCDglobalpars(${prefix}BOUNDS)

#  Labelled entry for ADC value.
         CCDCcdWidget Adc($sindex) adc($sindex) \
            Ccd::labent $Panel($sindex).adc$sindex \
                     -text "Analogue-to-digital conversion factor:" \
                     -textvariable CCDglobalpars(${prefix}ADC)

#  Labelled entry for RNOISE value.
         CCDCcdWidget Rnoise($sindex) rnoise($sindex) \
            Ccd::labent $Panel($sindex).rnoise$sindex \
                        -text "Readout noise (ADUs):" \
                        -textvariable CCDglobalpars(${prefix}RNOISE)

#  Labelled entry for MASK value.
         CCDCcdWidget Mask($sindex) mask($sindex) \
            Ccd::labent $Panel($sindex).mask$sindex \
                      -text "Defect mask:" \
                      -textvariable CCDglobalpars(${prefix}MASK)

#  Labelled entry for DEFERRED value.
         CCDCcdWidget Deferred($sindex) deferred($sindex) \
            Ccd::labent $Panel($sindex).deferred$sindex \
                          -text "Deferred charge (usually zero):" \
                          -textvariable CCDglobalpars(${prefix}DEFERRED)

#  Labelled entry for SATURATION value.
         CCDCcdWidget Satur($sindex) satur($sindex) \
            Ccd::labent $Panel($sindex).saturation$sindex \
                       -text "Saturated pixel value (ADUs):" \
                       -textvariable CCDglobalpars(${prefix}SATURATION)

#  Labelled entry for bias level.
         CCDCcdWidget Zero($sindex) zero($sindex) \
            Ccd::labent $Panel($sindex).bias$sindex \
                      -text "Bias level (ADUs):" \
                      -textvariable CCDglobalpars(${prefix}ZERO)
      }

#  Choice bar for OK etc.
      CCDCcdWidget Choice choice \
         Ccd::choice $Top.choice \
                    -standard 0

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
           CCDGetFileName $Top.restore \"Read restoration file\" 0
           if { \$CCDimportexists } {
              CCDReadRestoreFile \"\$CCDimportfile\"
              $Choice invoke OK
              CCDIssueInfo \"Parameters restored from file \$CCDimportfile\"
           } else {
              CCDIssueInfo \"File not found: \$CCDimportfile\"
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

#  Add bindings for an expose of each of the Set Index-specific panels.
#  Then when any of them is exposed, it will fill in blank fields
#  from the corresponding fields on panels which have already been
#  filled in.  This means you don't have to type the same thing for
#  each Set Index if they have the same values.
      if { $useset } {
         foreach sindex $CCDsetindices {
            bind $panel($sindex) <Expose> "
               global CCDgloprefix
               global CCDglobalpars
               set CCDgloprefix {$sindex,}
               foreach param {$params} {
                  if { \$CCDglobalpars($sindex,\$param) == \"\" } {
                     foreach sjndex \"$CCDsetindices\" {
                        if { \$CCDglobalpars(\$sjndex,\$param) != \"\" } {
                           set CCDglobalpars($sindex,\$param) \\
                               \$CCDglobalpars(\$sjndex,\$param)
                        }
                     }
                  }
               }
            "
         }

#  No Sets: set the global prefix to the empty string.
      } else {
         set CCDgloprefix {}
      }


#  Add menu option to select MASK from existing files.
      $Menu addcommand Options \
         {Select MASK from existing files ....} \
         " global CCDgloprefix
           global CCDimportexists
           global CCDimportfile
           CCDGetFileName $Top.restore \"Select MASK (NDF or ARD file)\" 1
           if { \$CCDimportexists } {
              set CCDglobalpars(\${CCDgloprefix}MASK) \"\$CCDimportfile\"
           }
         "

#  Add buttons showing available directions.
      foreach sindex $setindices {
         $Direct($sindex) addbutton {x} {X}
         $Direct($sindex) addbutton {y} {Y}
      }

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
#  to set CCDglobalpars to "" as this is only way to clear entry widgets.
      if { $useset } {
         set resetcmd \
            "foreach element \"$params\" {
                set CCDglobalpars(\$element) {}
                foreach sindex \$CCDsetindices {
                   set CCDglobalpars(\$sindex,\$element) {}
                }
             }
            "
      } else {
         set resetcmd \
            "foreach element \"$params\" {
                set CCDglobalpars(\$element) {}
             }
            "
      }
      $Choice addbutton Reset $resetcmd

#------------------------------------------------------------------------------
#  All widget and subparts created so add help.
#------------------------------------------------------------------------------
      $Top sethelp ccdpack CCDSetCCDGlobalsWindow
      $Menu sethelpitem {On Window} ccdpack CCDSetCCDGlobalsWindow
      $Menu sethelp all ccdpack CCDSetCCDGlobalsMenu
      foreach sindex $setindices {
         $Adc($sindex) sethelp sun139 CCDADC
         $Bounds($sindex) sethelp sun139 CCDbounds
         $Deferred($sindex) sethelp sun139 CCDdeferred
         $Direct($sindex) sethelp sun139 CCDdirection
         $Extent($sindex) sethelp sun139 CCDextent
         $Mask($sindex) sethelp sun139 datamasks
         $Rnoise($sindex) sethelp sun139 CCDrnoise
         $Satur($sindex) sethelp sun139 CCDsaturate
         $Zero($sindex) sethelp sun139 CCDbiaslevel
      }

#------------------------------------------------------------------------------
#  Pack all widgets.
#------------------------------------------------------------------------------
      foreach sindex $setindices {
         pack $extent($sindex)    -fill x
         pack $direct($sindex)    -fill x
         pack $bounds($sindex)    -fill x
         pack $adc($sindex)       -fill x
         pack $rnoise($sindex)    -fill x
         pack $mask($sindex)      -fill x
         pack $deferred($sindex)  -fill x
         pack $satur($sindex)     -fill x
         pack $zero($sindex)      -fill x
      }
      pack $menu      -fill x
      pack $choice    -side bottom -fill x
      pack $setswitch -fill both
      pack $frame     -fill x -expand true
      if { $useset } {
         $Setswitch invoke "Set Index [lindex $CCDsetindices 0]"
      } else {
         pack $panel(0)  -fill x
      }

#  Wait for this procedure to exit.
      CCDWindowWait $Top

#  End of procedure.
   }

# $Id$
