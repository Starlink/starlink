proc CCDNDFOrganize { Topwin args } {
#+
#  Name:
#     CCDNDFOrganize

#  Purpose:
#     Organizes NDFs into colour and type related lists.

#  Language:
#     TCL

#  Type of Module:
#     Tcl/Tk procedure.

#  Description:
#     This routine creates a top-level window with options to describe
#     the types of NDFs which are to be "imported" into the system. The
#     primary window has options for saying whether or not different
#     filters types are present and whether or not any dark or flash
#     NDFs have different exposure times to the target data. If data
#     colours differ then windows can be presented for getting separate
#     lists of "target" and "flatfield" data for each filter type. If
#     dark or flash NDFs have different exposure times then they (and
#     other likely NDFs) can be associated with exposure factor(s).
#
#     On exit a further window is created which reflects the chosen
#     options and allows the actual names of the NDFs (and any
#     required factors) to be entered. This window is destroyed at
#     this point and can only be re-created by another CCDNDFOrganize call.

#  Arguments:
#     Topwin = window (read)
#        The top-level window name.
#     args = list (read)
#        If present this should be a command to run if the NDF import
#        runs successfully (such as enabling commands for the next section).

#  Global Variables:
#     CCDhaveframe = array (write)
#        This is a boolean array which has flags indicating which of the
#        available frame types have been imported into the system. The
#        indices of this array are:
#            (targets)
#            (flatfields)
#            (biases)
#            (darks)
#            (flashes)
#            (master_biases)
#            (master_flashes)
#            (master_darks)
#            (master_flatfields)
#     CCDsame = array (write)
#        This array indicates the current answer to the questions about
#        the presence of different filters, and whether or not the darks
#        and flashes have different exposure times. The indices are:
#            (filter)
#            (darks)
#            (flashes)
#        The values are either 1 (true) or 0 (false).
#     CCDfilternames = string (write)
#        The names of the known filters. These are separated by
#         whitespace. If none are given this is set to blank rather
#        than remaining unset.
#     CCDndfs = array (write)
#        The names of the NDFs. This array is indexed by the frame types
#         ("targets", "flatfields", "biases", "darks" and "flashes" ) and
#         also by the filter type if used (this is the second index).
#     CCDirflats = booleans (read and write)
#        If targets are to be used as flats, if none of the correct colour
#        are available.

#  Copyright:
#     Copyright (C) 1994 Science & Engineering Research Council.
#     Copyright (C) 1995, 2000 Central Laboratory of the Research
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
#     3-MAY-1994 (PDRAPER):
#        Original version.
#     24-MAY-1994 (PDRAPER):
#        Removed control of NDF input from this form. Added required
#        controls for exposure times in all frames.
#     23-MAR-1995 (PDRAPER):
#        Added help (menu and context).
#     1-JUN-1995 (PDRAPER):
#        Removed built-in keyboard traversal, now provided by Tk.
#     13-NOV-1995 (PDRAPER):
#        Added support for masters and using targets as possible
#        flatfields (IR usage).
#     16-MAY-2000 (MBT):
#        Upgraded for Tcl8.
#     1-JAN-2006 (PDRAPER):
#        Changed to use new meta-widget names (s/Ccd_/Ccd::/g).
#     {enter_changes_here}

#  Bugs:
#     {note_any_bugs_here}

#-

#  global variables:
   global CCDhaveframe
   global CCDsame
   global CCDfilternames
   global CCDndfs
   global CCDirflats
   global CCDglobalpars
   global buttonstate         ;# Local global variable for controlling
                               # state of buttons.
#.

#------------------------------------------------------------------------------
#  Initialisation.
#------------------------------------------------------------------------------
#  Initialise the various global arrays if they do not exists already
   if { ![info exists CCDhaveframe(targets)] } {
      set CCDhaveframe(targets) 1 }
   if { ![info exists CCDhaveframe(flatfields)] } {
      set CCDhaveframe(flatfields) 1 }
   if { ![info exists CCDhaveframe(biases)] } {
      set CCDhaveframe(biases) 0 }
   if { ![info exists CCDhaveframe(darks)] } {
      set CCDhaveframe(darks) 0 }
   if { ![info exists CCDhaveframe(flashes)] } {
      set CCDhaveframe(flashes) 0 }
   if { ![info exists CCDhaveframe(master_biases)] } {
      set CCDhaveframe(master_biases) 0 }
   if { ![info exists CCDhaveframe(master_flats)] } {
      set CCDhaveframe(master_flats) 0 }
   if { ![info exists CCDhaveframe(master_flashes)] } {
      set CCDhaveframe(master_flashes) 0 }
   if { ![info exists CCDhaveframe(master_darks)] } {
      set CCDhaveframe(master_darks) 0 }

#  If undecided assume filters are not used.
   if { [info exists CCDsame(filter)] } {
      if { $CCDsame(filter) } {
         set firststate disabled
      } else {
         set firststate normal
      }
   } else {
      set firststate disabled
      set CCDsame(filter) 1
   }

#  Set dark and flash time to be same by default.
   if { ![info exists CCDsame(darks)] } {
      set CCDsame(darks) 1
   }
   if { ![info exists CCDsame(flashes)] } {
      set CCDsame(flashes) 1
   }

#  And do not use TARGETs as flatfields.
   if { ![info exists CCDirflats] } {
      set CCDirflats FALSE
   }

#  Master bias isn't zeroed (this only work after last call!).
   if { ![info exists CCDglobalpars(ZEROED)] } {
      set CCDglobalpars(ZEROED) FALSE
   }

#------------------------------------------------------------------------------
#  Create widgets and associate any help.
#------------------------------------------------------------------------------
#  Create top-level object.
   CCDCcdWidget Top top Ccd::toplevel $Topwin -title "Organize NDFs"
   wm withdraw $top

#  Add standard menubar.
   CCDCcdWidget Menu menu Ccd::helpmenubar $Top.menu -standard 0

#  Create frame for central part.
   CCDTkWidget Centre centre frame $top.centre

#  Array of checkbuttons for indicating frames types.
   CCDCcdWidget Frames frames \
      Ccd::checkarray $centre.ftypes -label "Frame types present:"

#  Boolean for indicating more than one filter.
   CCDCcdWidget Filter filter \
      Ccd::radioarray $Centre.filter \
                  -standard 1 \
                  -label "Data have same filter type:" \
                  -variable CCDsame(filter)

#  Names of filters if used.
   CCDCcdWidget Filternames filternames \
      Ccd::labent $Centre.filters \
                       -text "Filter names:" \
                       -textvariable CCDfilternames

#  Boolean for use targets as flatfields (uses adam TRUE and FALSE).
   CCDCcdWidget Irflats irflats \
      Ccd::radioarray $Centre.irflats \
                   -standard 1 \
                   -adampars 1 \
                   -label "Use targets as possible flatfields:" \
                   -variable CCDirflats

#  Boolean for master bias state (zeroed or not).
   CCDCcdWidget Mbiasstate mbiasstate \
      Ccd::radioarray $Centre.mbiasstate \
                   -standard 1 \
                   -adampars 1 \
                   -label "Master bias has mean of zero:" \
                   -variable CCDglobalpars(ZEROED)

#  Boolean for dark times same.
   CCDCcdWidget Darks darks \
      Ccd::radioarray $Centre.darkexpose \
                 -standard 1 \
                 -label "Dark times are all the same:" \
                 -variable CCDsame(darks)

#  Boolean for flash times same.
   CCDCcdWidget Flashes flashes \
      Ccd::radioarray $Centre.flashexpose \
                   -standard 1 \
                   -label "Flash times are all the same:" \
                   -variable CCDsame(flashes)

#  Choice for final action.
   CCDCcdWidget Choice choice Ccd::choice $Top.choice

#------------------------------------------------------------------------------
#  Configure widgets.
#------------------------------------------------------------------------------
#  File items to cancel or accept window and exit interface.
   $Menu addbutton File 0
   $Menu addcommand File {Close Window} "$Choice invoke Cancel"
   $Menu addcommand File {Accept Window} "$Choice invoke OK"
   $Menu addcommand File {Exit} CCDExit

#  Add buttons to indicate the frame types.
   $Frames addbutton {targets}    -variable CCDhaveframe(targets)
   $Frames addbutton {flatfields} -variable CCDhaveframe(flatfields)
   $Frames addbutton {biases}     -variable CCDhaveframe(biases)
   $Frames addbutton {darks}      -variable CCDhaveframe(darks)
   $Frames addbutton {flashes}    -variable CCDhaveframe(flashes)
   $Frames addbutton {master_bias}  -variable CCDhaveframe(master_biases)
   $Frames addbutton {master_flats} -variable CCDhaveframe(master_flats)
   $Frames addbutton {master_dark}  -variable CCDhaveframe(master_darks)
   $Frames addbutton {master_flash} -variable CCDhaveframe(master_flashes)

#  No darks disable dark time comment.
   $Frames addcommand darks \
      "global CCDhaveframe
       global CCDsame
       if { ! \$CCDhaveframe(darks) } {
          $Darks state all disabled
          set CCDsame(darks) 1
       } else {
          $Darks state all normal
       }
      "
   if { ! $CCDhaveframe(darks) } {
      $Darks state all disabled
   }

#  No flashes disable flash time comment.
   $Frames addcommand flashes \
      "global CCDhaveframe
       global CCDsame
       if { ! \$CCDhaveframe(flashes) } {
          $Flashes state all disabled
          set CCDsame(flashes) 1
       } else {
          $Flashes state all normal
       }
      "
   if { ! $CCDhaveframe(flashes) } {
      $Flashes state all disabled
   }

#  No filters enables irflats.
   $Frames addcommand flatfields \
      "global CCDhaveframe(flatfields)
       global CCDirflats
       global CCDsame
       if { \$CCDhaveframe(flatfields) && \$CCDsame(filter) } {
          set CCDirflats FALSE
          $Irflats state all disabled
       } else {
          $Irflats state all normal
       }

      "

#  Set irflats initial state.
   if { $CCDhaveframe(flatfields) && $CCDsame(filter) } {
      set CCDirflats FALSE
      $Irflats state all disabled
   } else {
      $Irflats state all normal
   }

#  Add commands to control state of filter names input and whether we're
#  allowing irflats (this is also bound to state of flatfields).
   $Filter addcommand true \
      "$Filternames clear 0 end
       $Filternames configure -state disabled
       global CCDhaveframe(flatfields)
       global CCDirflats
       if { \$CCDhaveframe(flatfields) } {
          set CCDirflats FALSE
          $Irflats state all disabled
       }
      "
   $Filter addcommand false \
      "$Filternames configure -state normal
       $Irflats state all normal
      "

#  and set its initial state.
   $Filternames configure -state $firststate

#  Master bias is zeroed state.
   $Frames addcommand master_bias \
      "global CCDhaveframe
       if { ! \$CCDhaveframe(master_biases) } {
          $Mbiasstate state all disabled
       } else {
          $Mbiasstate state all normal
       }
      "
   if { ! $CCDhaveframe(master_biases) } {
      $Mbiasstate state all disabled
   }

#  Choice when all is completed. Check that filternames have
#  been given if CCDsame(filter) is 0. Assign the filter to NONE if
#  this is the case.
   $Choice addcommand {OK} \
      "global CCDsame
       global CCDfilternames
       global CCDhaveframe
       if { \$CCDhaveframe(targets) || \$CCDhaveframe(flatfields) ||
            \$CCDhaveframe(biases) || \$CCDhaveframe(darks) ||
            \$CCDhaveframe(flashes) || \$CCDhaveframe(master_biases) ||
            \$CCDhaveframe(master_flats) || \$CCDhaveframe(master_darks) ||
            \$CCDhaveframe(master_flashes) } {
          if \$CCDsame(filter) {
             set CCDfilternames NONE
          }
          $Top kill $Top
          CCDNDFDoImport $Top $args
       } else {
          CCDIssueInfo {Need at least one frame type}
       }
      "

#  Cancel removes all the globals.
   $Choice addcommand {Cancel} \
      "global CCDsame
       global CCDhaveframe
       global CCDfilternames
       global CCDndfs
       global CCDirflats
       catch { unset CCDsame }
       catch { unset CCDhaveframe }
       catch { unset CCDfilternames }
       catch { unset CCDndfs }
       set CCDirflats FALSE
       $Top kill $Top
      "

#------------------------------------------------------------------------------
#  Associate any help with the window.
#------------------------------------------------------------------------------
   $Top sethelp ccdpack CCDNDFOrganizeWindow
   $Menu sethelpitem {On Window} ccdpack CCDNDFOrganizeWindow
   $Menu sethelp File ccdpack OnFile

#------------------------------------------------------------------------------
#  Pack all widgets.
#------------------------------------------------------------------------------
   pack $menu -fill x
   pack $choice -side bottom -fill x
   pack $centre -fill both
   pack $frames -fill both
   pack $filter -fill x
   pack $filternames -fill x
   pack $irflats -fill x
   pack $mbiasstate -fill x
   pack $darks -fill x
   pack $flashes -fill x
   wm deiconify $top

#------------------------------------------------------------------------------
#  Wait for interaction to finish.
#------------------------------------------------------------------------------
   CCDWindowWait $Top

#  End of procedure.
}

# $Id$
