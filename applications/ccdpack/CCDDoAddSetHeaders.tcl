   proc CCDDoAddSetHeaders { Top Unsetbox Setbox args } {
#+
#  Name:
#     CCDDoAddSetHeaders

#  Purpose:
#     Do the work of adding Set headers to NDFs.

#  Language:
#     TCL

#  Type of Module:
#     Tcl/Tk procedure

#  Description:
#     This routine, invoked by CCDAddSetHeaders, calls the MAKESET task
#     to write CCDPACK Set header information to a list of NDFs.

#  Arguments:
#     Top = string
#        The command name of the top-level window controlling this
#        procedure.  On completion, that window is destroyed.
#     Unsetbox = string
#        The command name of the listbox which contains all the NDF
#        names.
#     Setbox = string
#        The command name of the listbox which contains the NDF names
#        to have Set headers written.  This is assumed to contain lines
#        in the format prepared by CCDAddSetHeaders which can be parsed
#        using CCDItemSetIndex.
#     args = list of strings
#        If present this gives a command to be executed on successful
#        completion.

#  Global Variables:
#     TASK = array (read and write)
#        Task control block.  Use TASK($taskname,error) to check status
#        of task on exit.
#     CCDaddwcs = boolean (read and write)
#        Whether a CCD_SET alignment frame should be written by MAKESET.

#  Copyright:
#     Copyright (C) 2001 Central Laboratory of the Research Councils.
#     Copyright (C) 2006 Particle Physics & Astronomy Research Council.
#     All Rights Reserved.

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
#     MBT: Mark Taylor (STARLINK)
#     PDRAPER: Peter W. Draper (STARLINK)
#     {enter_new_authors_here}

#  History:
#     18-JUN-2001 (MBT):
#        Original version.
#     01-FEB-2006 (PDRAPER):
#        Changed to use new meta-widget names (s/Ccd_/Ccd::/g).
#     {enter_further_changes_here}

#  Bugs:
#     {note_any_bugs_here}

#-

#  Global variables.
      global TASK
      global CCDaddwcs

#  Check whether all of the NDFs are setted.
      set nndfs [$Unsetbox size]
      set noset ""
      for {set i 0} {$i < $nndfs} {incr i} {
         set parsed [CCDItemSetIndex $Unsetbox $i]
         set value [lindex $parsed 0]
         set name [lindex $parsed 1]
         if { $value == "" } { lappend noset $name }
      }

#  Warn the user if they are not all setted.
      if { [llength $noset] > 0 } {
         CCDIssueInfo "Not all NDFs have been assigned a Set"

#  Remove Set headers from any not to be setted.
         set tmpfile CCDEDIT.NDFS
         CCDNameListFile $tmpfile $noset
         if { $CCDaddwcs } {
            set fixwcs "fixwcs=yes"
         } else {
            set fixwcs "fixwcs=no"
         }
         set cmd "in=^$tmpfile mode=erase name=set $fixwcs reset accept"
         CCDRunTask ccdedit $cmd 1 $Top "Removing Set headers from some files"
         catch {exec rm $tmpfile}
      }

#  Write Set header information.
      set boxsize [$Setbox size]
      set TASK(makeset,error) ""
      if { $boxsize > 1 } {

#  Get Sets from listboxes into Tcl lists.
         set nset 0
         catch {unset ndflist}
         catch {unset indexlist}
         catch {unset setsize}
         for {set i 0} {$i < $boxsize} {incr i} {
            set line [CCDItemSetIndex $Setbox $i]
            set value [lindex $line 0]
            set name [lindex $line 1]
            if { $value == "SET" } {
               incr nset
            } else {
               lappend ndflist($nset) $name
               lappend indexlist($nset) $value
            }
         }

#  Check if all Sets are the same size.
         set samesize 1
         for {set i 1} {$i <= $nset} {incr i} {
            set setsize($i) [llength $ndflist($i)]
            if { $i > 1 && $setsize([expr $i - 1]) != $setsize($i) } {
               set samesize 0
            }
         }

#  Warn if they are not.
         if { ! $samesize } {
            CCDIssueInfo "Not all Sets are the same size"
         }

#  Construct a widget to show progress.
         CCDCcdWidget Waiter waiter Ccd::toplevel $Top.waiter -title Working...
         wm withdraw $waiter
         CCDTkWidget Frame1 frame1 frame $waiter.f1
         CCDTkWidget Frame2 frame2 frame $waiter.f2
         CCDTkWidget Message message \
            label $frame2.message -text "Adding Set headers"
         global iset
         set iset 0
         CCDTkWidget Animate animate \
            scale $frame1.animate -orient horizontal \
                  -from 0 -to [expr $nset + 1] \
                  -sliderlength 8 -state disabled -showvalue 0 \
                  -takefocus 0 -variable iset
         pack $frame1 -side bottom -fill both -expand 1
         pack $frame2 -side top -fill both -expand 1 -ipadx 15
         pack $animate -fill both -expand 1
         pack $message -fill both -expand 1
         CCDCentreWindow $Waiter $Top
         wm deiconify $waiter

#  Loop over each Set invoking MAKESET once for each to add the headers.
         set tmpfile MAKESET.NDFS
         for {set iset 1} {$iset <= $nset} {incr iset} {

#  Write the NDF names to a temporary indirection file.
            CCDNameListFile $tmpfile $ndflist($iset)

#  Prepare arguments.
            set indices [join $indexlist($iset) ,]
            if { $CCDaddwcs } {
               set addwcs "addwcs=yes"
            } else {
               set addwcs "addwcs=no"
            }
            set cmd "in=^$tmpfile mode=list indices=\[$indices\] $addwcs \
                     setsize=[llength $indexlist($iset)] reset accept"

#  Invoke MAKESET.
            CCDRunTask makeset $cmd 3 $Top "Adding Set headers"
         }
         catch { exec rm $tmpfile }

#  No Set header information to add.  Warn the user.
      } else {
         CCDIssueInfo "No NDFs have been assigned a Set"
      }

#  If we have been given a command to execute on successful completion,
#  do it here.
      if { "$args" != "" && $TASK(makeset,error) == "" } {
         eval $args
      }

#  Destroy the controlling window now the work is done.
      $Top kill $Top
   }
# $Id$
