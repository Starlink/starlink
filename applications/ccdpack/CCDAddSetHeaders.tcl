   proc CCDAddSetHeaders { Topwin args } {
#+
#  Name:
#     CCDAddSetHeaders

#  Purpose:
#     Allows user to group NDFs into Sets and adds CCDPACK Set headers.

#  Language:
#     TCL

#  Type of Module:
#     Tcl/Tk procedure

#  Description:
#     This routine creates a top-level window which presents a list
#     of all the NDFs currently known to the system and allows the
#     user to group them into CCDPACK Sets using various methods.
#
#     When the user has assigned Set membership to some or all of
#     the NDFs and indicates that the selection is to be accepted,
#     the NDFs will be edited to include the Set headers as required.

#  Arguments:
#     Topwin = window (read)
#        The top-level window name.
#     args = list (read)
#        If present this should be a command to run if the NDF import
#        runs successfully (such as enabling commands for the next section).

#  Global Variables:
#     CCDaddwcs = boolean (read and write)
#        Whether a CCD_SET alignment frame should be written by MAKESET.
#     CCDallndfs = list (read and write)
#        A list of all the names of NDFs which have been imported.
#     CCDndfcontainers = array (read)
#        An array mapping every NDF structure so far encountered to its
#        HDS container file.
#     CCDsetindices = list of integers
#        The NDF Set Index values that we know about.
#     CCDsetindicesvalid = boolean
#        True if CCDsetindices represents a user-selected value.

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
#     14-JUN-2001 (MBT):
#        Original version.
#     01-FEB-2006 (PDRAPER):
#        Changed to use new meta-widget names (s/Ccd_/Ccd::/g).
#     {enter_further_changes_here}

#  Bugs:
#     {note_any_bugs_here}

#-

#  Global Variables:
      global CCDallndfs
      global CCDndfcontainers
      global CCDaddwcs
      global CCDsetindices
      global CCDsetindicesvalid

#  Check that there is some data.
      set exists 0
      if { ! [info exists CCDallndfs] || [llength $CCDallndfs] == 0 } {
         CCDIssueInfo \
            "No frames have been selected,\nimport some into the system"
         return
      }

#------------------------------------------------------------------------------
#  Widget creation.
#------------------------------------------------------------------------------
#  Create top-level object.
      CCDCcdWidget Top top Ccd::toplevel $Topwin -title "Group NDFs by Set"
      wm withdraw $top

#  Add a standard menu bar.
      CCDCcdWidget Menu menu Ccd::helpmenubar $Top.menu

#  Create frame for central part.
      CCDTkWidget Centre centre frame $top.centre

#  Create frame for part with setted and unsetted listboxes.
      CCDTkWidget Fileboxes fileboxes frame $centre.fileboxes

#  Create listbox for unSetted NDFs.
      CCDCcdWidget Unsetbox unsetbox \
         Ccd::scrollbox $Fileboxes.unset -label "All NDFs" -singleselect 0

#  Create frame for Setted NDFs.
      CCDCcdWidget Setbox setbox \
         Ccd::scrollbox $Fileboxes.set -label "NDFs in Sets" -singleselect 0

#  Create panel for buttons controlling manual transfer between listboxes.
      CCDCcdWidget Transfer transfer \
         Ccd::choice $Fileboxes.transfer -standard 0 -stack vertical

#  Create panel for automatic Setting method buttons.
      CCDCcdWidget Autoset autoset \
         Ccd::choice $Centre.autoset -standard 0

#  Create panel for widget control.
      CCDCcdWidget Control control \
         Ccd::choice $Top.control -standard 0

#------------------------------------------------------------------------------
#  Pack widgets.
#------------------------------------------------------------------------------
      pack $menu -side top -fill x
      pack $centre -side top -fill both -expand 1
      pack $fileboxes -fill both -expand 1
      pack $unsetbox -side left -fill both -expand 1
      pack $transfer -side left -fill both
      pack $setbox -side right -fill both -expand 1
      pack $control -side bottom -fill x
      pack $autoset -side bottom -fill x

#------------------------------------------------------------------------------
#  Widget configuration.
#------------------------------------------------------------------------------

#  Menu configuration.
      $Menu addcommand File {Close Window} "$Control invoke Cancel"
      $Menu addcommand File {Accept Window} "$Control invoke OK"
      $Menu addcommand File {Exit} CCDExit
      $Menu addcommand Options {Configure Set size...} \
         "CCDGetSetIndices $Top.getsize 1"
      $Menu addcheckbutton Options {Write CCD_SET alignment coordinate frame} \
         -variable CCDaddwcs

#  Add buttons to choice widgets.
      set newsetbutton "New Set\n  ->  "
      set appendbutton "Append\n  ->  "
      set unsetbutton "De-Set\n  <-  "
      $Transfer addbutton $newsetbutton
      $Transfer addbutton $appendbutton
      $Transfer addbutton $unsetbutton
      $Autoset addbutton "By Container"
      $Autoset addbutton "By Order"
      $Autoset addbutton "Reset"
      $Control addbutton "Cancel"
      $Control addbutton "OK"

#  We must post the widget now so that the waiting notification window
#  posted by the SHOWSET invocation can position itself properly.
      wm deiconify $top

#  Run SHOWSET to determine the Set header content of all the NDFs which
#  we know about.  This has to be done before the button bindings since
#  we need to know how many NDFs there are.
      set tmpfile1 SHOWSET.NDFS
      set tmpfile2 SHOWSET.OUT
      CCDNameListFile $tmpfile1 $CCDallndfs
      set cmd "in=^$tmpfile1 listby=none namelist=$tmpfile2 \
               pickindex=all pickname=all setless=false reset accept"
      CCDRunTask showset $cmd 1 $Topwin "Examining existing Set headers"

#  Read the SHOWSET output list file to find the Set header information for
#  each NDF which has some.
      catch {unset setindex}
      catch {unset setname}
      catch {unset ndfexist}
      if { ! [catch { open $tmpfile2 } fileid] } {
         while { [gets $fileid line] > -1 } {
            if { [regexp {^ *([^ ]+) *# *(-?[0-9]+) *([^ ]*) *$} \
                  $line dummy ndf sindex sname] } {
               if { [array names ndfexist $ndf] == "" } {
                  set setindex($ndf) $sindex
                  lappend setname($sname) $ndf
                  set ndfexist($ndf) 1
               }
            }
         }
         close $fileid

#  If we fail to open the file which SHOWSET has just written, it means
#  information about existing Set headers will not be used.  This
#  shouldn't happen, but if it does it's not too harmful - take no
#  action.
      } else {
      }

#  Remove the temporary files.
      catch {exec rm $tmpfile1 $tmpfile2}

#  Go through each NDF in the list adding it to the unsetted box.
#  If there are any duplicates in the list, don't add them twice.
#  Also set the value of the nndfs variable to hold the size of the
#  unsetted list.
      catch {unset ndfexist}
      set nndfs 0
      foreach ndf $CCDallndfs {
         if { [array names ndfexist $ndf] == "" } {
            if { [array names setindex $ndf] == "$ndf" } {
               set sindex $setindex($ndf)
            } else {
               set sindex ""
            }
            $Unsetbox insert end $ndf
            CCDItemSetIndex $Unsetbox end $sindex
            incr nndfs
         }
         set ndfexist($ndf) 1
      }

#  Now go through the NDFs we read in from the file, and write into
#  the setted listbox any which already have Set headers.
      foreach sname [lsort -dictionary [array names setname]] {
         $Setbox insert end ""
         CCDItemSetIndex $Setbox end SET
         foreach ndf $setname($sname) {
            $Setbox insert end $ndf
            CCDItemSetIndex $Setbox end $setindex($ndf)
         }
      }

#  Bind button for moving NDFs into Setted list.
      $Transfer addcommand $newsetbutton \
         "CCDNewSet $Unsetbox $Setbox \[$Unsetbox curselection\]
         "

#  Bind button for moving a single NDF into Setted list.
      $Transfer addcommand $appendbutton \
         "CCDAddSetItems $Unsetbox $Setbox \[$Unsetbox curselection\]
         "

#  Bind button for moving NDFs out of Setted list; this erases all the
#  elements in the current selection of the box except for Sett headling
#  lines, then goes through the list removing any Set heading lines
#  which now have no members.
      $Transfer addcommand $unsetbutton \
         "foreach sel \[lsort -integer -decreasing \[$Setbox curselection\]\] {
             if {\[lindex \[CCDItemSetIndex $Setbox \$sel\] 0\] != \"SET\"} {
                set setted \[$Setbox get \$sel\]
                for {set i 0} {\$i < $nndfs} {incr i} {
                set unsetted \[$Unsetbox get \$i\]
                   if {\$setted == \$unsetted} {
                      CCDItemSetIndex $Unsetbox \$i {}
                   }
                }
                $Setbox clear \$sel
             }
          }
          CCDPurgeEmptySets $Setbox
         "

#  Bind a double-click on the setted list to de-set the selected item.
      $Setbox bind list <Double-Button-1> "+
         $Transfer invoke \"$unsetbutton\"
      "

#  Bind a double-click on the unsetted list to append the selected item.
      $Unsetbox bind list <Double-Button-1> "+
         $Transfer invoke \"$appendbutton\"
      "

#  Bind HDS container grouping button.
      $Autoset addcommand "By Container" \
         "$Autoset invoke Reset
          catch {unset container}
          set conts {}
          for {set i 0} {\$i < $nndfs} {incr i} {
             set ndf \[lindex \[CCDItemSetIndex $Unsetbox \$i\] 1\]
             set containerfile \[CCDContainerFile \$ndf\]
             if { \$containerfile != \$ndf } {
                if { \[array names container \$containerfile\] == \"\" } {
                   lappend conts \$containerfile
                }
                lappend container(\$containerfile) \$i
             }
          }
          foreach cont \$conts {
             CCDNewSet $Unsetbox $Setbox \$container(\$cont)
          }
         "

#  Bind ordered grouping button.
      $Autoset addcommand "By Order" \
         "CCDGetSetIndices $Top.getsize
          set size \[llength \$CCDsetindices\]
          if { \$size > 0 && \$CCDsetindicesvalid } {
             $Autoset invoke Reset
             for {set i 0} {\$i < $nndfs} {incr i \$size} {
                set items {}
                for {set j 0} {\$j < \$size} {incr j} {
                   lappend items \[expr \$j + \$i\]
                }
                CCDNewSet $Unsetbox $Setbox \$items
             }
          }
         "

#  Bind Reset button; empties the setted list and ensures all the entries
#  in the unsetted list have no Set Index annotations.
      $Autoset addcommand "Reset" \
         "$Setbox clear 0 end
          for {set i 0} {\$i < $nndfs} {incr i} {
             CCDItemSetIndex $Unsetbox \$i {}
          }
         "

#  Bind Cancel button.
      $Control addcommand "Cancel" "$Top kill $Top"

#  Bind Accept button.
      $Control addcommand "OK" \
         "CCDDoAddSetHeaders $Top $Unsetbox $Setbox $args"

#------------------------------------------------------------------------------
#  Define any help for this window.
#------------------------------------------------------------------------------
      $Top sethelp ccdpack CCDAddSetHeadersWindow
      $Menu sethelpitem {On Window} ccdpack CCDAddSetHeadersWindow
      $Menu sethelp all ccdpack CCDAddSetHeadersMenu

#------------------------------------------------------------------------------
#  Use widgets.
#------------------------------------------------------------------------------
#  Cause a wait for this window.
      CCDWindowWait $Top

#  End of procedure.
   }
# $Id$
