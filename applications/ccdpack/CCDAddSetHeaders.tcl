   proc CCDAddSetHeaders { Topwin args } {
#+
#  Name:
#     CCDAddSetHeaders

#  Purpose:
#     Allows user to group NDFs into Sets and adds CCDPACK Set headers.

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

#  Global variables:
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

#  Authors:
#     MBT: Mark Taylor (STARLINK)

#  History:
#     14-JUN-2001 (MBT):
#        Original version.

#-

#  Global Variables:
      global CCDallndfs
      global CCDndfcontainers
      global CCDaddwcs
      global CCDsetindices
      global CCDsetindicesvalid

#------------------------------------------------------------------------------
#  Widget creation.
#------------------------------------------------------------------------------
#  Create top-level object.
      CCDCcdWidget Top top Ccd_toplevel $Topwin -title "Group NDFs by Set"
      wm withdraw $top

#  Add a standard menu bar.
      CCDCcdWidget Menu menu Ccd_helpmenubar $Top.menu

#  Create frame for central part.
      CCDTkWidget Centre centre frame $top.centre

#  Create frame for part with setted and unsetted listboxes.
      CCDTkWidget Fileboxes fileboxes frame $centre.fileboxes

#  Create listbox for unSetted NDFs.
      CCDCcdWidget Unsetbox unsetbox \
         Ccd_scrollbox $Fileboxes.unset -label "All NDFs" -singleselect 0

#  Create frame for Setted NDFs.
      CCDCcdWidget Setbox setbox \
         Ccd_scrollbox $Fileboxes.set -label "NDFs in Sets" -singleselect 0

#  Create panel for buttons controlling manual transfer between listboxes.
      CCDCcdWidget Transfer transfer \
         Ccd_choice $Fileboxes.transfer -standard 0 -stack vertical

#  Create panel for automatic Setting method buttons.
      CCDCcdWidget Autoset autoset \
         Ccd_choice $Centre.autoset -standard 0 

#  Create panel for widget control.
      CCDCcdWidget Control control \
         Ccd_choice $Top.control -standard 0

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

#  Run SHOWSET to determine the Set header content of all the NDFs which
#  we know about.
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
      foreach sname [array names setname] {
         $Setbox insert end ""
         CCDItemSetIndex $Setbox end SET
         foreach ndf $setname($sname) {
            $Setbox insert end $ndf
            CCDItemSetIndex $Setbox end $setindex($ndf)
         }
      }

#  Menu configuration.
      $Menu addcommand File {Close Window} "$Control invoke Cancel"
      $Menu addcommand File {Accept Window} "$Control invoke OK"
      $Menu addcommand File {Exit} CCDExit
      $Menu addcheckbutton Options {Write CCD_SET alignment coordinate frame} \
         -variable CCDaddwcs

#  Bind button for moving NDFs into Setted list.
      $Transfer addbutton "New Set\n  ->  " \
         "CCDNewSet $Unsetbox $Setbox \[$Unsetbox curselection\]
         "

#  Bind button for moving NDFs out of Setted list; this erases all the
#  elements in the current selection of the box, then goes through the
#  list removing any Set heading lines which now have no members. 
      $Transfer addbutton "De-Set\n  <-  " \
         "foreach sel \[lsort -integer -decreasing \[$Setbox curselection\]\] {
            set setted \[$Setbox get \$sel\]
            for {set i 0} {\$i < $nndfs} {incr i} {
               set unsetted \[$Unsetbox get \$i\]
               if {\$setted == \$unsetted} {
                  CCDItemSetIndex $Unsetbox \$i {}
               }
            }
            $Setbox clear \$sel
          }
          set pos 0
          while {\$pos < \[$Setbox size\]} {
             if {\[lindex \[CCDItemSetIndex $Setbox \$pos\] 0\] == \"SET\" &&
                  ( \[lindex \[CCDItemSetIndex $Setbox \[expr \$pos+1\]\] 0\] \
                                                               == \"SET\" ||
                   \$pos == \[expr \[$Setbox size\] - 1\] )} {
                $Setbox clear \$pos
             } else {
                incr pos
             }
          }
         "

#  Bind HDS container grouping button.
      $Autoset addbutton "By Container" \
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
      $Autoset addbutton "By Order" \
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
      $Autoset addbutton "Reset" \
         "$Setbox clear 0 end
          for {set i 0} {\$i < $nndfs} {incr i} {
             CCDItemSetIndex $Unsetbox \$i {}
          }
         "

#  Bind Cancel button.
      $Control addbutton "Cancel" "$Top kill $Top"

#  Bind Accept button.
      $Control addbutton "OK" \
         "CCDDoAddSetHeaders $Top $Unsetbox $Setbox $args"

#------------------------------------------------------------------------------
#  Define any help for this window.
#------------------------------------------------------------------------------


#------------------------------------------------------------------------------
#  Use widgets.
#------------------------------------------------------------------------------
#  Cause a wait for this window.
      wm deiconify $top
      CCDWindowWait $Top

#  End of procedure.
   }
# $Id$
