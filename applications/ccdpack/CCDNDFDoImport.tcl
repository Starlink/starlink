   proc CCDNDFDoImport { Topwin args } {
#+
#  Name:
#     CCDNDFDoImport

#  Purpose:
#     Selects and imports NDFs.

#  Language:
#     Tcl/Tk procedure

#  Description:
#     This routine is initiated by CCDNDFOrganize to perform the
#     actual selection of NDFs. The selection modes used depend on the
#     various global variables which are setup by CCDNDFOrganize which
#     determine the types of NDFs to be accessed, whether they are
#     colour dependent, whether dark and/or pre-flash exposures are
#     required etc.

#  Arguments:
#     Topwin = window (read)
#        Name of the top-level window which is created.
#     args = list (read)
#        If present this should be a command to run if the NDF import
#        runs successfully (such as enabling commands for the next section).

#  Global Variables:
#     CCDhaveframe = array (read)
#        This is a boolean array which has flags indicating which of the
#        available frame types are to be imported into the system. The
#        indices of this array are:
#            (targets)
#            (flatfields)
#            (biases)
#            (darks)
#            (flashes)
#            (master_biases)
#            (master_flats)
#            (master_darks)
#            (master_flashes)
#     CCDsame = array (read)
#        This array indicates the answer to the questions about
#        the presence of different filters, and whether or not the darks
#        and flashes have different exposure times. The indices are:
#            (filter)
#            (darks)
#            (flashes)
#        The values are either 1 (true) or 0 (false).
#     CCDfilternames = string (read)
#        The names of any known filters. These are separated by
#        whitespace or commas.
#     CCDndfs = array (write)
#        The names of the NDFs. This array is indexed by the frame types
#        ("targets", "flatfields", "biases", "darks" and "flashes" ) and
#        also by the filter type if used (this is the second index).
#     CCDfactors = array (read)
#        Any previous exposure factors (dark and flash times). These
#        are indexed by frame type, filter (if any) and darks or
#        flashes, as appropriate.
#     Target = array (write)
#        Names of the table/scrollbox widgets that contain the NDF
#        targets (and any exposures). This is indexed by the filtername.
#     Flat = array (write)
#        Names of the table/scrollbox widgets that contain the NDF
#        flatfields (and any exposures). This is indexed by the filtername.
#     Bias = variable (write)
#        Name of scrollbox containing bias names.
#     Dark = variable (write)
#        Name of scrollbox/table with dark count frames (and any exposures).
#     Flash = variable (write)
#        Name of scrollbox/table with flash count frames (and any exposures).
#     Masterbias = variable (write)
#        Name of scrollbox/table with master bias frame.
#     Masterflat = variable (write)
#        Name of scrollbox/table with master flatfields.
#     Masterdark = variable (write)
#        Name of scrollbox/table with master dark frame.
#     Masterflash = variable (write)
#        Name of scrollbox/table with master pre-flash frame.

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
#     24-MAY-1994 (PDRAPER):
#        Original version.
#     1-JUN-1995 (PDRAPER):
#        Removed built-in keyboard traversal.
#     17-JUL-1995 (PDRAPER):
#        Complete rewrite for new look (all NDFs accessed from one window).
#     12-NOV-1995 (PDRAPER):
#        Added masters import.
#     16-MAY-2000 (MBT):
#        Upgraded for Tcl8.
#     1-JAN-2006 (PDRAPER):
#        Fix problems with default file filter not being seen.
#        Changed to use new meta-widget names (s/Ccd_/Ccd::/g).
#     {enter_changes_here}

#  Bugs:
#     {note_any_bugs_here}

#-

#  Global variables:
      global CCDhaveframe
      global CCDsame
      global CCDfilternames
      global CCDndfs
      global CCDfactors
      global env
      global CCDcurrentdirectory
      global CCDimagefilters

#  Names of widgets with NDF lists.
      global Target
      global Flat
      global Bias
      global Dark
      global Flash
      global Masterbias
      global Masterflat
      global Masterdark
      global Masterflash

#.

#  Initialisation. Split the filternames into a list
#  (note ", " is not ",").
      if { [info exists CCDfilternames] } {
         set Fnames  [split $CCDfilternames ", "]
      }

#------------------------------------------------------------------------------
#  Widget creation.
#------------------------------------------------------------------------------
      CCDCcdWidget Top top \
         Ccd::toplevel $Topwin -title "Organize NDFs into types"
      wm withdraw $top
      CCDCcdWidget Menu menu Ccd::helpmenubar $Top.m
      CCDTkWidget Frame1 frame1 frame $top.f1
      CCDTkWidget Frame11 frame11 frame $frame1.f1
      CCDCcdWidget Directory directory Ccd::labent $Frame11.l1 -text Directory:
      if { [llength $CCDimagefilters] > 1 } {
         CCDCcdWidget FileFilter fileFilter \
            Ccd::option $Frame11.l2 -text "File Filter:"
      } else {
         CCDCcdWidget FileFilter fileFilter \
            Ccd::labent $Frame11.l2 -text "File Filter:"
      }
      CCDCcdWidget Directbox directbox \
         Ccd::scrollbox $Frame1.s1 -label {Directories:}
      CCDCcdWidget Filesbox filesbox \
         Ccd::scrollbox $Frame1.s2 -label "Images in directory:" -singleselect 0
      CCDCcdWidget Control control \
         Ccd::choice $Frame1.c -standard 0 -stack vertical
      CCDCcdWidget Choice choice Ccd::choice $Top.c1 -standard 0
      CCDTkWidget Frame2 frame2 frame $top.f2
      CCDCcdWidget Switch switch \
         Ccd::reveal $Top.r -label "Data type:" -stack array \
                         -columns 5 -in $Frame2

#------------------------------------------------------------------------------
#  Create the necessary table/scrollbox widgets and configure according to
#  requirements for additional exposure times. Also add binding so that
#  when any window is unmapped its label is configured to show the number
#  of entries.
#------------------------------------------------------------------------------
#  Targets.
      if { $CCDhaveframe(targets) } {
         if { $CCDsame(darks) && $CCDsame(flashes) } {

#  Only need scrollboxes.
            foreach f $Fnames {
               CCDCcdWidget Targ targ \
                  Ccd::scrollbox $Frame2.t$f -singleselect 0 \
                                             -label "Images selected:"
               if { $f != "NONE" } {
                  set label "TARGET $f"
               } else {
                  set label "TARGET"
               }
               $Switch addbutton $label $Targ
               bind $targ <Unmap> \
                  "CCDUpdateLabelCount $Switch \"$label\" $Targ"
               set Target($f) $Targ
            }
         } else {

#  Need tables with editable exposures.
            foreach f $Fnames {
               CCDCcdWidget Targ targ \
                  Ccd::table $Frame2.t$f -singleselect 0 -padvalue 1
               if { $f != "NONE" } {
                  set label "TARGET $f"
               } else {
                  set label "TARGET"
               }
               $Switch addbutton $label $Targ
               bind $targ <Unmap> \
                  "CCDUpdateLabelCount $Switch \"$label\" $Targ"
               $Targ setlabel 0 {Images Selected:}
               set cols 1
               if { ! $CCDsame(darks) } {
                  incr cols
                  $Targ configure -columns $cols
                  $Targ setlabel [expr $cols -1] {Dark Time:}
               }
               if { ! $CCDsame(flashes) } {
                  incr cols
                  $Targ configure -columns $cols
                  $Targ setlabel [expr $cols -1] {Flash Time:}
               }
               set Target($f) $Targ
            }
         }
      }

#  Flatfields.
      if { $CCDhaveframe(flatfields) } {
         if { $CCDsame(darks) && $CCDsame(flashes) } {

#  Only need scrollboxes.
            foreach f $Fnames {
               CCDCcdWidget Fl fl \
                  Ccd::scrollbox $Frame2.f$f -singleselect 0 \
                                             -label "Images Selected:"
               if { $f != "NONE" } {
                  set label "FLAT $f"

               } else {
                  set label "FLAT"
               }
               $Switch addbutton $label $Fl
               bind $fl <Unmap> \
                  "CCDUpdateLabelCount $Switch \"$label\" $Fl"
               set Flat($f) $Fl
            }
         } else {

#  Need tables with editable exposures.
            foreach f $Fnames {
               CCDCcdWidget Fl fl \
                  Ccd::table $Frame2.f$f -singleselect 0 -padvalue 0
               if { $f != "NONE" } {
                  set label "FLAT $f"
               } else {
                  set label "FLAT"
               }
               $Switch addbutton $label $Fl
               bind $fl <Unmap> \
                  "CCDUpdateLabelCount $Switch \"$label\" $Fl"
               $Fl setlabel 0 {Images Selected:}
               set cols 1
               if { ! $CCDsame(darks) } {
                  incr cols
                  $Fl configure -columns $cols
                  $Fl setlabel [expr $cols -1] {Dark Time:}
               }
               if { ! $CCDsame(flashes) } {
                  incr cols
                  $Fl configure -columns $cols
                  $Fl setlabel [expr $cols -1] {Flash Time:}
               }
               set Flat($f) $Fl
            }
         }
      }

#  Biases.
      if { $CCDhaveframe(biases) } {
         CCDCcdWidget Bias bias \
            Ccd::scrollbox $Frame2.b -singleselect 0 -label "Images Selected:"
         $Switch addbutton "BIAS" $Bias
         bind $bias <Unmap> "CCDUpdateLabelCount $Switch {BIAS} $Bias"
      }

#  Darks.
      if { $CCDhaveframe(darks) } {

#  Need a table if exposures are not the same.
         if { $CCDsame(darks) } {
            CCDCcdWidget Dark dark \
               Ccd::scrollbox $Frame2.d -singleselect 0 \
                         -label "Images Selected:"
         } else {
            CCDCcdWidget Dark dark \
               Ccd::table $Frame2.d -singleselect 0 -padvalue 1
            $Dark setlabel 0 {Images Selected:}

#  Configure number of columns (do this now as this keeps the frames
#  the same size as an earlier instances).
            $Dark configure -columns 2
            $Dark setlabel 1 {Dark Time:}
         }
         $Switch addbutton "DARK" $Dark
         bind $dark <Unmap> "CCDUpdateLabelCount $Switch {DARK} $Dark"
      }

#  Flashes.
      if { $CCDhaveframe(flashes) } {

#  Need a table if exposures are not the same.
         if { $CCDsame(flashes) } {
            CCDCcdWidget Flash flash \
               Ccd::scrollbox $Frame2.l -singleselect 0 \
                          -label "Images Selected:"
         } else {
            CCDCcdWidget Flash flash \
               Ccd::table $Frame2.l -singleselect 0 -padvalue 1
            $Flash setlabel 0 {Images Selected:}
            $Flash configure -columns 2
            $Flash setlabel 1 {Flash Time:}
         }
         $Switch addbutton "FLASH" $Flash
         bind $flash <Unmap> "CCDUpdateLabelCount $Switch {FLASH} $Flash"
      }

#  Master biases.
      if { $CCDhaveframe(master_biases) } {
         CCDCcdWidget Masterbias masterbias \
            Ccd::scrollbox $Frame2.mb -singleselect 1 -label "File Selected:"
         $Switch addbutton "MASTER BIAS" $Masterbias
         bind $masterbias <Unmap> \
            "CCDUpdateLabelCount $Switch {MASTER BIAS} $Masterbias"
      }

#  Master flats (at least one for each filter).
      if { $CCDhaveframe(master_flats) } {

#  Only need scrollboxes.
         foreach f $Fnames {
            CCDCcdWidget Mflat mflat \
               Ccd::scrollbox $Frame2.mf$f \
                             -singleselect 1 -label "File Selected:"
            if { $f != "NONE" } {
               set label "MASTER FLAT $f"

            } else {
               set label "MASTER FLAT"
            }
            $Switch addbutton $label $Mflat
            bind $mflat <Unmap> "CCDUpdateLabelCount $Switch \"$label\" $Mflat"
            set Masterflat($f) $Mflat
         }
      }
#  Master dark.
      if { $CCDhaveframe(master_darks) } {
         CCDCcdWidget Masterdark masterdark \
            Ccd::scrollbox $Frame2.md -singleselect 1 -label "File Selected:"
         $Switch addbutton "MASTER DARK" $Masterdark
         bind $masterdark <Unmap> \
            "CCDUpdateLabelCount $Switch {MASTER DARK} $Masterdark"
      }

#  Master flash.
      if { $CCDhaveframe(master_flashes) } {
         CCDCcdWidget Masterflash masterflash \
            Ccd::scrollbox $Frame2.mf -singleselect 1 -label "File Selected:"
         $Switch addbutton "MASTER FLASH" $Masterflash
         bind $masterflash <Unmap> \
            "CCDUpdateLabelCount $Switch {MASTER FLASH} $Masterflash"
      }


#-----------------------------------------------------------------------------
#  Widget configuration
#-----------------------------------------------------------------------------

#  Menu.
#  File items to cancel or accept window and exit interface.
      $Menu addcommand File {Close Window} "$Choice invoke Cancel"
      $Menu addcommand File {Accept Window} "$Choice invoke OK"
      $Menu addcommand File {Exit} CCDExit

#  Options menu contains list of directories which we have visited.
#  and any directories already visited this session.
      CCDRestoreDirectoryMenu $Menu Options $Directory $Choice Filter
      CCDRecordDirectoryinMenu $Menu Options [pwd] $Directory $Choice Filter
      CCDRecordDirectoryinMenu $Menu Options $env(HOME) $Directory \
                               $Choice Filter

#  Current directory and file filter entries.
#  Add binding to entry to move to the new directory when return is
#  pressed. Also add new command to options menu to move back here.
      $Directory bind entry <Key-Return> "$Choice invoke Filter;break"

#  Bind this entry to invoke the file filter button when <Return> is
#  pressed.
      $FileFilter bind entry <Key-Return> "$Choice invoke Filter;break"

#  Add all the possible image filters if available.
      if { [llength $CCDimagefilters] > 1 } {
         foreach pair "$CCDimagefilters" {
            set name [lindex $pair 0]
            set type [lindex $pair 1]
            $FileFilter addoption $name $type "$Choice invoke Filter"
         }
      }

#  Directory listbox.
#  Bind single and double mouse click to put listbox entry into the
#  directory entry window. Double also accepts this as the value to use.
#  Motion of a depressed mouse-1 button changes the directory name.
      $Directbox bind list <Button-1> "
         $Directory clear 0 end
         set index \[ %W nearest %y \]
         $Directory insert 0 \[ %W get \$index \]
         $Directbox select clear 0 end
         $Directbox select anchor \$index
      "
      $Directbox bind list <B1-Motion> "
         $Directory clear 0 end
         set index \[ %W nearest %y \]
         $Directory insert 0 \[ %W get \$index \]
         $Directbox select clear 0 end
         $Directbox select anchor \$index
      "
      $Directbox bind list <Return> "
         $Directory clear 0 end
         set index \[ lindex \[ %W curselection \] 0\]
         $Directory insert 0 \[ %W get \$index \]
         $Directbox select clear 0 end
         $Choice invoke Filter
      "
      $Directbox bind list <Double-Button-1> "
         $Directory clear 0 end
         set index \[ %W nearest %y \]
         $Directory insert 0 \[ %W get \$index \]
         $Directbox select clear 0 end
         $Choice invoke Filter
      "

#  Possible files listbox.
#  Bind double mouse click to put listbox entry into the selected files window.
      $Filesbox bind list <Double-Button-1> "
         global CCDcurrentdirectory
         set index \[ %W nearest %y\]
         set filename \[ %W get \$index \]
         set dir \$CCDcurrentdirectory
         if { \$dir == \"/\" } {
            \[$Switch current\] insert end \"/\$filename\"
         } else {
            \[$Switch current\] insert end \"\$dir/\$filename\"
         }
         $Filesbox select clear 0 end
         $Filesbox select anchor \$index
      "

#  Bind Return to choose selected files.
      $Filesbox bind list <Return> "
         global CCDcurrentdirectory
         set index \[ lindex \[ %W curselection \] 0\]
         set filename \[ %W get \$index \]
         set dir \$CCDcurrentdirectory
         if { \$dir == \"/\" } {
            \[$Switch current\] insert end \"/\$filename\"
         } else {
            \[$Switch current\] insert end \"\$dir/\$filename\"
         }
         $Filesbox select clear 0 end
         $Filesbox select anchor \$index
      "


#  Option buttons to add/remove files into/from selected list
      $Control configure  -maxwidth 8

#  Button to add the current selection to the selected names listbox.
      $Control addbutton Add \
         "global CCDcurrentdirectory
          if { \$CCDcurrentdirectory == \"/\" } {
             set dir \"/\"
          } else {
             set dir \"\${CCDcurrentdirectory}/\"
          }
          CCDCopyListbox $Filesbox \
                         \[$Switch current\] \
                         select \$dir
         "

      $Control addbutton "Add \n all" \
         "global CCDcurrentdirectory
          if { \$CCDcurrentdirectory == \"/\" } {
             set dir \"/\"
          } else {
             set dir \"\${CCDcurrentdirectory}/\"
          }
          CCDCopyListbox $Filesbox \
                         \[$Switch current\] \
                         range 0 end \$dir
         "

      $Control addbutton Remove \
         "CCDRemoveFromList \[$Switch current\] clear"

      $Control addbutton "Remove \n all" \
         "\[$Switch current\] clear 0 end"

#  Choice bar. OK Button imports NDFs into system and destroys this
#  window.
      $Choice addbutton OK "CCDDoPresent $Top $args"

#  Filter button reads the current directory and changes to it. It also
#  enters a command in the options to return to any directory we have
#  visited.
      $Choice addbutton Filter \
         "CCDGetFileUpdate $Filesbox $Directbox  $Directory $FileFilter 1
          CCDRecordDirectoryinMenu $Menu Options \
             \[$Directory get\] \
             $Directory \
             $Choice Filter
         "

#  Cancel just removes this window without worries. Current state
#  etc. will be lost.
      $Choice addbutton Cancel \
        "$Top kill $Top
         global Target
         global Flat
         global Bias
         global Dark
         global Flash
         global Masterbias
         global Masterflat
         global Masterdark
         global Masterflash
         catch { unset Target }
         catch { unset Flat }
         catch { unset Bias }
         catch { unset Dark }
         catch { unset Flash }
         catch { unset Masterbias }
         catch { unset Masterflat }
         catch { unset Masterdark}
         catch { unset Masterflash }
        "

#------------------------------------------------------------------------------
#  Define any help for this window.
#------------------------------------------------------------------------------
      $Top sethelp ccdpack CCDNDFDoImportWindow
      $Menu sethelpitem {On Window} ccdpack CCDNDFDoImportWindow
      $Menu sethelp all ccdpack CCDNDFDoImportMenu
      $Directory sethelp ccdpack CCDNDFDoImportDirectory
      $FileFilter sethelp ccdpack CCDNDFDoImportFilter
      $Directbox sethelp ccdpack CCDNDFDoImportDirectories
      $Choice sethelp all ccdpack CCDNDFDoImportChoice

#------------------------------------------------------------------------------
#  Pack widgets.
#------------------------------------------------------------------------------
      pack $menu    -side top -fill x
      pack $choice  -side bottom -fill x
      pack $switch  -side top -fill x
      pack $frame1  -side left -fill both
      pack $frame11 -side top -fill x
      pack $directory -side top -fill x
      pack $fileFilter -side top -fill x
      pack $directbox -side left -fill both
      pack $filesbox -side left -fill both
      pack $control -side left -fill both
      pack $frame2 -side right -fill both -expand true

#------------------------------------------------------------------------------
#  Use widgets.
#------------------------------------------------------------------------------

#  Make one of the windows visible (whatever is available).
      set f [lindex $Fnames 0]
      set label ""
      if { [info exists Masterflash] } { set label {MASTER FLASH} }
      if { [info exists Masterdark] } { set label {MASTER DARK} }
      if { [info exists Masterbias] } { set label {MASTER BIAS} }
      if { [info exists Masterflat($f)] } {
         if { $f == "NONE" } {
            set label {MASTER FLAT}
         } else {
            set label "MASTER FLAT $f"
         }
      }
      if { [info exists Flash] } { set label FLASH }
      if { [info exists Dark] } { set label DARK }
      if { [info exists Bias] } { set label BIAS }
      if { [info exists Flat($f)] } {
         if { $f == "NONE" } {
            set label FLAT
         } else {
            set label "FLAT $f"
         }
      }
      if { [info exists Target($f)] } {
         if { $f == "NONE" } {
            set label TARGET
         } else {
            set label "TARGET $f"
         }
      }
      $Switch invoke $label

#  Deal with the current directory (default to [pwd])
      if { ![info exists CCDcurrentdirectory] } {
         set CCDcurrentdirectory [pwd]
      } elseif { $CCDcurrentdirectory == "" } {
         set CCDcurrentdirectory [pwd]
      } elseif { ![file isdirectory $CCDcurrentdirectory] } {
         set CCDcurrentdirectory [pwd]
      }
      $Directory insert 0 $CCDcurrentdirectory

#  Set file filter and invoke the filter button.
      if { [llength $CCDimagefilters] > 1 } {
         $FileFilter insert 0 [lindex [lindex $CCDimagefilters 0] 1]
      } else {
         $FileFilter insert 0 "$CCDimagefilters"
      }
      $Choice invoke Filter

#  Set the contents of the listboxes etc. To those that already exist,
#  if any.

#  Targets.
      if { $CCDhaveframe(targets) } {
         foreach f $Fnames {
            if { [info exists CCDndfs(targets,$f)]} {
               if { $CCDsame(darks) && $CCDsame(flashes) } {
                  eval $Target($f) insert end $CCDndfs(targets,$f)
               } else {
                  set i 0
                  foreach item $CCDndfs(targets,$f) {
                     if { ! $CCDsame(darks) } {
                        if {[info exists CCDfactors(targets,$f,darks)]} {
                           lappend item \
                              [lindex $CCDfactors(targets,$f,darks) $i]
                        } else {
                           lappend item 1
                        }
                     }
                     if { ! $CCDsame(flashes) } {
                        if {[info exists CCDfactors(targets,$f,flashes)]} {
                           lappend item \
                              [lindex $CCDfactors(targets,$f,flashes) $i]
                        } else {
                           lappend item 1
                        }
                     }
                     eval $Target($f) insert end $item
                     incr i
                  }
               }
            }

#  Update the count in the button label.
            if { $f == "NONE" } {
               set label "TARGET"

            } else {
               set label "TARGET $f"
            }
            CCDUpdateLabelCount $Switch "$label" $Target($f)
         }
      }

#  Flatfields.
      if { $CCDhaveframe(flatfields) } {
         foreach f $Fnames {
            if { [info exists CCDndfs(flatfields,$f)]} {
               if { $CCDsame(darks) && $CCDsame(flashes) } {
                  eval $Flat($f) insert end $CCDndfs(flatfields,$f)
               } else {
                  set i 0
                  foreach item $CCDndfs(flatfields,$f) {
                     if { ! $CCDsame(darks) } {
                        if {[info exists CCDfactors(flatfields,$f,darks)]} {
                           lappend item \
                              [lindex $CCDfactors(flatfields,$f,darks) $i]
                        } else {
                           lappend item 1
                        }
                     }
                     if { ! $CCDsame(flashes) } {
                        if {[info exists CCDfactors(flatfields,$f,flashes)]} {
                           lappend item \
                              [lindex $CCDfactors(flatfields,$f,flashes) $i]
                        } else {
                           lappend item 1
                        }
                     }
                     eval $Flat($f) insert end $item
                     incr i
                  }
               }
            }

#  Update the count in the button label.
            if { $f == "NONE" } {
               set label "FLAT"

            } else {
               set label "FLAT $f"
            }
            CCDUpdateLabelCount $Switch "$label" $Flat($f)
         }
      }

#  Biases.
      if { $CCDhaveframe(biases) } {
         if { [ info exists CCDndfs(biases) ] } {
            eval $Bias insert end $CCDndfs(biases)
         }
         CCDUpdateLabelCount $Switch {BIAS} $Bias
      }

#  Darks.
      if { $CCDhaveframe(darks) } {
         if { [info exists CCDndfs(darks)] } {

#  Quick insertion if no darks and/or flashes are required. Otherwise
#  need to look at each element in turn.
            if { $CCDsame(darks) && $CCDsame(flashes) } {
               eval $Dark insert end $CCDndfs(darks)
            } else {
               set i 0
               foreach item $CCDndfs(darks) {
                  if { ! $CCDsame(darks) } {
                     if {[info exists CCDfactors(darks,darks)]} {
                     lappend item \
                        [lindex $CCDfactors(darks,darks) $i]
                     } else {
                        lappend item 1
                     }
                  }
                  eval $Dark insert end $item
                  incr i
               }
            }
         }
         CCDUpdateLabelCount $Switch {DARK} $Dark
      }

#  Flashes.
      if { $CCDhaveframe(flashes) } {
         if { [ info exists CCDndfs(flashes) ] } {

#  Quick insertion if no darks and/or flashes are required. Otherwise
#  need to look at each element in turn.
            if { $CCDsame(darks) && $CCDsame(flashes) } {
               eval $Flash insert end $CCDndfs(flashes)
            } else {
               set i 0
               foreach item $CCDndfs(flashes) {
                  if { ! $CCDsame(darks) } {
                     if {[info exists CCDfactors(flashes,darks)]} {
                        lappend item \
                           [lindex $CCDfactors(flashes,darks) $i]
                     } else {
                        lappend item 1
                     }
                  }
                  if { ! $CCDsame(flashes) } {
                     if {[info exists CCDfactors(flashes,flashes)]} {
                        lappend item \
                           [lindex $CCDfactors(flashes,flashes) $i]
                     } else {
                        lappend item 1
                     }
                  }
                  eval $Flash insert end $item
                  incr i
               }
            }
         }
         CCDUpdateLabelCount $Switch {FLASH} $Flash
      }

#  Master bias.
      if { $CCDhaveframe(master_biases) } {
         if { [ info exists CCDndfs(master_biases) ] } {
            eval $Masterbias insert end $CCDndfs(master_biases)
         }
         CCDUpdateLabelCount $Switch {MASTER BIAS} $Masterbias
      }

#  Flatfields.
      if { $CCDhaveframe(master_flats) } {
         foreach f $Fnames {
            if { [info exists CCDndfs(master_flats,$f)]} {
               eval $Masterflat($f) insert end $CCDndfs(master_flats,$f)
            }

#  Update the count in the button label.
            if { $f == "NONE" } {
               set label "MASTER FLAT"

            } else {
               set label "MASTER FLAT $f"
            }
            CCDUpdateLabelCount $Switch "$label" $Masterflat($f)
         }
      }

#  Master dark.
      if { $CCDhaveframe(master_darks) } {
         if { [ info exists CCDndfs(master_darks) ] } {
            eval $Masterdark insert end $CCDndfs(master_darks)
         }
         CCDUpdateLabelCount $Switch {MASTER DARK} $Masterdark
      }

#  Master flash.
      if { $CCDhaveframe(master_flashes) } {
         if { [ info exists CCDndfs(master_flashes) ] } {
            eval $Masterflash insert end $CCDndfs(master_flashes)
         }
         CCDUpdateLabelCount $Switch {MASTER FLASH} $Masterflash
      }


#  Cause a wait for this window.
      wm deiconify $top
      CCDWindowWait $Top

#  End of procedure.
   }
# $Id$
