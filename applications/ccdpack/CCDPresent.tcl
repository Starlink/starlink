   proc CCDPresent {Top args} {
#+
#  Name:
#     CCDPresent

#  Purpose:
#     Runs the PRESENT application.

#  Language:
#     Tcl/Tk procedure

#  Description:
#     This routine runs the PRESENT application for the CCDNDFDoImport
#     procedure. It creates a file "XREDUCE.NDFS" which lists all the
#     known NDFs together with their frame types,filters, dark and
#     pre-flash exposure times. This information is generated from the
#     global variables which are setup by the CCDNDFDoImport procedure
#     and its associates. The main variable is the CCDhaveframe array
#     which identifies the types of NDF which have been presented to
#     the system and require the information importing into their
#     CCDPACK extensions.

#  Arguments:
#     Top = window (read)
#        The name of the top-level window that will parent a
#        toplevel widget to contain any output from the PRESENT
#        command. If this option has been chosen (CCDseetasks=1). If
#        the task output is not to be monitored then a wait is
#        performed $Top until the application run sucessfully
#     args = list (read)
#        If present this should be a command to run if the NDF import
#        runs successfully (such as enabling commands for the next section).

#  Global Variables:
#     CCDndfs = array (read)
#        This array contains the names of all known NDFs. The indices of the
#        array the either the names of the frame types (which are -
#        targets,flatfields,biases,darks,flashes and the possible masters)
#        or a combination of the frame type and filter type (the frame type
#        is the first index and the second the filter type).
#     CCDhaveframe = array (read)
#        This array indicates the type of NDFs which are available.
#        The indices are again targets,flatfields,biases,darks and
#        flashes. The value is a boolean. Note that all these elements
#        must exist.
#     CCDsame = array (read)
#        This array indicates whether or not the data have the same
#        filter types, the same dark exposures and/or the same
#        pre-flash exposures. The indices are filter, darks, flashes.
#     CCDfilternames = list (read)
#        If CCDsame(filter) is true then this is a comma separated
#        list of the filter names.
#     CCDfactors = array (read)
#        If the data require dark and/or pre-flash correction then
#        this array contains the exposure factors. The indices are
#        frame type followed by filter type and finally darks or
#        flashes depending on which type of correction the value
#        applies to. The filter type may not be used (if no filters are
#        available or the exposures are for dark or pre-flash masters).
#     CCDallndfs = list (write)
#        On exit this array contains all the names of the NDFs which
#        have been imported. It's contents on entry are completely erased.
#     TASK = array (read and write)
#        Task control block. Use TASK(present,error) to check status
#        of task on exit.

#  Copyright:
#     Copyright (C) 1994 Science & Engineering Research Council.
#     Copyright (C) 1995, 2001 Central Laboratory of the Research
#     Councils. All Rights Reserved.

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
#     23-MAY-1994 (PDRAPER):
#        Original version.
#     13-NOV-1995 (PDRAPER):
#        Added support for masters and IR flats (from targets).
#     19-JUN-2001 (MBT):
#        Added missing global declaration of TASK.
#     {enter_changes_here}

#  Bugs:
#     {note_any_bugs_here}

#-

#  Global variables:
      global CCDndfs
      global CCDallndfs
      global CCDhaveframe
      global CCDsame
      global CCDfilternames
      global CCDfactors
      global CCDglobalpars
      global TASK
#.

#  Split filtername up.
      if { [info exists CCDfilternames] } {
         set Fnames [split $CCDfilternames ", "]
      }

#  Check to see if we have some frames.
      if { [array exists CCDhaveframe] } {
	 if { ! [ catch { open XREDUCE.NDFS w } fileid ] } {

#  Initialise the CCDallndfs variable.
            set CCDallndfs {}
            unset CCDallndfs

#  Perform addition of a line for each NDF. Look at the NDF types in
#  turn checking for the various options which are available for
#  them.
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#  Targets and Flatfields.
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	    foreach ftype {target flat} {
               if { $ftype == "target" } {
                  set plural "targets"
               } else {
                  set plural "flatfields"
	       }
	       if { $CCDhaveframe($plural) } {

#  Different filters in use. Need indices for each known filter type.
                  set ndfindex {}
                  foreach filter $Fnames {
                     lappend ndfindex "$plural,$filter"
                  }

#  Each of the known indices has a list of NDF names. Process each
#  list in turn.
                  set j -1
                  foreach index $ndfindex {
                     incr j
                     if { [info exists CCDndfs($index)] } {
                        set i 0
                        foreach ndf $CCDndfs($index) {

#  Initialise line intended for writing to file with the name of the NDF.
                           lappend CCDallndfs $ndf
                           set line [CCDFileToNDFName $ndf]
                           append line ",$ftype"

#  Add the filter.
                           append line ",[lindex $Fnames $j]"

#  Add a dark time.
                           if { $CCDsame(darks) } {
                              if { [info exists CCDhaveframe(darks)] } {
                                 append line ",1"
                              } else {
                                 append line ",!"
                              }
                           } else {
                              append line ",[lindex $CCDfactors($index,darks) $i]"
                           }
#  And a flash time.
                           if { $CCDsame(flashes) } {
                              if { [info exists CCDhaveframe(darks)] } {
                                 append line ",1"
                              } else {
                                 append line ",!"
                              }
                           } else {
                              append line ",[lindex $CCDfactors($index,flashes) $i]"
                           }
                           puts $fileid "$line"
                           incr i
                        }
                     }
                  }
               }
            }
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#  Biases.
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#  Biases are straight-forward. Nothing to add.
	    if { $CCDhaveframe(biases) && [ info exists CCDndfs(biases) ] } {
               foreach ndf $CCDndfs(biases) {
		  lappend CCDallndfs $ndf
                  puts $fileid "[ CCDFileToNDFName $ndf ],BIAS,NONE,!,!"
	       }
	    }

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#  Darks and flashes.
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#   No filter for these. Also darks may not have a flash time.
	    foreach ftype {dark flash} {
               if { $ftype == "dark" } {
		  set plural darks
               } else {
		  set plural flashes
	       }
	       if {$CCDhaveframe($plural) && [info exists CCDndfs($plural)]} {
                  set i 0
		  foreach ndf $CCDndfs($plural) {

#  Initialise line intended for writing to file with the name of the NDF.
		     lappend CCDallndfs $ndf
		     set line [CCDFileToNDFName $ndf]
		     append line ",$ftype"

#  No filter (if one is used).
		     append line ",NONE"

#  Add a dark time.
		     if { $CCDsame(darks) } {
			append line ",1"
		     } else {
			append line \
                           ",[lindex $CCDfactors($plural,darks) $i]"
		     }

#  And a flash time for flash frames. Darks may not have a flash time.
                     if { $ftype == "dark" } {
			append line ",!"
		     } elseif { $CCDsame(flashes) } {
			append line ",1"
		     } else {
                        if { $ftype == "flash" } {
			   append line \
                              ",[lindex $CCDfactors($plural,flashes) $i ]"
                        }
		     }
		     puts $fileid "$line"
                     incr i
		  }
	       }
	    }


#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#  Master biases.
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#  Master biases are fairly straight-forward, just need to add ZEROED in
#  dark time position.
	    if { $CCDhaveframe(master_biases) && \
                    [ info exists CCDndfs(master_biases) ] } {
               foreach ndf $CCDndfs(master_biases) {
		  lappend CCDallndfs $ndf
                  puts $fileid \
   "[CCDFileToNDFName $ndf],MASTER_BIAS,NONE,$CCDglobalpars(ZEROED),!"
	       }
	    }

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#  Master flatfields.
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#  Complicated by filter dependency.
            if { $CCDhaveframe(master_flats) } {

#  Different filters in use. Need indices for each known filter type.
               set ndfindex {}
               foreach filter $Fnames {
                  lappend ndfindex "master_flats,$filter"
               }

#  Each of the known indices has a list of NDF names. Process each
#  list in turn.
               set j -1
               foreach index $ndfindex {
                  incr j
                  if { [info exists CCDndfs($index)] } {
                     foreach ndf $CCDndfs($index) {

#  Initialise line intended for writing to file with the name of the NDF.
                        lappend CCDallndfs $ndf
                        set line [CCDFileToNDFName $ndf]
                        append line ",MASTER_FLAT"

#  Add the filter.
                        append line ",[lindex $Fnames $j]"
                        append line ",!,!"
                        puts $fileid "$line"
                     }
                  }
               }
            }

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#  Master darks
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	    if { $CCDhaveframe(master_darks) && \
                    [ info exists CCDndfs(master_darks) ] } {
               foreach ndf $CCDndfs(master_darks) {
		  lappend CCDallndfs $ndf
                  puts $fileid "[CCDFileToNDFName $ndf],MASTER_DARK,!,!,!"
	       }
	    }

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#  Master flashes
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	    if { $CCDhaveframe(master_flashes) && \
                    [ info exists CCDndfs(master_flashes) ] } {
               foreach ndf $CCDndfs(master_flashes) {
		  lappend CCDallndfs $ndf
                  puts $fileid "[CCDFileToNDFName $ndf],MASTER_FLASH,!,!,!"
	       }
	    }

#  And close the temporary file.
            close $fileid

#  All being well we now have a file with the required contents,
#  so....  run the PRESENT application.  Need to add all existing
#  global parameters as GUI runs as an I task.
            set command "multientry=true simple=true modify=true \
                         in=^XREDUCE.NDFS reset accept "
            if { ! [info exists CCDglobalpars(LOGTO)] } {
               set CCDglobalpars(LOGTO) "BOTH"
	    }
	    append command "logto=$CCDglobalpars(LOGTO) "

            if { ! [info exists CCDglobalpars(LOGFILE)] } {
               set CCDglobalpars(LOGFILE) "CCDPACK.LOG"
	    }
	    append command "logfile=$CCDglobalpars(LOGFILE) "

            foreach vector "BOUNDS EXTENT" {
               if { [info exists CCDglobalpars($vector)] } {
                  if { $CCDglobalpars($vector) != {} } {
                     append command "$vector=\[$CCDglobalpars($vector)\] "
                  }
               }
            }
            foreach scalar "ADC RNOISE SATURATION DIRECTION DEFERRED" {
               if { [info exists CCDglobalpars($scalar)] } {
                  if { $CCDglobalpars($scalar) != {} } {
                     append command "$scalar=$CCDglobalpars($scalar) "
                  }
               }
            }
            if { [info exists CCDglobalpars(ZERO)] } {
               if { [info exists CCDglobalpars(ZERO)] } {
                  append command "biasvalue=$CCDglobalpars(ZERO) "
               }
	    }
            set TASK(present,error) ""
            CCDRunTask present "$command" 2 $Top \
               " Setting data descriptions, please wait "

#  Remove the file list.
            catch {exec rm XREDUCE.NDFS}

#  Run the command for completion if supplied and things are OK.
            if { "$args" != "" && $TASK(present,error) == "" } {
               eval $args
            }
	 } else {

#  Failed to open temporary file.
	    CCDIssueError "Failed to open temporary file ($fileid)"
	 }
      } else {
	 CCDIssueInfo {No NDFs to process}
      }

#  End of procedure.
   }
# $Id$
