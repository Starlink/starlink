proc CCDCheckReduce { Top } {
#+
#  Name:
#     CCDCheckReduce

#  Purpose:
#     Checks what sort of reduction is possible.

#  Language:
#     Tcl/Tk procedure

#  Description:
#     This routine uses the variables set up the the xreduce interface
#     to pseudo schedule a reduction using the routine SCHEDULE. The
#     idea is to get SCHEDULE to work out what type of debiassing options
#     are available use these to constrain the options that are available
#     from the X interface.

#  Arguments:
#     Top = window (read)
#        Name of the top-level window to parent the one showing the output
#        from SCHEDULE if it is required.

#  Global Variables:
#      CCDallndfs = list (read)
#         The names of all the known NDFs.
#      CCDglobalpars = array (read)
#         The array of all the currently defined global
#         parameters. The indices are ADC, MASK etc.
#      TASK = array (read)
#         Task control array. We need to check TASK(schedule,error) to make
#         sure that the application ran successfully.
#      CCDirflats = boolean (read)
#         True if targets frames are to be considered as flatfields,
#         in the absence of any.

#  Copyright:
#     Copyright (C) 1995 Central Laboratory of the Research Councils.
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
#     PDRAPER: Peter Draper (STARLINK - Durham University)
#     {enter_new_authors_here}

#  History:
#     19-OCT-1995 (PDRAPER):
#        Original version.
#     13-NOV-1995 (PDRAPER):
#        Added IRFLATS support.
#     {enter_changes_here}

#  Bugs:
#     {note_any_bugs_here}

#-

#  Global variables:
   global BIAS
   global CCDallndfs
   global CCDglobalpars
   global CCDirflats
   global TASK
#.

#  Default debiassing possibilities.
   set BIAS(debias,1) 1
   set BIAS(debias,2) 1
   set BIAS(debias,3) 1
   set BIAS(debias,4) 0
   set BIAS(interp,1) 1
   set BIAS(interp,2) 1
   set BIAS(interp,3) 1
   set BIAS(interp,4) 1

#  Exit status
   set status 1

#  Create a list of all the NDF names to input into SCHEDULE.
   set exists 0
   if { [info exists CCDallndfs] } {
      if { $CCDallndfs != {} } {
         set exists 1
      }
   }
   if { $exists } {
      if { ! [catch { open XREDUCE.NDFS w } fileid] } {

#  Put names into list.
         foreach ndf $CCDallndfs {
            puts $fileid [CCDFileToNDFName $ndf]
         }
         close $fileid

#  Catch snaphot of .tmp files in current directory.
         foreach file [glob -nocomplain *.tmp] {
            set tmp($file) 1
         }


#  Now run the task on these files.
         set TASK(schedule,error) ""
         set command \
            "logto=terminal in=^XREDUCE.NDFS stype=csh \
             script=xreduce.tmp debias=3 interp=1 execute=false \
             irflats=$CCDirflats accept reset"
         CCDRunTask schedule $command 2 $Top \
            "  Checking possible debiassing methods please wait "

#  Remove any new .tmp files.
         foreach file [glob -nocomplain *.tmp] {
            if { ! [info exists tmp($file)] } {
               catch {exec rm $file}
            }
         }
         catch { unset tmp }

         if { $TASK(schedule,error) != "" } {
            CCDIssueError "Failed to check possible reduction schedules"
            set status 0
         } else {

#  Otherwise parse the output from SCHEDULE and extract the possible options.
            set BIAS(debias,1) 0
            set BIAS(debias,2) 0
            set BIAS(debias,3) 0
            set BIAS(debias,4) 0
            set BIAS(interp,1) 0
            set BIAS(interp,2) 0
            set BIAS(interp,3) 0
            set BIAS(interp,4) 0
            set modified 0
            set outputlist [split $TASK(schedule,output) "\n"]
            set length [llength $outputlist]
            set i 0
            while { 1 } {
               set line [lindex $outputlist $i]
               switch $line {
                  "  Debiassing options which seem to be available:" {
                     for { set j 0 } { $j < 4 } { incr j } {
                        incr i
                        set v [lindex $outputlist $i]
                        if { $v != "" } {
                           catch {unset type}
                           scan $v "%d %s" type dummy
                           if { [info exists type] } {
                              set BIAS(debias,$type) 1
                              set modified 1
                           }
                        } else {
                           break
                        }
                     }
                  }
                  "  Interpolation options which seem to be available:" {
                     for { set j 0 } { $j < 4 } { incr j } {
                        incr i
                        set v [lindex $outputlist $i]
                        if { $v != "" } {
                           catch {unset type}
                           scan $v "%d %s" type dummy
                           if { [info exists type] } {
                              set BIAS(interp,$type) 1
                              set modified 1
                           }
                        } else {
                           break
                        }
                     }
                     break ;#  finished.
                  }
               }
               incr i
               if { $i > $length } { break }
            }

#  If no types have been extracted revert to defaults.
            if { !$modified } {
               CCDIssueInfo \
                  "No debiassing options appear to be available proceeding with default options"
               set BIAS(debias,1) 1
               set BIAS(debias,2) 1
               set BIAS(debias,3) 1
               set BIAS(debias,4) 0
               set BIAS(interp,1) 1
               set BIAS(interp,2) 1
               set BIAS(interp,3) 1
               set BIAS(interp,4) 1
            }
         }
      } else {

#  Failed to open file.
         CCDIssueError "Failed to open temporary file"
         set status 0
      }
   } else {

#  No NDFs to process.
      CCDIssueInfo "No data frames are available for processing, \
import some into the system!"
      set status 0
   }

#  End of procedure.
   return $status
}
# $Id$
