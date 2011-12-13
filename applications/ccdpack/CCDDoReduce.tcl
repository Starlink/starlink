proc CCDDoReduce { Top } {

#+
#  Name:
#     CCDDoReduce

#  Purpose:
#     Performs the reduction.

#  Language:
#     Tcl/Tk procedure

#  Description:
#     This routine uses the variables set up by the xreduce interface
#     to activate a reduction. The CCDPACK application SCHEDULE
#     performs most of the actual work, but we cannot allow it to
#     execute the script as we need to add a CCDSETUP command to set
#     the global parameters.
#
#     A message indicating that the reduction has started and where
#     the output is going logged to is shown.

#  Arguments:
#     Top = window (read)
#        Name of the top-level window to parent the one showing the output
#        from SCHEDULE if it is required.

#  Global Variables:
#     CCDallndfs = list (read)
#        The names of all the known NDFs.
#     CCDglobalpars = array (read)
#        The array of all the currently defined global
#        parameters. The indices are ADC, MASK etc.
#     TASK = array (read)
#        Task control array. We need to check TASK(schedule,error) to make
#        sure that the application ran successfully.
#     CCDirflats = boolean (read)
#        If targets can be used as flatfield substitutes.
#     CCDsetindices = list of integers
#        The NDF Set Index values represented in the data.

#  Copyright:
#     Copyright (C) 1994 Science & Engineering Research Council.
#     Copyright (C) 1995-1996, 2001 Central Laboratory of the Research
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
#     1-JUN-1994 (PDRAPER):
#        Original version.
#     21-OCT-1995 (PDRAPER):
#        Added code for CCDSETUP command.
#     13-NOV-1995 (PDRAPER):
#        Added IRFLATS support.
#     30-JUL-1996 (PDRAPER):
#        Added inverse list of ADAM logicals that use inverse logic
#        (i.e. that mean the reserve of their actual value!).
#     22-JUN-2001 (MBT):
#        Added USESET parameter and CCDSETUP commands for each Set Index.
#     {enter_changes_here}

#  Bugs:
#     {note_any_bugs_here}

#-

#  Global variables:
   global CCDallndfs
   global CCDdir
   global CCDglobalpars
   global CCDirflats
   global CCDsetindices
   global TASK

#  Local constants:
   set vectors "EXTENT BOUNDS"
   set scalars "LOGTO LOGFILE USESET SATURATE GENVAR PRESERVE DIRECTION \
                ADC RNOISE MASK DEFERRED SATURATION"
   set inverse "SETSAT"

#.

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

#  Now build the command to run the application from the available parameters.
         set command "in=^XREDUCE.NDFS "
         if { ! [info exists CCDglobalpars(LOGTO)] } {
            set CCDglobalpars(LOGTO) "BOTH"
         }
         append command "logto=$CCDglobalpars(LOGTO) "
         if { ! [info exists CCDglobalpars(LOGFILE)] } {
            set CCDglobalpars(LOGFILE) "CCDPACK.LOG"
         }
         append command "logfile=$CCDglobalpars(LOGFILE) "

         if { ! [info exists CCDglobalpars(SCRIPTNAME)] } {
            set CCDglobalpars(SCRIPTNAME) "xreduce.csh"
         }
         set script $CCDglobalpars(SCRIPTNAME)
         append command "script=$script "

         if { ! [info exists CCDglobalpars(SCRIPTTYPE)] } {
            set CCDglobalpars(SCRIPTTYPE) CSH
         }
         append command "stype=$CCDglobalpars(SCRIPTTYPE) "

         if { [info exists CCDglobalpars(DEBIASTYPE)] } {
            append command "debias=$CCDglobalpars(DEBIASTYPE) "
         }
         if { [info exists CCDglobalpars(INTERPTYPE)] } {
            append command "interp=$CCDglobalpars(INTERPTYPE) "
         }
         if { [info exists CCDglobalpars(SPACESAVE)] } {
            append command "spacesave=$CCDglobalpars(SPACESAVE) "
         }
         if { [info exists CCDglobalpars(MASTERBIAS)] } {
            append command "masterbias=$CCDglobalpars(MASTERBIAS) "
         }
         if { [info exists CCDglobalpars(MASTERDARK)] } {
            append command "masterdark=$CCDglobalpars(MASTERDARK) "
         }
         if { [info exists CCDglobalpars(MASTERFLASH)] } {
            append command "masterflash=$CCDglobalpars(MASTERFLASH) "
         }
         if { [info exists CCDglobalpars(DEBIASEXT)] } {
            append command "debiasext=$CCDglobalpars(DEBIASEXT) "
         }
         if { [info exists CCDglobalpars(DARKEXT)] } {
            append command "darkext=$CCDglobalpars(DARKEXT) "
         }
         if { [info exists CCDglobalpars(FLASHEXT)] } {
            append command "flashext=$CCDglobalpars(FLASHEXT) "
         }
         if { [info exists CCDglobalpars(FLATEXT)] } {
            append command "flatext=$CCDglobalpars(FLATEXT) "
         }
         if { [info exists CCDirflats] } {
            append command "irflats=$CCDirflats "
         }

#  Make sure execution isn't immediate and try to inhibit prompts.
         append command "execute=false "
         append command "reset "
         append command "accept"

#  Create the command procedure.
         set TASK(schedule,error) ""
         CCDRunTask schedule $command 2 $Top \
            " Performing reduction scheduling please wait "

#  Remove the file list.
         catch {exec rm XREDUCE.NDFS}

#  If no errors happened the we need to add the ccdsetup command
#  and execute the script.
         if { $TASK(schedule,error) == "" } {

#  Read the execute script.
            set f [open $script r]
            set contents [read $f]
            close $f

#  Now reopen it (this erase the previous contents).
            set f [open $script w]

#  Add comments.
            puts $f "#"
            puts $f "#  XREDUCE script"
            puts $f "#"
            catch {puts $f "#  Created by [exec whoami] on [exec date]"}
            puts $f "#"

#  Write the CCDSETUP commands specific to each Set Index if we are using
#  Sets.
            if { $CCDglobalpars(USESET) == "TRUE" } {
               foreach sindex \$CCDsetindices {

#  Write the start of the CCDSETUP command.
                  if { [array names CCDglobalpars $sindex,*] != "" } {
                     puts $f " ccdsetup byset index=$sindex \\"

#  Specify the vector parameters.
                     foreach vector "$vectors" {
                        set key "$sindex,$vector"
                        if { [info exists CCDglobalpars($key)] } {
                           if { $CCDglobalpars($key) != {} } {
                              puts $f "  $vector='\[$CCDglobalpars($key)\]' \\"
                           }
                        }
                     }

#  Specify the scalar parameters.
                     foreach scalar "$scalars" {
                        set key "$sindex,$scalar"
                        if { [info exists CCDglobalpars($key)] } {
                           if { $CCDglobalpars($key) != {} } {
                              puts $f "  $scalar=$CCDglobalpars($key) \\"
                           }
                        }
                     }

#  Specify the inverse parameters.
                     foreach scalar "$inverse" {
                        set key "$sindex,$scalar"
                        if { [info exists CCDglobalpars($key)] } {
                           if { $CCDglobalpars($key) != {} } {
                              if { $CCDglobalpars($key) == "TRUE" } {
                                 puts $f "  $scalar=FALSE \\"
                              } else {
                                 puts $f "  $scalar=TRUE \\"
                              }
                           }
                        }
                     }

#  Finish off the command.
                     puts $f "  RESET ACCEPT"
                  }
               }
            }

#  Write the general CCDSETUP command.
            puts $f " ccdsetup byset=false \\"

#  Specify the vector parameters.
            foreach vector "$vectors" {
               if { [info exists CCDglobalpars($vector)] } {
                  if { $CCDglobalpars($vector) != {} } {
                     puts $f "  $vector='\[$CCDglobalpars($vector)\]' \\"
                  }
               }
            }

#  Specify the scalar parameters.
            foreach scalar "$scalars" {
               if { [info exists CCDglobalpars($scalar)] } {
                  if { $CCDglobalpars($scalar) != {} } {
                     puts $f "  $scalar=$CCDglobalpars($scalar) \\"
                  }
               }
            }

#  Specify the inverse parameters.
            foreach scalar "$inverse" {
               if { [info exists CCDglobalpars($scalar)] } {
                  if { $CCDglobalpars($scalar) != {} } {
                     if { $CCDglobalpars($scalar) == "TRUE" } {
                        puts $f "  $scalar=FALSE \\"
                     } else {
                        puts $f "  $scalar=TRUE \\"
                     }
                  }
               }
            }
            puts $f "  RESET ACCEPT"

#  Write a command to log the global parameters in force.
            puts $f " ccdshow"

#  Write the rest of the file back.
            puts $f "$contents"
            close $f

#  Now we need to execute this script.
            set logfile $CCDglobalpars(EXELOGFILE)
            exec $CCDdir/ccdexecute csh $script $logfile >&@stdout
            CCDIssueInfo "  Reduction started. The output will be \
logged in the file \"$logfile\".  "

#  And start up the background process monitor.
            if { [file executable $CCDdir/filemonitor] } {
               if { [file dirname $logfile] == "." } {
                  exec $CCDdir/filemonitor "[pwd]/$logfile" &
               } else {
                  exec $CCDdir/filemonitor "$logfile" &
               }
            }
         }
      } else {

#  Failed to open file.
         CCDIssueError "Failed to open temporary file"
      }
   } else {

#  No NDFs to process.
      CCDIssueInfo "No NDFs are available for processing, \
have you imported any into the system?"
   }

#  End of procedure.
}
# $Id$
