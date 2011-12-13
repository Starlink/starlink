   proc CCDMonitorTask { Textwindow watch task output } {
#+
#  Name:
#     CCDMonitorTask

#  Purpose:
#     Monitors the progress (output) of a task.

#  Language:
#     TCL

#  Type of Module:
#     Tcl/Tk procedure.

#  Description:
#     This routine monitors the text returned from a task running in
#     the background. If it detects the presence of the string "!!"
#     then it assumes that an error has occurred and reports this fact.
#     If the global variable CCDseetasks is true then it attempts to
#     write all non-error output to a text-like widget $textwindow.
#
#     If watch is set true then the output is scanned for the string
#     (Number n of m), if located then n and m are used to modify the
#     value of the global variable TASK($task,progress) as a percentage.

#  Arguments:
#     Textwindow = window (write)
#        Command name of a text-like widget for writing the task output to.
#     watch = boolean (read)
#        If watch is set true then the task output is monitored for
#        progress statements. These are used to reflect the relative
#        scaling of a scale widget.
#     task = string (read)
#        Name of the task that is running. Used to store errors.
#     output = string (read)
#        The output of the task.

#  Global Variables:
#     CCDseetasks = boolean (read)
#        If true then the output from the task is echoed into a window.
#     TASK = array (write)
#        All the output from the task is written into the element
#        ($task,output). This is for use when parsing is required (to
#        extract values etc.). Before starting a new task this element
#        should be cleared. The element ($task,progress) is set to the
#        percentage of the task completed, if watch is true.

#  Copyright:
#     Copyright (C) 1995, 2000-2001 Central Laboratory of the Research
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
#     15-MAR-1995 (PDRAPER):
#        Added header.
#     2-AUG-1995 (PDRAPER):
#        Converted for use with the tcl ADAM extensions (instead of BLT).
#     21-AUG-1995 (PDRAPER):
#        Added storage of output to TASK(task,output).
#     30-AUG-1995 (PDRAPER):
#        Added watch argument to allow progress feedback in a scale
#        type widget.
#     16-MAY-2000 (MBT):
#        Upgraded for Tcl8.
#     3-JUL-2001 (MBT):
#        Improved error reporting - now reports all lines of error message
#        to stdout, which is helpful if a bit messy.  Also removed an
#        error reporting bug I introduced in the last change.
#     {enter_further_changes_here}

#  Bugs:
#     {note_any_bugs_here}

#-

#  Global variables.
      global CCDseetasks
      global TASK

#.

#  Check for an error.
      if { [string range $output 0 0 ] == "!" } {
         ccdputs $output
         if { [string range $output 0 1 ] == "!!" } {
            CCDTaskError $task $output
         }
      }

#  Write output from task into window if required.
      set textwindow [CCDPathOf $Textwindow]
      if { $CCDseetasks } {
         if { [winfo exists $textwindow] } {
            $Textwindow insert end "$output\n"
         }
      }

#  If required provide a progress update *+ & *- are the start and end
#  of a task. 2 percent so that we can see it has started!
      if { $watch } {
         switch -regexp $output {
            {^\*\+$} {
               set TASK($task,progress) 2
            }
            {^\*\-$} {
               set TASK($task,progress) 100
            }
            default {
               if {[regexp {Number\ +([0-9]+)\ +of\ +([0-9]+)} $output d n m]} {
                  set TASK($task,progress) [expr ($n*100)/$m]
               }
            }
         }
      }

#  Keep a copy of the output for parsing.
      append TASK($task,output) "$output\n"

#  Keep interface alive
      update idletasks
   }
# $Id$
