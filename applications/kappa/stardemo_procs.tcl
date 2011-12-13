#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#+
#  Name:
#     stardemo_procs.tcl

#  Purpose:
#     Defines Tcl procedures needed by STARDEMO.

#  Language:
#     TCL

#  Copyright:
#     Copyright (C) 1999 Central Laboratory of the Research Councils

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
#     DSB: David S. Berry (STARLINK)
#     {enter_new_authors_here}

#  History:
#     22-OCT-1999 (DSB):
#        Original version
#     {enter_further_changes_here}

#  Bugs:
#     {note_any_bugs_here}

#-


proc BeginUF {} {
#+
#  Name:
#     BeginUF

#  Purpose:
#     Mark the start of a temporary file context. Calls to this procedure
#     should be matched by calls to EndUF.

#  Language:
#     TCL

#  Arguments:
#     None

#  Returned Value:
#     An integer context identifier which can be passed to EndUF.

#  Globals:
#     IFILE (Read)
#        Temporary file names created by UniqueFile are stored in stardemo's
#        temporary STARDEMO_SCRATCH directory so that they are deleted when
#        stardemo terminates. They have a name of the form stardem<i> where
#        <i> is an integer, which is different for each file and
#        increases monotonically throughout the execution of stardemo. IFILE
#        records the value of i used in the previous call to UniqueFile.
#     IFILE_STACK (Write)
#        A stack on which is stored the value of IFILE corresponding to
#        the first temporary file to be created in the new context.

#  Copyright:
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
#     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
#     02111-1307, USA

#  Authors:
#     {original_author_entry}

#  History:
#     {enter_changes_here}

#  Bugs:
#     {note_any_bugs_here}

#-
   global IFILE
   global IFILE_STACK
   set nentry [llength $IFILE_STACK]
   Push IFILE_STACK [expr $IFILE + 1]
   return $nentry
}

proc Blink {w option value1 value2 interval} {
#+
#  Name:
#     Blink

#  Purpose:
#     Blinks a widget option.

#  Language:
#     TCL

#  Arguments:
#     w
#       The widget path.
#     option
#       The option to be blinked (eg "-foreground").
#     value1
#       The first option value to use (eg "red" ).
#     value2
#       The second option value to use (eg "green" ).
#     interval
#       The interval between flashes in milliseconds.

#  Globals:
#     STOP_BLINK (Read and Write)
#       If this is not null on entry then the blinking is stopped, and
#       the option value is set to the value of STOP_BLINK (which is then
#       reset to null).

#  Copyright:
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
#     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
#     02111-1307, USA

#  Authors:
#     {original_author_entry}

#  History:
#     {enter_changes_here}

#  Bugs:
#     {note_any_bugs_here}

#-
   global STOP_BLINK

   if { [winfo exists $w] } {
      if { $STOP_BLINK == "" } {
         $w configure $option $value1
         after $interval [list Blink $w $option $value2 $value1 $interval]
      } {
         $w configure $option $STOP_BLINK
         set STOP_BLINK ""
      }
   }
}

proc CheckMsg {action val} {
#+
#  Name:
#     CheckMsg

#  Purpose:
#     Checks messages created by an ADAM action for error messages. If an
#     error message is found, it is added to a global list of error messages.

#  Language:
#     TCL

#  Arguments:
#     action
#       The current action.
#     val
#       The value of the ADAM error message.

#  Globals:
#     ADAM_ERRORS (Read and Write)
#       The current list of ADAM error messages.
#     ATASK_OUTPUT (Read and Write)
#       Any non-error messages are appended to this list. Each
#       message is stored as a new element in the list.
#     LOGFILE_ID (Read)
#       The file id for any logfile to which all messages should be
#       written.

#  Copyright:
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
#     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
#     02111-1307, USA

#  Authors:
#     {original_author_entry}

#  History:
#     {enter_changes_here}

#  Bugs:
#     {note_any_bugs_here}

#-
   global ADAM_ERRORS
   global ATASK_OUTPUT
   global LOGFILE_ID

# Write all messages to the log file if there is one.
   if { $LOGFILE_ID != "" } {
      puts $LOGFILE_ID "$action: $val"
   }

# Error messages are distinguished from other informational messages
# by starting with one or more exclamation marks. Ignore the supplied
# message if it does not start with an exclamation mark. Otherwise,
# add it to the list (on a new line), and indicate an error has occurred.
   if { [regexp {^!+(.*)} $val match mess] } {
      if { [info exists ADAM_ERRORS] } {
         if { [string length $mess] > 30 } {
            set ADAM_ERRORS "$ADAM_ERRORS\n$mess"
         } {
            set ADAM_ERRORS "$ADAM_ERRORS $mess"
         }
      } {
         set ADAM_ERRORS $mess
      }

# If the message is not an error message, append it as a new element to
# the list stored in ATASK_OUTPUT.
   } {
      append ATASK_OUTPUT "$val\n"
   }
}

proc CheckRF {task} {
#+
#  Name:
#     CheckRF

#  Purpose:
#     Check that the AMS rendevous file for a task still exists. If it
#     does not (for some reason it seems to be occasionally deleted by
#     the StarTcl system, turning the process into a zombie), then the
#     task is killed and re-loaded.

#  Language:
#     TCL

#  Arguments:
#     The task to be checked (previously loaded using LoadTask).

#  Returned Value:
#     Returns 1 if the rendevous file still exists, and zero if it
#     did not exist (in which case the task will have been re-loaded).

#  Globals:
#     RENDEVOUS (Read)
#        A 1-d array, indexed by task name, storing the path to the
#        task's rendevous file.
#     TASK_FILE
#        A 1-d array, indexed by task name, storing the path to the
#        task's executable binary file (as supplied to LoadTask).

#  Copyright:
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
#     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
#     02111-1307, USA

#  Authors:
#     {original_author_entry}

#  History:
#     {enter_changes_here}

#  Bugs:
#     {note_any_bugs_here}

#-
   global RENDEVOUS
   global TASK_FILE

   if { ![file exists $RENDEVOUS($task)] } {
      catch {$task kill}
      Message "$task rendevous file ($RENDEVOUS($task)) has dissappeared! Re-loading the task."
      LoadTask $task $TASK_FILE($task)
      set ret 0
   } {
      set ret 1
   }

   return $ret

}

proc Confirm {message} {
#+
#  Name:
#     Confirm

#  Purpose:
#     Get the user to confirm an operation. The supplied text is displayed
#     and the user presses one of two buttons. An indication of which
#     button was pressed is returned.

#  Language:
#     TCL

#  Arguments:
#     message
#       The message to display.

#  Returned Value:
#     Zero is returned if the operation should be cancelled, one is
#     returned if it is ok to proceed.

#  Copyright:
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
#     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
#     02111-1307, USA

#  Authors:
#     {original_author_entry}

#  History:
#     {enter_changes_here}

#  Bugs:
#     {note_any_bugs_here}

#-

# Display the dialog box and get the user's response.
   set but [dialog .confirm "stardemo Confirmation" $message {} 0 OK Cancel]

# Return the answer.
   return [expr 1 - $but]
}

proc Message {message} {
#+
#  Name:
#     Message

#  Purpose:
#     Display a dialogue box displaying a message, and wait for the
#     user to press the "OK" button.

#  Language:
#     TCL

#  Arguments:
#     message
#       The message to display.

#  Globals:
#     TOP (Read)
#        The path to the main application window.

#  Copyright:
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
#     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
#     02111-1307, USA

#  Authors:
#     {original_author_entry}

#  History:
#     {enter_changes_here}

#  Bugs:
#     {note_any_bugs_here}

#-
   global TOP
   global env

# If the top level window has not yet been created, message to standard
# output
   if { ![info exists TOP] } {
      puts $message

# Otherwise, display the message in a dialog box.
   } {

# Display the dialog box.
      dialog .msg "stardemo - Message" $message {} 0 OK

   }

}

proc EndUF {context leave} {
#+
#  Name:
#     EndUF

#  Purpose:
#     Mark the end of a temporary file context. Calls to this procedure
#     should be matched by calls to BeginUF. This procedure deletes all
#     temprary files created since the correspinding call to BeginUF, except
#     for any files included in the argument "leave".

#  Language:
#     TCL

#  Arguments:
#     context
#        A context identifier returned by BeginUF. All contexts contained
#        with the specified context are also ended.
#     leave
#        A list of files which are to be escaped into the next higher
#        context.

#  Globals:
#     STARDEMO_SCRATCH
#       The path to the temporary directory used to store temporary images
#       created by stardemo.
#     IFILE (Read)
#        Temporary file names created by UniqueFile are stored in stardemo's
#        temporary STARDEMO_SCRATCH directory which is deleted when
#        stardemo terminates. They have a name of the form stardem<i> where
#        <i> is an integer, which is different for each file and
#        increases monotonically throughout the execution of stardemo. IFILE
#        records the value of i used in the previous call to UniqueFile.
#     IFILE_STACK (Write)
#        A stack on which is stored the value of IFILE corresponding to
#        the first temporary file created in the current context.

#  Copyright:
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
#     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
#     02111-1307, USA

#  Authors:
#     {original_author_entry}

#  History:
#     {enter_changes_here}

#  Bugs:
#     {note_any_bugs_here}

#-
   global IFILE
   global IFILE_STACK
   global STARDEMO_SCRATCH

# Loop round each value of IFILE used in this context. This starts with
# the value stored by the corresponding call to BeginUF, and ends with
# the current value.
   set levels [expr [llength $IFILE_STACK] - $context ]
   set ifile_start [Pop IFILE_STACK $levels]
   for {set i $ifile_start} {$i <= $IFILE} {incr i} {

# Construct the corresponding file name (NB, make sure this next line keeps
# in step with any changes made in procedure UniqueFile).
      set file "$STARDEMO_SCRATCH/stardem$i"

# If the file exists (with any file extension), but has not been included
# in the supplied list of files to be escaped, then delete the file.
      if { [lsearch -exact $leave $file] == -1 } {
         foreach f [glob -nocomplain ${file}.*] {
            catch "exec rm -f $f"
         }
      }
   }
}

proc exit {args} {

#+
#  Name:
#     exit

#  Purpose:
#     Shutdown the tcl script, cleaning up stardemo internals in the process.

#  Language:
#     TCL

#  Arguments:
#     args
#       The exit integer status value.

#  Notes:
#     - This command replaces the built-in Tcl "exit" command, which should
#     have been renamed as "tcl_exit".

#  Globals:
#     ADAM_TASKS (Read)
#       A list of the names of the ADAM tasks started up by stardemo.
#     ADAM_USER (Read)
#       The path to the temporary ADAM_USER directory used by stardemo.
#     OLD_ADAM_USER (Read)
#       The original value of the ADAM_USER environment variable, or a null
#       string if ADAM_USER was not defined.
#     OLDKAPPA (Read)
#       A list of process id.s for any KAPPA processes which were running
#       when stardemo was started.
#     STARDEMO_SCRATCH (Read)
#       The path to the directory used by stardemo to store temporary NDFs.

#  Copyright:
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
#     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
#     02111-1307, USA

#  Authors:
#     {original_author_entry}

#  History:
#     {enter_changes_here}

#  Bugs:
#     {note_any_bugs_here}

#-
   global ADAM_TASKS
   global ADAM_USER
   global env
   global LOGFILE_ID
   global OLD_ADAM_USER
   global OLD_AGI_USER
   global STARDEMO_SCRATCH

# Re-instate the original exit command in case anything goes wrong in
# this procedure.
   rename exit {}
   rename tcl_exit exit

# Close any log file.
   if { $LOGFILE_ID != "" } { close $LOGFILE_ID }

# Kill all the ADAM tasks started up by stardemo.
   foreach task $ADAM_TASKS {
      if { [info commands $task] != "" } {
         catch {$task kill}
      }
   }

# Delete the temporary ADAM_USER directory created at the start.
   catch "exec rm -rf $ADAM_USER"

# Delete the STARDEMO_SCRATCH directory created at the start.
   catch "exec rm -rf $STARDEMO_SCRATCH"

# Kill any new processes (i.e ones which are not in the list of KAPPA
# processes which were active when stardemo started).
   killNew

# Re-instate the original ADAM_USER and AGI_USER environment variables.
   if { $OLD_ADAM_USER != "" } {
      set env(ADAM_USER) $OLD_ADAM_USER
   } {
      unset env(ADAM_USER)
   }

   if { $OLD_AGI_USER != "" } {
      set env(AGI_USER) $OLD_AGI_USER
   } {
      unset env(AGI_USER)
   }

# Finally, kill the current process.
   exit
}

proc FindHelp {x y} {
#+
#  Name:
#     FindHelp

#  Purpose:
#     Find the htx cross-reference label associated with particular
#     root coordinates.

#  Language:
#     TCL

#  Arguments:
#     x y
#        The X and Y root coordinates.

#  Returned Value:
#     The HTX cross-reference label assocaited with the given position,
#     or a blank string if there is no associated label.

#  Globals:
#     HELP_LABELS (Read)
#       A 1-D array index by widget name. Each element is an htx
#       cross-reference label to be displayed if the widget is selected
#       using "Help on pointer".

#  Copyright:
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
#     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
#     02111-1307, USA

#  Authors:
#     {original_author_entry}

#  History:
#     {enter_changes_here}

#  Bugs:
#     {note_any_bugs_here}

#-
   global HELP_LABELS

# Find the lowest level window at the given root coordinates.
   set w [winfo containing $x $y]

# Assume there is no label associated with this position.
   set help_label ""

# Loop until we find a label or we have checked all levels in the
# widget's family tree.
   while { $w != "" } {

# If this widget has a label, return it.
      if { [info exists HELP_LABELS($w)] } {
         set help_label $HELP_LABELS($w)
         break

# Otherwise, find the father of the current widget.
      } {
         set w [winfo parent $w]
      }
   }

# Return the label
   return $help_label

}

proc Finish {save} {
#+
#  Name:
#     Finish

#  Purpose:
#     Exit stardemo, warning the user if the output images have not yet
#     been saved.

#  Language:
#     TCL

#  Arguments:
#     save
#        Should the output images be saved before asking the user whether
#        or not to exit? Ignored at the moment.

#  Copyright:
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
#     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
#     02111-1307, USA

#  Authors:
#     {original_author_entry}

#  History:
#     {enter_changes_here}

#  Bugs:
#     {note_any_bugs_here}

#-

# See if the user confirms the intention to exit. If so, exit.
   if { [Confirm "Exit stardemo?"] } { exit }

}

proc GetParam {task param} {
#+
#  Name:
#     GetParam

#  Purpose:
#     Get a parameter value from an ADAM task.

#  Language:
#     TCL

#  Arguments:
#     task
#       The name of the task (eg "kapview").
#     param
#       The name of the parameter in the form "action:param"
#       (eg "datapic:ncx1").

#  Returned Value:
#     The parameter value.

#  Notes:
#     - This procedure does not return until the parameter value has been
#     obtained.

#  Globals:
#     PAR_VALUE (Write)
#       The most recently acquired parameter value.

#  Copyright:
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
#     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
#     02111-1307, USA

#  Authors:
#     {original_author_entry}

#  History:
#     {enter_changes_here}

#  Bugs:
#     {note_any_bugs_here}

#-
   global PAR_VALUE

   global PAR_GOT

# Issue the request for the parameter value.
   set PAR_GOT 0
   $task get $param -getresponse {set PAR_VALUE %V;set PAR_GOT 1}

# Wait until the request has been fulfilled.
   WaitFor PAR_GOT

# Return the parameter value.
   return $PAR_VALUE
}

proc HelpArea {} {
#+
#  Name:
#     HelpArea

#  Purpose:
#     Create or destroy the frame displaying help information at the
#     bottom of the main window.

#  Language:
#     TCL

#  Arguments:
#     None.

#  Globals:
#     F4 (Read and Write)
#        The name of the frame to contain help information.
#     HAREA (Read)
#        Is help information to be displayed?
#     HLP_FONT (Read)
#        The font in which to display help information.

#  Copyright:
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
#     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
#     02111-1307, USA

#  Authors:
#     {original_author_entry}

#  History:
#     {enter_changes_here}

#  Bugs:
#     {note_any_bugs_here}

#-
   global F4
   global HAREA
   global HLP_FONT
   global TOP
   global HSTRUT
   global HLAB
   global FONT

# If required, create the help frame (if it has not already been created).
   if { $HAREA } {
      if { $F4 == "" } {

# Find the pixel size of the font. Use 10 if the font string cannot be
# parsed.
         if { ![regexp {^-[^-]*-[^-]*-[^-]*-[^-]*-[^-]*-[^-]*-([^-]+)} $HLP_FONT \
                match pixsize] } { set pixsize 10 }

# Find the pixels in 2 characters.
         if { [scan $pixsize %d pxsiz] == 0 } {
            set pxsiz 14
         }
         set height [expr 2 * $pxsiz]

# Create the frame to enclose the help text.
         set F4 [frame $TOP.help -relief groove -bd 2]
         pack $F4 -fill x -side bottom -anchor s -padx 1m -pady 1m

# Create a dummy frame with height but no width to act as a vertical strut.
# Geometry propagation is turn off for this frame so that its requested
# size will be retained even though nothing is put in the frame. This
# strut is used to keep the help area the same size even if the message text
# within it requires a narrower area.
         set HSTRUT [Strut $F4 $height]

# Create a label widget to display the time remaining.
         set con [label $F4.conlab -justify center -textvariable TLEFT \
                            -anchor e -font $FONT -width 3 ]
         pack $con -side right

# The width is the requested width of the whole window.
         update idletasks
         set width [expr [winfo width .] - [winfo width $con]]
         set width [expr 0.95 * $width]

# Create a message widget to display dynamic help information about
# the widget underneath the pointer.
         set HLAB [message $F4.lab -justify left -textvariable HELP \
                            -anchor w -font $HLP_FONT -width $width]
         pack $HLAB -side left -fill x -expand 1

# Set up the help for the help area.
         SetHelp $F4  ".  An area which shows brief help on the object under the pointer. More detailed help can be obtained using the Help menu." STARDEMO_HELP_AREA
         SetHelp $con ".  Indicates the time (in seconds) remaining until the next commentary change (in auto-paging mode)."  STARDEMO_TIME_AREA
      }

# If required, destroy the help frame (if it has not already been destroyed).
   } {
      if { $F4 != "" } {
         destroy $F4
         set F4 ""
      }
   }
}

proc Helper {} {
#+
#  Name:
#     Helper

#  Purpose:
#     Selects the text to display in the help area.

#  Language:
#     TCL

#  Arguments:
#     None.

#  Globals:
#     HELPS (Read)
#        An array holding the help messages for all widgets, indexed by
#        widget name.
#     HELP (Write)
#        The text to be displayed in the help area.

#  Copyright:
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
#     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
#     02111-1307, USA

#  Authors:
#     {original_author_entry}

#  History:
#     {enter_changes_here}

#  Bugs:
#     {note_any_bugs_here}

#-

   global HELPS
   global HELP
   global F4
   global HSTRUT
   global HLAB
   global HAREA

   if { ![winfo exists $HLAB] } { return }

# If the vertical strut which stops the help area collapsing when the
# the number of lines in the help area reduces, is smaller than the
# current height of the help area, extend it.

   if { $F4 != "" && $HAREA } {
      set whgt [winfo height $HLAB]
      set shgt [winfo height $HSTRUT]
      if { $shgt < $whgt } {
         $HSTRUT configure -height $whgt
      }
   }

# This function is usually invoked when the pointer enters or leaves a
# widget. The identification of the widget is not reliable if the pointer
# is on the boundary, so pause for 10 milliseconds to allow the pointer
# to get away from the boundary.
   after 10

# Find the lowest level widget under the pointer.
   set x [winfo pointerx .]
   set y [winfo pointery .]
   set w [winfo containing $x $y]

# Check all the ancestors of this widget. This loop will be broken out of when
# a widget is found which has an associated help message.
   while { $w != "" } {
      if { [info exists HELPS($w)] } {
         set HELP $HELPS($w)
         break
      }
      set w [winfo parent $w]
   }

# If no suitable widget was found, store a null help string.
   if { $w == "" } {
      set HELP ""
   }
}

proc LoadTask {task file} {
#+
#  Name:
#     LoadTask

#  Purpose:
#     Load an ADAM task so that it can be used.

#  Language:
#     TCL

#  Arguments:
#     task
#      The name by which the task is to be known
#      (eg "kapview").
#     file
#      The file containing the executable image
#      (eg "/star/bin/kappa/kapview_mon").

#  Notes:
#     -  This procedure shuts down the whole application if the task
#     cannot be loaded.

#  Globals:
#     ADAM_TASKS (Write)
#       A list of the names of the ADAM tasks started up by stardemo.

#  Copyright:
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
#     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
#     02111-1307, USA

#  Authors:
#     {original_author_entry}

#  History:
#     {enter_changes_here}

#  Bugs:
#     {note_any_bugs_here}

#-
   global ADAM_TASKS
   global ADAM_USER
   global RENDEVOUS
   global TASK_FILE

# Load the task.
   set taskload [list adamtask $task $file ]
   if {[catch $taskload error] != 0} {
      puts "Error loading task $task (file $file): \"$error\". Aborting..."
      Message "Error loading task $task (file $file): \"$error\". Aborting..."
      exit 1
   }

# Poll for the task to attach to the message system.
   set count 0
   while {[$task path] == 0} {
      after 100
      incr count
      if {$count > 100} {
         puts "Timed out waiting for task \"$task\" (file $file) to start. Aborting..."
         Message "Timed out waiting for task \"$task\" (file $file) to start. Aborting..."
         catch {$task kill}
         exit 1
      }
   }

# Append the name of the task to the list of tasks started up so far.
   lappend ADAM_TASKS $task

# Save the name of the rendevous file.
   foreach rfile [glob -nocomplain $ADAM_USER/${task}_*] {
      if { [regexp "${task}_\[0-9\]+\$" $rfile] } {
         set RENDEVOUS($task) $rfile
         break
      }
   }

   if { ![info exists RENDEVOUS($task)] } {
      puts "Cannot find the rendevous file for $task."
      Message "Cannot find the rendevous file for $task."
      exit 1
   }
   set TASK_FILE($task) $file

}

proc MenuHelp {win label text} {
#+
#  Name:
#     MenuHelp

#  Purpose:
#     Establish the help text to display when the pointer is over
#     a specified entry in a specified menu.

#  Language:
#     TCL

#  Arguments:
#     win
#        The name of the menu.
#     label
#        The textual label for the menu entry.
#     text
#        The help information to display.

#  Globals:
#     MENUHELPS (Write)
#        A 2d array indexed by widget path and entry label, holding
#        the help text strings for all menu entries.

#  Copyright:
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
#     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
#     02111-1307, USA

#  Authors:
#     {original_author_entry}

#  History:
#     {enter_changes_here}

#  Bugs:
#     {note_any_bugs_here}

#-
   global MENUHELPS

# Store the supplied help text.
   set MENUHELPS($win,$label) $text

# Arrange for a blank help string to be displayed when the pointer
# initially enters the menu. This will be changed by the MenuMotionBind
# procedure.
   SetHelp $win ""
}

proc MenuMotionBind {win y} {
#+
#  Name:
#     MenuBind

#  Purpose:
#     Displays help as the pointer moves over a menu. It should be bound
#     to motion over all menus.

#  Language:
#     TCL

#  Arguments:
#     win
#        The name of the window currently under the pointer.
#     y
#        The y coordinate of the pointer.

#  Globals:
#     HELP (Write)
#        The current help text to display.
#     MENUHELPS (Read)
#        The help text for each entry of each menu.

#  Copyright:
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
#     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
#     02111-1307, USA

#  Authors:
#     {original_author_entry}

#  History:
#     {enter_changes_here}

#  Bugs:
#     {note_any_bugs_here}

#-
   global HELP
   global MENUHELPS

# Ignore separators and tearoffs
   set mty [$win type @$y]
   if { $mty != "separator" && $mty != "tearoff" } {

# Get the label from the menu entry under the pointer.
      set label [$win entrycget @$y -label]

# Get the help text associated with this menu entry
      if { [info exists MENUHELPS($win,$label)] } {
         set HELP $MENUHELPS($win,$label)
      } {
         set HELP ""
      }
   } {
      set HELP ""
   }
}

proc GetParamED {task param} {
#+
#  Name:
#     GetParamED

#  Purpose:
#     Returns the value of an ATASK parameter substituing "E" exponents for
#     "D" exponents.

#  Language:
#     TCL

#  Arguments:
#     task
#        The name of the task
#     param
#        The name of the parameter, in the form "<application>:<parameter>"

#  Returned Value:
#     The parameter value.

#  Copyright:
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
#     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
#     02111-1307, USA

#  Authors:
#     {original_author_entry}

#  History:
#     {enter_changes_here}

#  Bugs:
#     {note_any_bugs_here}

#-
   regsub -nocase -all D [GetParam $task $param] E res
   return $res
}

proc Obey {task action params args} {
#+
#  Name:
#     Obey

#  Purpose:
#     Executes an ADAM application.

#  Language:
#     TCL

#  Arguments:
#     task
#       The name of the task containing the application (eg "kapview").
#     action
#       The name of the application (eg "display").
#     params
#       Any command line parameter assignments to pass to the
#       application. A blank string must be supplied if no
#       command line parameter assignments are needed.
#     args
#       o  If the optional string "noreport" is supplied, then any error
#       messages generated by the action are not displayed.
#       o  If the name of a currently defined global variable is supplied,
#       then the variable is assumed to be a 1-D array, indexed by A-task
#       parameter name. The associated values are the values to supply for
#       the A-task's parameters if they are prompted for.
#       o  The presence of any other non-blank value after "params" causes
#       the whole TCL application to abort if the specified action
#       does not complete succesfully.

#  Returned Value:
#     If the application completes succesfully, then 1 is returned.
#     Otherwise, 0 is returned.

#  Notes:
#     - The Task must already have been loaded using LoadTask.
#     - Any error messages created by the action are displayed in a dialog
#     box, unless the optional argument "args" has the value "noreport".
#     - This procedure does not return until the application has finished.
#     In the mean time, the display is "frozen" so that no further actions
#     can be initiated.

#  Globals:
#     ACTION (Write)
#      Name of current action in the form "task:action".
#     ADAM_ERRORS (Write)
#      The messages from the most recent ADAM application to fail.
#     ATASK_OUTPUT (Write)
#       Any non-error messages generated by the action are appended to
#       this list. Each message is stored as a new element in the list.
#     STATUS (Write)
#      The status string returned by the action.

#  Copyright:
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
#     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
#     02111-1307, USA

#  Authors:
#     {original_author_entry}

#  History:
#     {enter_changes_here}

#  Bugs:
#     {note_any_bugs_here}

#-
   global ACTION
   global ADAM_ERRORS
   global ATASK_OUTPUT
   global CAN
   global CANCEL_OP
   global ALPHATEXT
   global LOGFILE_ID
   global STATUS
   global DOING
   global AF

# Return without action if the opoeration was canceleed.
   if { $CANCEL_OP } { return 0 }

# Store the current action being performed in global.
   set ACTION "$task:$action"
   set DOING "  % $action $params"

# Classify the optional argument (if supplied). By default, parameter
# requests are replied to be sending a PAR__NULL (!) value, errors do
# not cause the application to abort, and errors are reported.
   set param_req "$task paramreply %R \!"
   set abort 0
   set report 1

   if { $args != "" } {

# See if an array of parameter values has been supplied.
      upvar #0 $args plist
      if { [info exists plist] } {
         set param_req "if { \[info exists ${args}(%n)\] } {
                           $task paramreply %R \$${args}(%n)
                        } {
                           $task paramreply %R \!
                        }"

# Otherwise, see if error reports are to be ignored.
      } elseif { $args == "noreport" } {
         set report 0

# Otherwise, abort on an error.
      } elseif { [lindex $args 0] != "" } {
         set abort 1
      }
   }

# Check that the AMS rendevous file still exists. If it doesn,t kill the
# task and reload it.
   CheckRF $task

# Clear any current ADAM messages.
   set ADAM_ERRORS {}
   set ATASK_OUTPUT {}

# Write the command we are to obey to the log file if there is one.
   if { $LOGFILE_ID != "" } {
      puts $LOGFILE_ID "\n$task $action $params..."
   }

# Start the action. Any messages generated by the action are processed
# by procedure CheckMsg. Error messages are appended to ADAM_ERRORS, other
# messages are thrown away. Parameter requests are responded to by
# sending a null (!) value. The variable STATUS is set when the
# action completes.
   set STATUS ""
   $task obey $action $params -inform "CheckMsg $action %V" \
                      -endmsg {set STATUS "%S"} \
                      -paramreq $param_req

# Wait until the action is finished. Check that the Rendevous file exists
# every 200 milliseconds. If the WaitFor command aborts early, try
# re-executing the obey command. Do not re-execute the command if it was
# terminated due to a user requested cancel (indicated by CANCEL_OP being
# non-zero).
   if { ![WaitFor STATUS [list CheckRF $task] 200] && !$CANCEL_OP } {
      set ADAM_ERRORS {}
      set ATASK_OUTPUT {}

      if { $LOGFILE_ID != "" } {
         puts $LOGFILE_ID "\n$task $action $params..."
      }

      set STATUS ""
      $task obey $action $params -inform "CheckMsg $action %V" \
                         -endmsg {set STATUS "%S"} \
                         -paramreq $param_req

      if { ![WaitFor STATUS [list CheckRF $task] 200] && !$CANCEL_OP } {
         Message "Problems with rendevous file! Aborting..."
         exit 1
      }
   }

# Set the return status. If the final status does not contain the string
# DTASK__ACTCOMPLET, assume the action failed.
   if { ![regexp {DTASK__ACTCOMPLET} $STATUS] } {
      set ok 0
   } {
      set ok 1
   }

# Display any error messages.
   if { $report && $ADAM_ERRORS != "" } {
      Message "$task action \"$action\" reported:\n$ADAM_ERRORS"
   }

# If failure is fatal, shut down.
   if { !$ok && $abort } {exit 1}

#  Re-congigure the canvas item containing the atask standard output text.
   if { [info exists ALPHATEXT] } {
      $ALPHATEXT configure -state normal
      $ALPHATEXT delete 1.0 end
      $ALPHATEXT insert end "$ATASK_OUTPUT"
      $ALPHATEXT configure -state disabled
   }

# Indicate that we are no longer executing an ADAM action.
   set ACTION ""

# Return the status. Return zero if the operation was cancelled by the
# user.
   if { $CANCEL_OP } { set ok 0 }

   return $ok
}

proc Pop {stack args} {
#+
#  Name:
#     Pop

#  Purpose:
#     Returns and removes the top value in the supplied FILO stack.

#  Language:
#     TCL

#  Arguments:
#     stack
#       The name (NOT the value) of a global list variable holding the stack.
#       On exit, the list holds one less element than on entry.
#     args
#        An optional argument giving the number of levels to pop off the
#        stack. It defaults to 1. If it is supplied as -1, then the
#        the first (bottom) entry is returned and the stack is emptied. If it
#        is supplied as 0, then the top entry on the stack is returned, but
#        it is not removed from the stack.

#  Returned Value:
#     The required stack element, or an empty string if the supplied stack
#     was empty.

#  Copyright:
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
#     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
#     02111-1307, USA

#  Authors:
#     {original_author_entry}

#  History:
#     {enter_changes_here}

#  Bugs:
#     {note_any_bugs_here}

#-

   upvar #0 $stack stk

   if { $args == "" } {
      set levels 1
   } {
      set levels $args
   }

   if { $levels == -1 } {
      set ret [lindex $stk end]
      set stk ""

   } elseif { $levels == 0 } {
      set ret [lindex $stk 0]

   } {
      set ret [lindex $stk [expr $levels - 1] ]
      set stk [lrange $stk $levels end]
   }

   return $ret
}

proc Push {stack value} {
#+
#  Name:
#     Push

#  Purpose:
#     Enter a new value onto the top of the supplied FILO stack.

#  Language:
#     TCL

#  Arguments:
#     stack
#       The name (NOT the value) of a global list variable holding the stack.
#       On exit, the list holds one more element than on entry.
#     value
#       The value to be pushed onto stack.

#  Returned Value:
#     The supplied value.

#  Notes:
#     - The new entry is stored at index 0 in the list, and existing entries
#     are moved up to make room for it.

#  Copyright:
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
#     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
#     02111-1307, USA

#  Authors:
#     {original_author_entry}

#  History:
#     {enter_changes_here}

#  Bugs:
#     {note_any_bugs_here}

#-
   upvar #0 $stack stk
   set stk [linsert $stk 0 $value]
   return $value
}

proc SelectFont {font} {
#+
#  Name:
#     SelectFont

#  Purpose:
#     Pick a nice font matching the supplied font pattern.

#  Language:
#     TCL

#  Arguments:
#     font
#        A font pattern suitable for use with xlsfont.

#  Returned Value:
#     A specific font matching the the supplied pattern. The first
#     matching font returned by xlsfonts is used, with the proviso that
#     font families are searched in the following order:
#        helvetica
#        lucida
#        fixed
#        clean
#        courier
#        times
#        charter
#        new century schoolbook
#
#     A null string is returned if no matching font can be found belonging
#     to any of these families.

#  Copyright:
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
#     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
#     02111-1307, USA

#  Authors:
#     {original_author_entry}

#  History:
#     {enter_changes_here}

#  Bugs:
#     {note_any_bugs_here}

#-

# Initialise the returned font.
   set rfont ""

# Run xlsfonts to get a list of all matching fonts.
   if { ![catch "exec  xlsfonts -fn \"$font\"" fonts] } {

# Check each acceptable font family...
      foreach n [list helvetica lucida fixed clean courier times charter "new century schoolbook"] {

# Find the index of the first matching font in the current family. If
# found, get the full font name and leave the loop.
         set i 0
         while { $i > -1 } {
            set i [lsearch -regexp $fonts "^-\[^-\]+-$n" ]
            set rfont [lindex $fonts $i]
            if { ![catch "button .test -font $rfont"] } {
               destroy .test
               break
            } {
               set rfont ""
               set fonts [lreplace $fonts $i $i]
            }
         }
         if { $rfont != "" } { break }
      }

# If the xlsfonts command failed display the message.
   } {
      Message "An error occurred using xlsfonts to list fonts...\n $fonts"
   }

# Return the font.
   return $rfont
}

proc Seq {com delay id count} {
#+
#  Name:
#     Seq

#  Purpose:
#     Initiates a timed sequence of commands.

#  Language:
#     TCL

#  Arguments:
#     com
#       The command to execute in the timed sequence.
#     delay
#       The number of milliseconds between executions of the
#       command given by "com".
#     id
#       A string which can be used to identify the sequence.
#     count
#       The name (note, NOT the value) of a variable in which to write
#       the number of entries made into the command so far.

#  Notes:
#     -  The sequence can be terminated by setting the global variable
#     SEQ_STOP to the id supplied when the sequence was initiated.

#  Globals:
#     SEQ_STOP
#       If this is set to the id of the current sequence, then the
#       sequence is terminated, and SEQ_STOP is reset to an empty string.

#  Copyright:
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
#     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
#     02111-1307, USA

#  Authors:
#     {original_author_entry}

#  History:
#     {enter_changes_here}

#  Bugs:
#     {note_any_bugs_here}

#-
   global SEQ_STOP

   upvar $count cnt

   set cnt [expr $cnt + 1 ]

   if { $SEQ_STOP != $id } {
      eval "$com"
      after $delay [list Seq $com $delay $id $count]
   } {
      set SEQ_STOP 0
   }
}

proc SetHelp {widget help args} {
#+
#  Name:
#     SetHelp

#  Purpose:
#     Set the text to appear at the bottom of the screen when the pointer
#     passes over a specified widget.

#  Language:
#     TCL

#  Arguments:
#     widget
#       The name of the widget (eg ".fr1.button").
#     help
#       The text to display.
#     args
#       An optional htx cross-reference label to be associated with the
#       widget.

#  Globals:
#     HELP_LABELS (Write)
#       A 1-D array index by widget name. Each element is an htx
#       cross-reference label to be displayed if the widget is selected
#       using "Help on pointer".
#     HELPS (Write)
#       An array holding the current help text for each widget.

#  Copyright:
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
#     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
#     02111-1307, USA

#  Authors:
#     {original_author_entry}

#  History:
#     {enter_changes_here}

#  Bugs:
#     {note_any_bugs_here}

#-
   global HELPS
   global HELP_LABELS

# Store the supplied text.
   set HELPS($widget) $help

# Store the htx label for the widget (if any).
   if { $args != "" } {
      set HELP_LABELS($widget) $args
   }

# Ensure that the displayed help text is up-to-date.
   Helper

}

proc ShowHelp {label} {
#+
#  Name:
#     ShowHelp

#  Purpose:
#     Display help.

#  Language:
#     TCL

#  Arguments:
#     label
#        A label into the polka document.

#  Copyright:
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
#     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
#     02111-1307, USA

#  Authors:
#     {original_author_entry}

#  History:
#     {enter_changes_here}

#  Bugs:
#     {note_any_bugs_here}

#-
   global HELP_TEXT

   if { [info exists HELP_TEXT($label)] } {
      set mess $HELP_TEXT($label)
      if { [string trim $mess] == "" } {
         set mess "No help available."
      }
   } else {
      set mess "No help available."
   }

   Message $mess
}

proc Spacer {name h w} {
#+
#  Name:
#     Spacer

#  Purpose:
#     Create a blank object of fixed size to use as a spacer.

#  Language:
#     TCL

#  Arguments:
#     name
#        The path to the widget to be created.
#     h
#        The height required (eg "4m", etc).
#     w
#        The width required (eg "4m", etc).

#  Returned Value:
#     The path to the spacer object.

#  Copyright:
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
#     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
#     02111-1307, USA

#  Authors:
#     {original_author_entry}

#  History:
#     {enter_changes_here}

#  Bugs:
#     {note_any_bugs_here}

#-
   set spacer [frame $name -height $h -width $w ]
   pack propagate $spacer 0
   return $spacer
}

proc UniqueFile {} {
#+
#  Name:
#     UniqueFile

#  Purpose:
#     Returns a unique file name for which no file currently exists.
#     These files are created in the STARDEMO_SCRATCH directory
#     created by stardemo, and so do not need to be deleted when finished
#     with as they will all be deleted when the temporary ADAM_USER
#     directory is deleted when stardemo exits.

#  Language:
#     TCL

#  Arguments:
#     None.

#  Returned Value:
#     The file name.

#  Globals:
#     STARDEMO_SCRATCH (Read)
#        The path to the STARDEMO_SCRATCH directory.
#     IFILE (Read and Write)
#        File names have a name of the form polka<i> where <i> is an
#        integer, which is different for each file. IFILE
#        records the value of i used in the previous call to this
#        function. The first value of i considered is one greater than
#        that used last time.

#  Copyright:
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
#     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
#     02111-1307, USA

#  Authors:
#     {original_author_entry}

#  History:
#     {enter_changes_here}

#  Bugs:
#     {note_any_bugs_here}

#-
   global STARDEMO_SCRATCH
   global IFILE

   incr IFILE
   set file "$STARDEMO_SCRATCH/polka$IFILE"

   while { [llength [glob -nocomplain ${file}.*] ] != 0 } {
      incr IFILE
      set file "$STARDEMO_SCRATCH/polka$IFILE"
   }

   return $file
}

proc WaitFor {name args} {
#+
#  Name:
#     WaitFor

#  Purpose:
#     Pause the caller until a named global variable changes its value.
#     Meanwhile, events are directed to a nominated "safe" window. This
#     "freezes" the display so that further actions cannot be initiated by
#     the user

#  Language:
#     TCL

#  Arguments:
#     name
#        The name (NOT the value) of the global variable to be watched.
#     args
#        An optional list argument. If supplied, the first element should
#        be a command and the second element should be a time in milliseconds.
#        The supplied command will be executed after each period of the
#        specified time, until the variable is changed. If the delay time
#        is not supplied it defaults to 100 milliseconds. If the suppleid
#        command returns a zero value, then the loop is aborted prematurely.

#  Returned Value:
#     Zero if a supplied command returned a zero value or the
#     CANCEL_OP variable was set to a non-zero value (in which
#     case the delay is aborted prematurely), and one otherwise.

#  Notes:
#     - This procedure should be used in place of tkwait, which should NOT
#     be used.

#  Globals:
#     SAFE (Read)
#        The path to a window which can receive notifivcation of all events
#        while we are waiting. This should be a window which ignores all
#        events.

#  Copyright:
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
#     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
#     02111-1307, USA

#  Authors:
#     {original_author_entry}

#  History:
#     {enter_changes_here}

#  Bugs:
#     {note_any_bugs_here}

#-
   global CAN
   global SAFE
   global CANCEL_OP

# Access the supplied variable using the local name "VAR".
   upvar #0 $name VAR

# Save the original value of the global variable being watched.
   set orig $VAR

# Save the old cursors and switch on a "clock" cursor.
   set old_cursor [. cget -cursor]
   . config -cursor watch

   if { [info exists CAN] } {
      set old_cancur [$CAN cget -cursor]
      $CAN config -cursor watch
   }

# Indicate that no gran has yet been made by this procedure.
   set grabset 0

# See if any command has been supplied.
   set nargs [llength $args]
   if { $nargs > 0 } {
      set com [lindex $args 0]
      if { $nargs > 1 } {
         set delay [lindex $args 1]
      } {
         set delay 100
      }
   } {
      set com ""
      set delay 100
   }

# Wait until the variable changes value, or the operation is cancelled ...
   set ret 1
   while { $VAR == $orig } {

# Attempt to set a grab on a "safe" window so that all button
# presses and mouse movements will be ignored. If succesful, note
# that we will need to release the grab.
      if { !$grabset } {
         if { ![catch "grab set $SAFE"] } {
            set grabset 1
         }
      }

# Execute any supplied command.
      if { $com != "" } {
         set ret [eval "$com"]
         if { !$ret } { break }
      }

# Break out of the loop if CANCEL_OP was set to a non-zero value.
      if { $CANCEL_OP } { break }

# Pause and then repeat.
      after $delay {set a 1}
      tkwait variable a

   }

# Release the grab set above (if any).
   if { $grabset } {
      grab release $SAFE
   }

# Revert to the previous cursors.
   . config -cursor $old_cursor

   if { [info exists CAN] } {
      $CAN config -cursor $old_cancur
   }

# Return zero if the operation has been cancelled.
   if { $CANCEL_OP } { set ret 0 }

   return $ret
}

proc CommentaryArea {} {
#+
#  Name:
#     CommentaryArea

#  Purpose:
#     Create the frame displaying commentary information at the
#     bottom of the main window.

#  Language:
#     TCL

#  Arguments:
#     None.

#  Globals:
#     F3 (Read and Write)
#        The name of the frame to contain commentary information.
#     COM_FONT (Read)
#        The font in which to display commentary information.

#  Copyright:
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
#     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
#     02111-1307, USA

#  Authors:
#     {original_author_entry}

#  History:
#     {enter_changes_here}

#  Bugs:
#     {note_any_bugs_here}

#-
   global F3
   global COM_FONT
   global HD_FONT
   global TOP
   global HSTRUT
   global CLAB

# Create the frame (if it has not already been created).
   if { $F3== "" } {

# Create the frame to enclose the commentary text.
      set F3 [frame $TOP.com -relief groove -bd 2]
      pack $F3 -fill both -expand 1 -padx 1m -pady 1m

# The width is the requested width of the whole window.
      update idletasks
      set width [winfo width .]

# The height is the remaining available height within the top level
# container.
      set height [expr [winfo height .] - [winfo height $TOP]]

# Create a dummy frame with height but no width to act as a vertical strut.
# Geometry propagation is turn off for this frame so that its requested
# size will be retained even though nothing is put in the frame. This
# strut is used to keep the area the same size even if the message text
# within it requires a narrower area.
      set HSTRUT [Strut $F3 $height]

# Create a scroll bar to scroll the text.
      set sc [scrollbar $F3.sc -command "$F3.lab yview" -width 15 -relief sunken]
      pack $sc -side right -fill y

# Create a label widget to contain the current headline.
      set HDLAB [label $F3.hdlab -justify left -textvariable HEADLINE \
                    -anchor nw -font $HD_FONT -foreground "#a00" -width 3 ]
      pack $HDLAB -side top -fill x -padx 7m -pady 2m

# Create a text widget to display dynamic commentary information
      set CLAB [text $F3.lab -state disabled -relief flat -wrap word -bd 0 \
                -highlightthickness 0 -font $COM_FONT -width $width \
                -foreground "#000" \
                -yscrollcommand "$F3.sc set" -spacing1 1m -spacing2 1m]
      pack $CLAB -fill both -expand 1 -padx 7m -pady 4m

# Adjust the width of the message width so that it occupies exactly the
# full width available.
      update idletasks
      $CLAB configure -width [winfo width $CLAB]

# Set up the help for the commentary area.
      SetHelp $F3 "An area which describes what is currently happening." STARDEMO_COM_AREA
   }
}


proc LoadDemos {dir report} {

# Initialise the number of demos loaded to zero.
   set ret 0

# Get the names of all .demo files in the specified directory.
   set demofiles [glob -nocomplain "$dir/*.demo"]

# Issue a message if no .demo files were found.
   if { [llength $demofiles] == 0 } {
      if { $report } {
         Message "No .demo files found in $dir."
      }

# Otherwise loop round loading each demo.
   } else {
      foreach file $demofiles {
         incr ret [LoadDemo $file]
      }
   }

# Return the number of demos succesfully loaded.
   return $ret

}

proc LoadDemo {file} {
   if { [catch {set ret [LoadDemoC $file]} mess] } {
      Message "Error in demo file \"$file\": $mess"
      set ret 0
   }

   return $ret
}


proc LoadDemoC {file} {
   global CHECK_DEMO
   global DEMO_FILE
   global SELECTED_DEMOS
   global DEMO

# Assume an error.
   set ret 0

# Check the file can be read.
   if { ![file readable $file] } {
      Message "Cannot read demo file $file"

# If so, open the file.
   } {
      set fd [open $file r]

# Loop round reading the contents of the file into a single list.
# Ignore comment lines starting with a hash.
      while { [gets $fd line] != -1 } {
         if { ![regexp {^ *#} $line ] } {
            append demo_text $line
            append demo_text " "
         }
      }

#  Close the file.
      close $fd

      set CHECK_DEMO 1
      set DEMO_FILE $file

      set i 0
      while { $i < [llength $demo_text] } {
         set name ""
         set ignore 0

#  Check the syntax of the demo script and store it in common array DEMO.
         set do [lindex $demo_text $i]

         incr i
         if { [string tolower $do] == "demo" } {
            set name [lindex $demo_text $i]
            incr i

            if { [info exist DEMO($name)] } {

               set but [dialog .confirm "StarDemo Confirmation" \
                               "There is already a loaded demo called \"$name\". Should this be replaced by the one in file \"$file\"?" \
                               {} 0 Yes No Cancel]
               if { $but == 0 } {
                  unset DEMO($name)
                  set di [lsearch -exact $SELECTED_DEMOS $name]
                  if { $di != -1 } {
                     set SELECTED_DEMOS [lreplace $SELECTED_DEMOS $di $di]
                  }
               } elseif { $but == 1 } {
                  set ignore 1
               } {
                  break
               }
            }

            if { !$ignore } {
               if { ![Demo $name [lindex $demo_text $i] ] } {
                  if { [info exist DEMO($name)] } { unset DEMO($name) }
               } {
                  lappend SELECTED_DEMOS $name
                  incr ret
               }
            }
            incr i

         } elseif { [string tolower $do] == "link" } {
            set linkname [lindex $demo_text $i]
            incr i
            if { ![Link global $linkname [lindex $demo_text $i]] } {
               set ret 0
               break
            }
            incr i

         } elseif { [string tolower $do] == "package" } {
            Package $name [lindex $demo_text $i]
            incr i

         } {
            Message "Illegal top-level component \"$do\" found in demo file $file"
            break
         }

      }

      set CHECK_DEMO 0
      set DEMO_FILE ""
   }

   return $ret
}

proc Demo {name body} {
#+
#  Name:
#     Demo

#  Purpose:
#     Execute a demo. If CHECK_DEMO is set non-zero then the syntax
#     of the demo script is checked but the demo is not actually executed.
#     A demo script consists of a series of "steps" (see procedure Step).

#  Language:
#     TCL

#  Arguments:
#     name - The name of the demo
#     body - The body of the demo script stored as a Tcl list.

#  Returned Value:
#     1 of all went ok, otherwise zero.

#  Globals:
#     CHECK_DEMO (Read)
#        If non-zero then the syntax of the demo scriptis checked but the
#        demo is not actually executed.
#     DEMO_FILE (Read)
#        If non-blank, holds the name of the demo file being checked.
#     DEMO (Write)
#        An array indexed by demo name. Each element in the array
#        contains the corresponding demo script. The supplied demo is added
#        to the array if CHECK_DEMO is non-zero and the syntax of the
#        script is OK.

#  Copyright:
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
#     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
#     02111-1307, USA

#  Authors:
#     {original_author_entry}

#  History:
#     {enter_changes_here}

#  Bugs:
#     {note_any_bugs_here}

#-
   global CHECK_DEMO
   global DEMO
   global DEMO_FILE
   global DEMO_LINKS
   global ABORT_DEMO
   global PAUSE_DEMO
   global DEMO_INFO

   set ret 1

# If the demo is to be executed, first load the global link texts,
# and demo information for this demo.
   if { !$CHECK_DEMO } {
      Diag "Locating link texts within demo $name..."

# Remove any existing links with "demo" or "step" scope.
      RemoveLinks demo

# We do not yet have any demo information
      set DEMO_INFO ""

      for {set i 0} { $i < [llength $body] } { incr i } {
         set element [string tolower [lindex $body $i]]
         Diag "   $element"

         if { $ABORT_DEMO } { break }
         if { $PAUSE_DEMO } { tkwait variable PAUSE_DEMO }

         if { $element == "step" } {
            incr i

         } elseif { $element == "env" } {
            incr i

         } elseif { $element == "link" } {
            incr i
            set linkname [lindex $body $i]
            incr i
            if { ![Link demo $linkname [lindex $body $i]] } {
               set ret 0
               break
            }

         } elseif { $element == "info" } {
            incr i
            set DEMO_INFO [lindex $body $i]

         }
      }
   }

   if { $CHECK_DEMO } {
      Diag "Checking the syntax of the current step..."
   } {
      Diag "Executing the current step..."
   }

# Now run the demo, or check the demo.
   for {set i 0} { $i < [llength $body] } { incr i } {
      set element [string tolower [lindex $body $i]]
      Diag "   $element"

      if { $ABORT_DEMO } { break }
      if { $PAUSE_DEMO } { tkwait variable PAUSE_DEMO }

      if { $element == "step" } {
         incr i
         if { ![Step $name [lindex $body $i]] } {
            set ret 0
            break
         }

      } elseif { $element == "env" } {
         incr i
         if { ![GetEnv [lindex $body $i]] } {
            set ret 0
            break
         }

      } elseif { $element == "link" } {
         incr i 2

      } elseif { $element == "info" } {
         incr i

      } elseif { $element != "" } {
         set mess "Illegal component \"$element\" found in demo $name"
         if { [info exists DEMO_FILE] } {
            if { $DEMO_FILE != "" } {
               append mess " (file $DEMO_FILE)"
            }
         }
         Message $mess
         set ret 0
         break

      }
   }

   if { $ret && $CHECK_DEMO } {
      set DEMO($name) $body
   }

   return $ret

}

proc Step {demo body} {
#+
#  Name:
#     Step

#  Purpose:
#     Execute a single step in a demo. If CHECK_DEMO is set non-zero then
#     the syntax of the step is checked but it is not actually executed.
#     A step consists of:
#       - a command to be executed
#       - some commentary text to be displayed describing the command
#       - a pause to be waited after the command has been executed (given
#         as a number of seconds)
#       - etc.

#  Language:
#     TCL

#  Arguments:
#     demo - The name of the demo to which this step belongs
#     body - The body of the step stored as a Tcl list.

#  Returned Value:
#     1 of all went ok, otherwise zero.

#  Globals:
#     DEMO_FILE (Read)
#        If non-blank, holds the name of the demo file being checked.

#  Copyright:
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
#     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
#     02111-1307, USA

#  Authors:
#     {original_author_entry}

#  History:
#     {enter_changes_here}

#  Bugs:
#     {note_any_bugs_here}

#-
   global DEMO_FILE
   global ABORT_DEMO
   global PAUSE_DEMO
   global CHECK_DEMO
   global STEP_LINKS
   global CLAB

   set ret 1


# If the step is to be executed, first check for step link texts.
   if { !$CHECK_DEMO } {
      Diag "Locating link texts within current step..."

# Remove any existing links with "step" scope.
      RemoveLinks step

      for {set i 0} { $i < [llength $body] } {incr i} {
         set element [string tolower [lindex $body $i]]
         Diag "   $element"

         if { $ABORT_DEMO } { break }
         if { $PAUSE_DEMO } { tkwait variable PAUSE_DEMO }

         if { $element == "command" } {
            incr i

         } elseif { $element == "text" } {
            incr i

         } elseif { $element == "pause" } {
            incr i

         } elseif { $element == "alpha" } {
            incr i

         } elseif { $element == "head" } {
            incr i

         } elseif { $element == "link" } {
            incr i
            set linkname [lindex $body $i]
            incr i
            if { ![Link step $linkname [lindex $body $i]] } {
               set ret 0
               break
            }
         }
      }

      TextTags $CLAB step
   }

# Now execute or check the step.
   if { $CHECK_DEMO } {
      Diag "Checking the syntax of the current step..."
   } {
      Diag "Executing the current step..."
   }

   for {set i 0} { $i < [llength $body] } {incr i} {
      set element [string tolower [lindex $body $i]]
      Diag "   $element"

      if { $ABORT_DEMO } { break }
      if { $PAUSE_DEMO } { tkwait variable PAUSE_DEMO }

      if { $element == "command" } {
         incr i
         if { ![Command [lindex $body $i]] } {
            set ret 0
            break
         }

      } elseif { $element == "text" } {
         incr i
         if { ![Text [lindex $body $i]] } {
            set ret 0
            break
         }

      } elseif { $element == "head" } {
         incr i
         if { ![Head [lindex $body $i]] } {
            set ret 0
            break
         }

      } elseif { $element == "pause" } {
         incr i
         if { ![Pause [lindex $body $i]] } {
            set ret 0
            break
         }

      } elseif { $element == "alpha" } {
         incr i
         if { ![Alpha [lindex $body $i]] } {
            set ret 0
            break
         }

      } elseif { $element == "link" } {
         incr i 2

      } {
         set mess "Illegal step component \"$element\" found in demo $demo"
         if { [info exists DEMO_FILE] } {
            if { $DEMO_FILE != "" } {
               append mess " (file $DEMO_FILE)"
            }
         }
         Message $mess
         set ret 0
         break

      }
   }

#  Reset the Adam Message System to prevent AMS trampling over the
#  monoliths when some (unknown) internal limit is exceeded (this would
#  result in the monolith dying and a "rendevouz file lost" message).
#  Amongst other things, this throws away all parameter defaults and clears
#  the screen.
   AdamReset

   return $ret

}

proc Package {demo body} {
#+
#  Name:
#     Package

#  Purpose:
#     Store information describing the package being demonstrated.

#  Language:
#     TCL

#  Arguments:
#     demo - The name of the demo to which this step belongs
#     body - The body of the step stored as a Tcl list.

#  Returned Value:
#     1 of all went ok, otherwise zero.

#  Globals:
#     DEMO_FILE (Read)
#        If non-blank, holds the name of the demo file being checked.

#  Copyright:
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
#     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
#     02111-1307, USA

#  Authors:
#     {original_author_entry}

#  History:
#     {enter_changes_here}

#  Bugs:
#     {note_any_bugs_here}

#-
   global DEMO_FILE
   global CHECK_DEMO
   global PACKAGE
   global SUN
   global PACKAGE_DESCRIPTION
   global PACKAGE_VERSION
   global PACKAGE_EMAIL
   global PACKAGE_URL
   global PACKAGE_TITLE

   set ret 1

   Diag "Loading package information for $demo..."

# Do nothing if the demo is being executed.
   if { $CHECK_DEMO } {

      for {set i 0} { $i < [llength $body] } {incr i} {
         set element [string tolower [lindex $body $i]]
         incr i

         if { $element == "name" } {
            set PACKAGE [lindex $body $i]
            set PACKAGE_TITLE "The Starlink\n$PACKAGE package"

         } elseif { $element == "sun" } {
            set SUN [lindex $body $i]

         } elseif { $element == "info" } {
            set PACKAGE_DESCRIPTION [lindex $body $i]

         } elseif { $element == "version" } {
            set PACKAGE_VERSION [lindex $body $i]

         } elseif { $element == "email" } {
            set PACKAGE_EMAIL [lindex $body $i]

         } elseif { $element == "url" } {
            set PACKAGE_URL [lindex $body $i]

         } elseif { $element == "link" } {
            set linkname [lindex $body $i]
            incr i
            Link package $linkname [lindex $body $i]

         } elseif { $element == "monolith" } {
            set task [lindex $body $i]
            incr i
            set templ [lindex $body $i]
            if { ![catch {exec sh -c "ls $templ"} file] } {
               LoadTask $task [lindex $file 0]
            } {
               Message "Cannot find $task executable file \"$templ\""
               exit
            }

         } {
            set mess "Illegal package component \"$element\" found in demo $demo"
            if { [info exists DEMO_FILE] } {
               if { $DEMO_FILE != "" } {
                  append mess " (file $DEMO_FILE)"
               }
            }
            Message $mess
            set ret 0
            break
         }
      }
   }

   return $ret

}

proc Command {command} {
#+
#  Name:
#     Command

#  Purpose:
#     Execute a single command in a demo. If CHECK_DEMO is set non-zero then
#     the command is not actually executed.

#  Language:
#     TCL

#  Arguments:
#     command - The command.

#  Returned Value:
#     1 of all went ok, otherwise zero.

#  Globals:
#     CHECK_DEMO (Read)
#        If non-zero then the syntax of the demo scriptis checked but the
#        demo is not actually executed.

#  Copyright:
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
#     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
#     02111-1307, USA

#  Authors:
#     {original_author_entry}

#  History:
#     {enter_changes_here}

#  Bugs:
#     {note_any_bugs_here}

#-
   global CHECK_DEMO
   global ABORT_DEMO
   global ENV_VARS
   global env

   Diag "      $command"

   if { !$CHECK_DEMO && !$ABORT_DEMO } {

      foreach var $ENV_VARS {
         set $var $env($var)
      }

      if { [catch {eval $command} mess] } {
         Message "Error executing command \"$command\" - $mess"
      } else {
         Diag "         $mess"
      }
   }
   return 1
}

proc Text {text} {
#+
#  Name:
#     Text

#  Purpose:
#     Display commentary text for a single command in a demo. If CHECK_DEMO
#     is set non-zero then the text is not actually displayed.

#  Language:
#     TCL

#  Arguments:
#     text - The text

#  Returned Value:
#     1 of all went ok, otherwise zero.

#  Globals:
#     CHECK_DEMO (Read)
#        If non-zero then the syntax of the demo script is checked but the
#        demo is not actually executed.

#  Copyright:
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
#     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
#     02111-1307, USA

#  Authors:
#     {original_author_entry}

#  History:
#     {enter_changes_here}

#  Bugs:
#     {note_any_bugs_here}

#-
   global CHECK_DEMO
   global ABORT_DEMO

   if { !$CHECK_DEMO && !$ABORT_DEMO } { SetCom $text }
   return 1
}

proc Pause {time} {
#+
#  Name:
#     Pause

#  Purpose:
#     Pause for a given duration, or until a key is pressed. No pause
#     occurs if CHECK_DEMO is set non-zero.

#  Language:
#     TCL

#  Arguments:
#     time - The time to pause in seconds

#  Returned Value:
#     1 of all went ok, otherwise zero.

#  Globals:
#     CHECK_DEMO (Read)
#        If non-zero then the syntax of the demo script is checked but the
#        demo is not actually executed.

#  Copyright:
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
#     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
#     02111-1307, USA

#  Authors:
#     {original_author_entry}

#  History:
#     {enter_changes_here}

#  Bugs:
#     {note_any_bugs_here}

#-
   global CHECK_DEMO
   global PAUSE_DEMO
   global PAGING
   global TIMER
   global ABORT_DEMO
   global CONTINUE
   global TLEFT
   global PRESS
   global SPEED

   Diag "      $time"

#  Ensure the demo is being run, not just checked.
   if { !$CHECK_DEMO && !$ABORT_DEMO } {

# Automatic paging...
      if { !$PAGING } {
         set CONTINUE 0
         set TIMER 0
         while { $TIMER < [expr int($time*$SPEED) ] } {
            if { $CONTINUE || $PAUSE_DEMO || $ABORT_DEMO } { break }
            set TLEFT "[expr int($time*$SPEED)-$TIMER]"
            after 1000 "incr TIMER"
            tkwait variable TIMER
         }

         if { $PAUSE_DEMO } { tkwait variable PAUSE_DEMO }
         set TLEFT ""

# Manual paging...
      } {

# Add instructions to the commentary explaining how to resume the demo.
         set TLEFT ""
         set PRESS "Press any key to continue..."

# Set a flag to indicate the demo is paused, and wait for its state to
# change by ResumeDemo.
         set PAUSE_DEMO 1
         tkwait variable PAUSE_DEMO
      }
   }
   return 1
}

proc Select {} {
   global DONE
   global LB
   global DEMO
   global SELECTED_DEMOS
   global HELP_TEXT

# Create the top level window for the dialogue box, and set its title.
    set top .select
    set topf [MakeDialog $top "Select demonstrations" 0]

# Create and pack a frame for the "OK", "CLEAR ALL", "SET ALL", "RESET",
# "CANCEL" and "HELP" buttons at the left hand side.
    set f3 [frame $topf.f3]
    pack $f3 -side left -fill y -padx 2m -expand 1

# Create and pack a frame for the listbox title.
    set f4 [frame $topf.f4]
    pack $f4 -side top -fill x -padx 2m

# Create the label and pack it.
    set lab [label $f4.label -text "Loaded\ndemonstrations"]
    pack $lab -side left -padx 1m -pady 1m

# Create and pack the listbox and scroll bar.
    set LB [listbox $topf.lb -relief sunken -bd 2 -yscrollcommand \
           "$topf.sc set" -height 10  -exportselection no -selectmode multiple ]
    SetHelp $LB ".  Click on a demonstration to select or de-select it. More than one demo can be selected."

    set sc [scrollbar $topf.sc -command "$LB yview" -width 10]
    pack $LB $sc -side left -fill y -padx 1m -pady 1m

# Enter all loaded demos into the listbox.
    set demos [array names DEMO]
    foreach name $demos {
       $LB insert end $name

# Add this demo to the listbox selection if it is currently selected
       if { [lsearch -exact $SELECTED_DEMOS $name] != -1 } {
          $LB selection set end
       }
    }

# Create the buttons and pack them into the left hand frame.
    set b1 [button $f3.b1 -text "De-select All" -width 15 -command {$LB selection clear 0 end}]
    SetHelp $b1 ".  Press to de-select all currently selected demonstrations."

    set b2 [button $f3.b2 -text "Select All" -width 15 -command {$LB selection set 0 end}]
    SetHelp $b2 ".  Press to select all currently unselected demonstrations."

    set b3 [button $f3.b3 -text "Reset" -width 15 -command "
                       \$LB selection clear 0 end
                       set names \[array names DEMO\]
                       for {set i 0} { \$i < \[llength \$names\] } {incr i} {
                          if { \[lsearch -exact \$SELECTED_DEMOS \[lindex \$names \$i\]\] != -1 } {
                             \$LB selection set \$i
                          }
                       }"
               ]
    SetHelp $b3 ".  Press to re-instate the original selection of demonstrations."

    set b4 [button $f3.b4 -text "Cancel" -width 15 -command "set DONE 2"]
    SetHelp $b4 ".  Press to close the dialog, using the originally selected demonstations."

    set b5 [button $f3.b5 -text "OK" -width 15 -command "set DONE 1"]
    SetHelp $b5 ".  Press to close the dialog, using the currently selected demonstations."

    set b6 [button $f3.b6 -text "Help" -width 15 \
                      -command {ShowHelp "STARDEMO_SELECT_DIALOG" }]
    SetHelp $b6 ".  Display help information on the \"Select dmonstrations\" window."


    set HELP_TEXT(STARDEMO_SELECT_DIALOG) "This dialog allows you to select the demonstrations which are to be run when the \"Run\" button is pressed. One or more demonstrations can be selected from amongst all those which have been loaded (use \"File/Load\" to load demonstrations). Clicking on an entry toggles its selection state (the selected items have a black background - unselected items have a grey background)."

    pack $b1 $b2 $b3 $b4 $b5 $b6 -side bottom -pady 2m

# Ensure that closing the window from the window manager is like pressing
# the Cancel button.
    wm protocol $top WM_DELETE_WINDOW "set DONE 2"
    wm protocol $top WM_CLOSE_WINDOW "set DONE 2"
    wm protocol $top WM_KILL_WINDOW "set DONE 2"

# Loop until a valid label has been obtained.
    set DONE 0
    while { $DONE == 0 } {

# Wait for the user to make a selection.
       tkwait variable DONE

# If the OK button was pressed, copy the listbox selection to the
# SELECTED_DEMOS variable.
       if { $DONE == 1 } {
          set SELECTED_DEMOS ""
          foreach i [$LB curselection] {
             lappend SELECTED_DEMOS [lindex $demos $i]
          }
       }
    }

# Destroy the dialog box.
    destroy $top

}

proc MakeDialog {w title grab} {
#+
#  Name:
#     MakeDialog

#  Purpose:
#     Create an empty dialog box. It should be destroyed using
#     "destroy $w" when no longer needed.

#  Language:
#     TCL

#  Arguments:
#     w
#        The name of the toplevel window to create.
#     title
#        The title to display above the toplevel window.
#     grab
#        Should the toplevel window grab all X events?

#  Returned Value:
#     The path to a frame in which the dialog box components can be packed.

#  Globals:
#     TOP (Read)
#        The path to the main application window.

#  Copyright:
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
#     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
#     02111-1307, USA

#  Authors:
#     {original_author_entry}

#  History:
#     {enter_changes_here}

#  Bugs:
#     {note_any_bugs_here}

#-
   global TOP

# Create the top level window for the dialogue box, and set its title.
# It inherits the (potentially private) colour map used by the main
# application window.
   set top [toplevel $w -colormap $TOP]
   wm title $top "StarDemo - $title"

# Attempt put a grab on this window, so that other windows become
# inactive. This is a bit fragile so put the grab inside a catch so that
# an error in grab will not abort the application.
   if { $grab } { catch "grab $top" }

# Create a frame to hold everything else so that we can have a blank
# border round the other widgets.
   set topf0 [frame $top.f0 -bd 3 -relief raised]
   set topf [frame $topf0.f ]

# Pack the frame holding everything else.
   pack $topf -fill both -expand 1 -ipadx 2m -ipady 2m
   pack $topf0 -padx 2m -pady 2m -ipadx 2m -ipady 2m -fill both -expand 1

# Return the name of the frame to contain everything else.
   return $topf
}

proc LoadFromFile {file} {
#+
#  Name:
#     LoadFromFile

#  Purpose:
#     Open a disk file and read demonstration scripts from it.

#  Language:
#     TCL

#  Arguments:
#     file
#        If supplied non-blank, then the demos are read from the specified
#        file. Otherwise, the user is asked to supply a file name.

#  Returned Value:
#     The number of demo scripts which were loaded succesfully.

#  Copyright:
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
#     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
#     02111-1307, USA

#  Authors:
#     {original_author_entry}

#  History:
#     {enter_changes_here}

#  Bugs:
#     {note_any_bugs_here}

#-

# If no file was supplied, get one.
   if { $file == "" } {
      set ok [OpenFile "r" "Demo file" "Give name of demo file to read:" file lfd]
      if { $ok } { close $lfd }
   } {
      set ok 1
   }

# Only proceed if a file was opened.
   if { $ok } {
      set ret [LoadDemo $file]
   } {
      set ret 0
   }

   return $ret

}

proc OpenFile {mode title text lfile lfd} {
#+
#  Name:
#     OpenFile

#  Purpose:
#     Open a file for reading or writing, reporting any errors which occur.
#     The user is asked to conform that it is ok to over-write an existing
#     file.

#  Language:
#     TCL

#  Arguments:
#     mode
#        The access mode; "r" for read-only, anything else produces
#        write access.
#     title
#        The string to use as the dialog window title.
#     text
#        A string to display as a label above the entry widget.
#     lfile
#        The name of the variable to recieve the file name.
#     lfd
#        The name of the variable to recieve the file descriptor
#        for the opened file.

#  Returned Value:
#     Zero if no file was opened, one otherwise.

#  Copyright:
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
#     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
#     02111-1307, USA

#  Authors:
#     {original_author_entry}

#  History:
#     {enter_changes_here}

#  Bugs:
#     {note_any_bugs_here}

#-
   global OPFILE_EXIT

   upvar $lfd fd
   upvar $lfile file

# Assume no file is opened
   set ret 0

# Create the top level window for the dialogue box.
   set top .openfile
   set topf [MakeDialog $top $title 1]

# Pack a label displaying the supplied text (if any).
   if { $text != "" } {
      pack [label $topf.lab -text $text] -expand 1 -fill x -side top -pady 2m
   }

# Create and pack an entry widget
   set ent [entry $topf.ent -width 40]
   pack $ent -side top -expand 1 -fill x -pady 4m

# Pre-load the current value of the file name (if not blank).
   if { [info exists file] && $file != "" } { $ent insert 0 $file }

# Bind <Return> in the entry to the OK button.
   bind $ent <Return> "set OPFILE_EXIT ok"

# Give the focus to the entry.
   focus $ent

# Create the OK and Cancel buttons, but don't pack them yet.
   set butfrm [frame $topf.butfrm]
   set b1 [button $butfrm.ok -text "OK" -command "set OPFILE_EXIT ok"]
   set b3 [button $butfrm.cancel -text "Cancel" -command "set OPFILE_EXIT cancel"]

   SetHelp $b1 ".  Press to close the dialog box, adopting the currently displayed file name."
   SetHelp $b3 ".  Press to close the dialog box, cancelling the current operation."

# Now pack the nuttons so that they appear at the bottom of the dialog box.
   pack $butfrm -fill x -expand 1
   pack $b1 $b3 -side left -expand 1

# Ensure that closing the window from the window manager is like pressing
# the Cancel button.
   wm protocol $top WM_DELETE_WINDOW "set OPFILE_EXIT cancel"
   wm protocol $top WM_KILL_WINDOW "set OPFILE_EXIT cancel"
   wm protocol $top WM_CLOSE_WINDOW "set OPFILE_EXIT cancel"

# Loop until an exit button is pressed.
   set exit 0
   while { !$exit } {

# Wait for the user to press a button.
      tkwait variable OPFILE_EXIT

# If the cancel button was pressed, exit returning zero.
      if { $OPFILE_EXIT == "cancel" } {
         set exit 1

# If the OK button was pressed, attempt to open the file, and exit.
      } elseif { $OPFILE_EXIT == "ok" } {

# Get the file name from the entry.
         set file [$ent get]

# First deal with cases where the file is to be read.
         if { $mode == "r" } {

#  Attempt to open the file for reading. Catch any error which occurs.
            if { [catch {set fd [open $file "r"]} msg] } {
               Message "File \"$file\" cannot be read:\n\n\"$msg\""
            } {
               set ret 1
            }

# Now deal with cases where the file is to be written.
         } {

# If the file already exists, get the user to confirm that it is
# OK to over-write it. If so, attempt to opne it for writing. Report any
# error.
            if { ![file exists $file] || [Confirm "Over-write existing file \"$file\"?"] } {
               if { [catch {set fd [open $file "w"]} msg] } {
                  Message "File \"$file\" cannot be opened for writing:\n\n\"$msg\""
               } {
                  set ret 1
               }
            }
         }

# If the file was opened succesfully, indicate that the dialog box should be
# closed.
         if { $ret } { set exit 1 }

      }
   }

# Destroy the dialog box, and return.
   destroy $top
   return $ret
}


proc Delete {} {
   global DONE
   global LB
   global DEMO
   global SELECTED_DEMOS
   global HELP_TEXT

# Create the top level window for the dialogue box, and set its title.
    set top .select
    set topf [MakeDialog $top "Delete demonstrations" 0]

# Create and pack a frame for the "OK", "CLEAR ALL", "SET ALL", "RESET",
# "CANCEL" and "HELP" buttons at the left hand side.
    set f3 [frame $topf.f3]
    pack $f3 -side left -fill y -padx 2m -expand 1

# Create and pack a frame for the listbox title.
    set f4 [frame $topf.f4]
    pack $f4 -side top -fill x -padx 2m

# Create the label and pack it.
    set lab [label $f4.label -text "Loaded\ndemonstrations"]
    pack $lab -side left -padx 1m -pady 1m

# Create and pack the listbox and scroll bar.
    set LB [listbox $topf.lb -relief sunken -bd 2 -yscrollcommand \
           "$topf.sc set" -height 10  -exportselection no -selectmode multiple ]
    SetHelp $LB ".  Click on a demonstration to select or de-select it. More than one demo can be selected."

    set sc [scrollbar $topf.sc -command "$LB yview" -width 10]
    pack $LB $sc -side left -fill y -padx 1m -pady 1m

# Enter all loaded demos into the listbox.
    set demos [array names DEMO]
    foreach name $demos { $LB insert end $name }

# Create the buttons and pack them into the left hand frame.
    set b1 [button $f3.b1 -text "De-select All" -width 15 -command {$LB selection clear 0 end}]
    SetHelp $b1 ".  Press to de-select all currently selected demonstrations."

    set b2 [button $f3.b2 -text "Select All" -width 15 -command {$LB selection set 0 end}]
    SetHelp $b2 ".  Press to select all currently unselected demonstrations."

    set b4 [button $f3.b4 -text "Cancel" -width 15 -command "set DONE 2"]
    SetHelp $b4 ".  Press to close the dialog, retaining all loaded demonstations."

    set b5 [button $f3.b5 -text "OK" -width 15 -command "set DONE 1"]
    SetHelp $b5 ".  Press to close the dialog, deleting the currently selected demonstations."

    set b6 [button $f3.b6 -text "Help" -width 15 \
                      -command {ShowHelp "STARDEMO_DELETE_DIALOG" }]
    SetHelp $b6 ".  Display help information on the \"Delete dmonstrations\" window."

    set HELP_TEXT(STARDEMO_DELETE_DIALOG) "This dialog allows you to delete demonstrations (i.e. remove them from the list of currently loaded demonstrations). Select the demonstrations to be deleted and then press OK. Clicking on an entry toggles its selection state."

    pack $b1 $b2 $b4 $b5 $b6 -side bottom -pady 2m

# Ensure that closing the window from the window manager is like pressing
# the Cancel button.
    wm protocol $top WM_DELETE_WINDOW "set DONE 2"
    wm protocol $top WM_CLOSE_WINDOW "set DONE 2"
    wm protocol $top WM_KILL_WINDOW "set DONE 2"

# Loop until a valid label has been obtained.
    set DONE 0
    while { $DONE == 0 } {

# Wait for the user to make a selection.
       tkwait variable DONE

# If the OK button was pressed, unset the DEMO array elements holding the
# selected demos.
       if { $DONE == 1 } {
          foreach i [$LB curselection] {
             set name [lindex $demos $i]
             unset DEMO($name)
             set di [lsearch -exact $SELECTED_DEMOS $name]
             if { $di != -1 } {
                set SELECTED_DEMOS [lreplace $SELECTED_DEMOS $di $di]
             }
          }
       }
    }

# Destroy the dialog box.
    destroy $top

}


proc Run {} {
   global DEMO
   global ADAM_USER
   global DEVICE
   global RUNNING_DEMO
   global SELECTED_DEMOS
   global ABORT_DEMO
   global LOOPING
   global DOING
   global SELECT
   global RUN
   global NEXT
   global ABORT
   global PAUSE
   global CAN
   global ADAM_TASKS
   global TASK_FILE
   global HEADLINE

#  Cancel the binding for Configure so that the configuration changes
#  do not result in teh GWM display being re-created.
   set bs [bind . <Configure>]
   bind . <Configure> ""

   $SELECT configure -state disabled
   $RUN configure -state disabled
   $NEXT configure -state normal
   $ABORT configure -state normal
   $PAUSE configure -state normal

   set first 1
   while { $first || $LOOPING } {

      foreach name $SELECTED_DEMOS {

         set ABORT_DEMO 0

         SetCom ""
         set HEADLINE "Starting demonstration \"$name\"..."

         set a 0
         after 2000 "set a 1"
         tkwait variable a

         set HEADLINE ""
         set RUNNING_DEMO $name
         Demo $name $DEMO($name)

         set RUNNING_DEMO "<idle>"
         set DOING ""

         SetCom ""
         set HEADLINE "Demonstration \"$name\" has now finished."

         set a 0
         after 2000 "set a 1"
         tkwait variable a

         set HEADLINE ""

         set first 0
         if { $ABORT_DEMO == 1 } { break }


      }

      if { $ABORT_DEMO == 1 } { break }

   }

   $SELECT configure -state normal
   $RUN configure -state normal
   $NEXT configure -state disabled
   $ABORT configure -state disabled
   $PAUSE configure -state disabled

# Re-instate the original re-configuration script.
   bind . <Configure> $bs

}

proc Abort {} {
   global ABORT_DEMO
   global TIMED_PAUSE
   global PAUSE_DEMO
   global HEADLINE

   if { $PAUSE_DEMO } { ResumeDemo }

   set ABORT_DEMO 1
   set TIMED_PAUSE 0
   set HEADLINE "Aborting demonstration..."
   SetCom ""

}

proc Next {} {
   global ABORT_DEMO
   global PAUSE_DEMO
   global HEADLINE

   if { $PAUSE_DEMO } { ResumeDemo }

   set ABORT_DEMO 2
   set HEADLINE "Moving on to the next demonstration..."
   SetCom ""

}

proc PauseDemo {} {
   global PAUSE_DEMO
   global PAUSE
   global TLEFT
   global PRESS

# Set a global flag which causes the demo procedures to pause, untill the
# flag changes value again.
   set PAUSE_DEMO 1

# Add instructions to the commentary explaining how to resume the demo.
   set TLEFT ""
   set PRESS "Press any key to continue..."

# Disable the pause button.
   $PAUSE configure -state disabled

}

proc ResumeDemo {} {
   global PAUSE
   global PAUSE_DEMO
   global PRESS
   global CONTINUE

# If the demo is paused...
   if { $PAUSE_DEMO } {

# Clear the "Press any key to continue" message.
      set PRESS ""

# Enable the pause button.
      $PAUSE configure -state normal

# Change the state of the global variable PAUSE_DEMO so that
# the tkwaits which are waiting for this variable to change are activated.
      set PAUSE_DEMO 0

# Otherwise abandon the current timed pause immediately.
   } {
      set CONTINUE 1
   }

}


proc Resize { init } {
#+
#  Name:
#     Resize

#  Purpose:
#     Respond to an interactive re-size of the window by replacing the
#     GWM canvas item with a new one with an appropriate new size.

#  Language:
#     TCL

#  Arguments:
#     None

#  Returned Value:
#     None

#  Copyright:
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
#     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
#     02111-1307, USA

#  Authors:
#     {original_author_entry}

#  History:
#     {enter_changes_here}

#  Bugs:
#     {note_any_bugs_here}

#-
   global GWM
   global CAN
   global GWM_NAME
   global COLOURS
   global CANWID
   global CANHGT
   global DEVICE
   global DOING

#Message "Re-sizing image display"

#  Cancel the binding for Configure so that the configuration changes
#  produced by this procedure do not cause an infinite loop.
   set com [bind . <Configure>]
   bind . <Configure> ""

# Get the new size for the GWM canvas item.
   update
   set CANWID [expr [winfo width $CAN] - 8]
   set CANHGT [expr [winfo height $CAN] - 8]

#  Erase the old GWM
   $CAN delete $GWM

#  Create the new GWM canvas.
   if { [catch {set GWM [$CAN create gwm 4 4 -height $CANHGT \
                                 -width $CANWID -name $GWM_NAME \
                                 -mincolours $COLOURS -tags gwm]} mess] } {

# If this failed, report an error, and exit.
       Message "Failed to resize the image display.\n\n$mess"
       exit
   }

# Establish the graphics and image display devices.
   if { $init } {
      Obey kapview lutable "mapping=linear coltab=grey device=$DEVICE" 1
      Obey kapview paldef "device=$DEVICE" 1
      Obey kapview gdclear "device=$DEVICE" 1
      Obey kapview gdset "device=$DEVICE" 1
   }
   set DOING ""

#  Re-instate the original binding for Configure.
   bind . <Configure> $com

}

proc SetCom {com args} {
   global CLAB
   global TEXT_TAGS

# Replace multiple spaces with single spaces.
   regsub -all { +} $com { } com2

#  Enable the text widget.
   $CLAB configure -state normal

# If required, remove the current contexts of the text widget, and return
# the index of the first new character.
   if { $args == "" } {
      $CLAB delete 1.0 end
      set ret 1.0
   } {
      set ret [$CLAB index end]
   }

# Display the text in the text widget, assigning tags to tagged text.
   DispText $CLAB $com

   return $ret

}

proc AdamReset {} {
   global CHECK_DEMO
   global ADAM_TASKS
   global ADAM_USER
   global DEVICE
   global TASK_FILE

   if { !$CHECK_DEMO } {

#  Kill all tasks
      foreach task $ADAM_TASKS {
         catch {$task kill}
      }

#  Record the tasks which are to be re-instated later and then empty the
#  currently list of active tasks.
      set tasks $ADAM_TASKS
      set ADAM_TASKS ""

#  Shutdown Startcl
      upvar #0 adamtask_priv priv
      fileevent $priv(PIPE) readable ""
      adam_reply $priv(RELAY_PATH) $priv(RELAY_MESSID) SYNC "" exit
      close $priv(PIPE)
      unset priv
      rename exit {}
      rename adamtask.realExit exit

#  Kill all processes created by this script so far.
      killNew

#  Delete the temporary ADAM_USER directory created at the start, and
#  create a new one.
      if { [file exists $ADAM_USER] } {
         catch {exec rm -r -f $ADAM_USER}
      }
      catch {exec mkdir -p $ADAM_USER}

#  Pause
      after 1000

#  Start up Startcl
      adamtask.init
      bind . <Destroy> ""

#  Re-load the tasks.
      foreach task $tasks {
         LoadTask $task $TASK_FILE($task)
      }

#  Re-establish the graphics devices.
      after 1000
      Obey kapview gdset "device=$DEVICE" 1

      after 1000
      Obey kapview gdclear ""

   }
   return 1
}

proc Head {text} {
   global ABORT_DEMO
   global CHECK_DEMO
   global HEADLINE

   Diag "      $text"

   if { !$CHECK_DEMO && !$ABORT_DEMO } {
      set HEADLINE $text
   }

   return 1
}

proc Alpha {state} {
   global CHECK_DEMO
   global DEMO_FILE
   global AF

   Diag "      $state"

   set ret 1

   if { !$CHECK_DEMO } {

      if { $state == "on" } {
         MakeAlpha

      } elseif { $state == "off" } {
         if { [info exists AF] && [winfo exists $AF] } {
            wm withdraw $AF
         }

      } {
         if { [info exists DEMO_FILE] } {
            set mess "Illegal alpha state \"$state\" found in demonstration file $DEMO_FILE"
         } {
            set mess "Illegal alpha state \"$state\" found in demonstration file."
         }
         Message $mess
         set ret 0
      }
   }
   return $ret
}

proc Info {} {
   global PACKAGE
   global PACKAGE_VERSION
   global PACKAGE_EMAIL
   global PACKAGE_URL
   global SUN
   global PACKAGE_DESCRIPTION

   set mess ""

   if { $PACKAGE != "" } {
      set mess "The <emph Starlink> <pack $PACKAGE> package"
   }

   if { $PACKAGE_VERSION != "" } {
      append mess " - version $PACKAGE_VERSION"
   }

   if { $mess != "" } {append mess ".\n"}

   if { $PACKAGE_EMAIL != "" } {append mess "\nE-mail: $PACKAGE_EMAIL"}
   if { $PACKAGE_URL != "" }   {append mess "\nWWW: <$PACKAGE_URL $PACKAGE_URL>"}

   if { $mess != "" } {append mess "\n\n"}

   if { $PACKAGE_DESCRIPTION != "" } {append mess $PACKAGE_DESCRIPTION}

   if { $mess == "" } {
      Message "No further information is available."
   } {
      ShowText "Package info" $mess global
   }
}

proc Link {scope name text} {
   global STEP_LINKS
   global DEMO_LINKS
   global PACKAGE_LINKS
   global GLOBAL_LINKS

   if { $scope == "global" } {
      set GLOBAL_LINKS([string toupper $name]) $text

   } elseif { $scope == "demo" } {
      set DEMO_LINKS([string toupper $name]) $text

   } elseif { $scope == "package" } {
      set PACKAGE_LINKS([string toupper $name]) $text

   } {
      set STEP_LINKS([string toupper $name]) $text
   }

   return 1

}

proc ShowLink {scope link} {
   global DEMO_FILE
   global GLOBAL_LINKS
   global PACKAGE_LINKS
   global DEMO_LINKS
   global STEP_LINKS

# If this is a URL, display it.
   if { $scope == "url" } {
      CCDShowHelp $link

# If this is a link to an htx document, split the link up into doc and
# label (delimited by a vertical bar), and display it.
   } elseif { $scope == "htx" } {

      if { [regexp {(.*)\|(.*)} $link match doc label] } {
         ShowMe $doc $label
      } {
         ShowMe $link
      }

#  Otherwise...
   } {

# Indicate we do not yet have any text to display.
      set mess ""

# Get the text to display.
      if { $scope == "global" } {
         if { [info exists GLOBAL_LINKS($link)] } {
            set mess $GLOBAL_LINKS($link)
         }

      } elseif { $scope == "demo" } {
         if { [info exists DEMO_LINKS($link)] } {
            set mess $DEMO_LINKS($link)
         }

      } elseif { $scope == "package" } {
         if { [info exists PACKAGE_LINKS($link)] } {
            set mess $PACKAGE_LINKS($link)
         }

      } {
         if { [info exists STEP_LINKS($link)] } {
            set mess $STEP_LINKS($link)
         }
      }

# If defined, display it in a new top level window.
      if { $mess != "" } {
         ShowText $link $mess $scope

# Otherwise, display a message.
      } {
         set mess "No text is defined for this link ($link) within the demo file."

         if { [info exists DEMO_FILE] } {
            if { $DEMO_FILE != "" } {
               set mess "No text is defined for this link ($link) within the file $DEMO_FILE."
            }
         }

         Message $mess
      }
   }
}

proc ShowText {name mess scope args} {
   global COM_FONT
   global TEXTDONE

# Get a version of the supplied name without any spaces, etc.
   regsub -all " " $name "" res
   set label [string tolower $res]

# Return if the window is already visible.
   set top ".$label"
   if { [winfo exists $top] } {
      raise $top
      return
   }

# Create the top level window for the dialogue box, and set its title.
   set topf [MakeDialog $top $name 0]
   pack $topf -ipadx 3m -ipady 3m

# Create a Frame to hold the buttons.
   set bf [frame $topf.bf]

# Create and pack the OK button.
   set b1 [button $bf.b1 -text "OK" -command "set TEXTDONE($label) 1"]
   SetHelp $b1 ".  Press to close this window."
   pack $b1 -side left -pady 2m -padx 4m

# Create any additional buttons.
   for {set i 0} {$i < [llength $args]} {incr i} {
      set name [lindex $args $i]
      incr i
      set command [lindex $args $i]
      set but [button $bf.ab$i -text $name -command $command]
      pack $but -side left -pady 2m -padx 4m
   }

   pack $bf -side bottom

# Create a scroll bar to scroll the text.
   set tf [frame $topf.tf -relief groove -bd 2]
   pack $tf -side top -fill both -expand 1 -ipadx 3m -ipady 3m

   set sc [scrollbar $tf.sc -command "$tf.lab yview" -width 15 -relief sunken]
   pack $sc -side right -fill y

# Create the text widget.
   set text [text $tf.lab -state disabled -relief flat -wrap word -bd 0 \
                   -highlightthickness 0 -font $COM_FONT -width 60 \
                   -foreground "#000" \
                   -height 20  -yscrollcommand "$tf.sc set"]
   pack $text -side left -fill both -expand 1 -padx 5m -pady 5m

# Ensure that closing the window from the window manager is like pressing
# the OK button.
    wm protocol $top WM_DELETE_WINDOW "set TEXTDONE($label) 1"
    wm protocol $top WM_CLOSE_WINDOW "set TEXTDONE($label) 1"
    wm protocol $top WM_KILL_WINDOW "set TEXTDONE($label) 1"

# Establish the text tag characteristics.
    TextTags $text $scope

# Display the text.
    DispText $text $mess

# Wait for the user to press OK.
    tkwait variable TEXTDONE($label)

# Destroy the dialog box.
    unset TEXTDONE($label)
    destroy $top
}

proc DispText {widget text} {
   global TEXT_TAGS

# Enable the widget.
   $widget configure -state normal

# Loop round every character in the supplied string, looking for opening
# and closing tag specifiers.
   set state 0
   set line ""
   set TEXT_TAGS ""

   foreach c [split $text {}] {

# "<" marks an opening tag specifier. Indicate that the next word
# (potentially following 1 or more spaces) will be a tag name. Since we
# are about to change the tags list, output the current text to the text
# widget, using the current list of tags.
      if { $c == "<" } {
         set state 1
         InsertText $widget $line $TEXT_TAGS
         set line ""

# ">" marks a closing tag specifier. Remove the most recent tag from the
# stack of tag names. Before doing this, display the current line of text
# with the current tags.
      } elseif { $c == ">" } {
         InsertText $widget $line $TEXT_TAGS
         set line ""
         Pop TEXT_TAGS

# For any other character...
      } {

# State 1 means "we are waiting for the first non-blank character
# following an opening tag specifier". When found, store the non-blank
# character as the first character in the tag name, and indicate that we
# are now in state 2.
         if { $state == 1 } {
            if { $c != " " } {
               set tag $c
               set state 2
            }

# State 2 means "we are waiting for the first blank character
# following a tag name". When found, push the tag name onto the stack of
# tag names, and indicate that we are now in state 0. Until then, just
# keep on appending the non-blank characters to the end of the current tag
# name.
         } elseif { $state == 2 } {
            if { $c == " " } {
               set newtag [CheckTag $widget $tag]
               Push TEXT_TAGS $newtag
               set state 0
            } {
               append tag $c
            }

# State 0 means "we are copying simple text". Just append the character to the
# end of the current line of text.
         } {
            append line $c
         }
      }
   }

# Display any remaining text.
   InsertText $widget $line ""

#  Prevent the user from changing the text.
   $widget configure -state disabled

}


proc TextTags {widget scope} {
   global PACKAGE_LINKS
   global GLOBAL_LINKS
   global DEMO_LINKS
   global STEP_LINKS
   global LINKCOL

# Delete all existing tags
   foreach tag [$widget tag names] {
      $widget tag delete $tag
   }

# Find a factor to scale the screeen size by. A nominal screen size of
# 1200x900 is assumed.
   set fac [expr [winfo screenwidth .]/1200.0 ]
   set fac2 [expr [winfo screenheight .]/900.0 ]
   if { $fac2 < $fac } {
      set fac $fac2
   }

# Find the font sizes in pixels.
   set px180 [expr round( 18.0 * $fac ) ]
   set px140 [expr round( 14.0 * $fac ) ]
   set px120 [expr round( 12.0 * $fac ) ]
   set px100 [expr round( 10.0 * $fac ) ]

# Set the fonts corresponding to the fomatting tags which can be used
# within demo scripts.
   $widget tag configure ATTR    -font [SelectFont "-*-helvetica-medium-r-*-*-$px140-*-*-*-*-*-*-*"]
   $widget tag configure BUTTON  -font [SelectFont "-*-*-medium-r-*-*-$px120-*-*-*-*-*-*-*"]
   $widget tag configure COMMAND -font [SelectFont "-*-helvetica-medium-r-*-*-$px180-*-*-*-*-*-*-*"]
   $widget tag configure DOC     -font [SelectFont "-*-times-bold-i-*-*-$px180-*-*-*-*-*-*-*"]
   $widget tag configure EMPH    -font [SelectFont "-*-times-bold-i-*-*-$px180-*-*-*-*-*-*-*"]
   $widget tag configure FILE    -font [SelectFont "-*-helvetica-medium-r-*-*-$px140-*-*-*-*-*-*-*"]
   $widget tag configure FOR     -font [SelectFont "-*-times-bold-i-*-*-$px180-*-*-*-*-*-*-*"]
   $widget tag configure PACK    -font [SelectFont "-*-*-bold-i-*-*-$px140-*-*-*-*-*-*-*"]
   $widget tag configure PARAM   -font [SelectFont "-*-helvetica-bold-r-*-*-$px140-*-*-*-*-*-*-*"]
   $widget tag configure URL     -font [SelectFont "-*-helvetica-medium-r-*-*-$px140-*-*-*-*-*-*-*"]

# Initialise a list of the available link tags
   set tags ""

# Set up bindings which cause the link text to be displayed whenever text
# with a given tag is clicked on. Global links are visible from all scopes.
   foreach link [array names GLOBAL_LINKS] {
      $widget tag bind $link <Button> "ShowLink global $link"
      lappend tags $link
   }

# Demo links are visible from demo and step scope.
   if { $scope == "demo" || $scope == "step" } {
      foreach link [array names DEMO_LINKS] {
         $widget tag bind $link <Button> "ShowLink demo $link"
         lappend tags $link
      }
   }

# Step links are visible only from step scope.
   if { $scope == "step" } {
      foreach link [array names STEP_LINKS] {
         $widget tag bind $link <Button> "ShowLink step $link"
         lappend tags $link
      }
   }

# Package links are visible only from package scope.
   if { $scope == "package" } {
      foreach link [array names PACKAGE_LINKS] {
         $widget tag bind $link <Button> "ShowLink package $link"
         lappend tags $link
      }
   }

# Set up bindings which cause the cursor and help text to change whenever
# text with a given tag is clicked on. Also set the colour of link text.
   foreach link $tags {
      $widget tag configure $link -foreground $LINKCOL
      $widget tag bind $link <Enter> \
         "set OLDCURSOR \[$widget cget -cursor\]
          $widget config -cursor right_ptr
          set OLDHELP \$HELP
          set HELP \".   Click to display more information in a pop-up window.\"
          "
      $widget tag bind $link <Leave> \
         "set HELP \$OLDHELP
          $widget config -cursor \$OLDCURSOR"
   }

}

proc SetSpeed {logspeed} {
   global SPEED
   set SPEED [expr pow( 10.0, $logspeed )]
}

proc DemoInfo {} {
   global DEMO_INFO
   global RUNNING_DEMO

   if { $RUNNING_DEMO != "<idle>" } {
      if { $DEMO_INFO != "" } {
         ShowText "Current demonstration" $DEMO_INFO demo
      } {
         Message "No extra information is available for demonstration \"$RUNNING_DEMO\"."
      }
   } {
      Message "No demonstration is currently running."
   }
}

proc RemoveLinks {scope} {
   global STEP_LINKS
   global DEMO_LINKS
   global CLAB

   set tags ""

   if { $scope == "step" || $scope == "demo" } {

      foreach link [array names STEP_LINKS] {
         $CLAB tag bind $link <Button> ""
         lappend tags $link
      }
      catch {unset STEP_LINKS}

      if { $scope == "demo" } {
         foreach link [array names DEMO_LINKS] {
            $CLAB tag bind $link <Button> ""
         lappend tags $link
         }
         catch {unset DEMO_LINKS}
      }
   }

   foreach link $tags {
      $CLAB tag configure $link -foreground black
      $CLAB tag bind $link <Enter> ""
      $CLAB tag bind $link <Leave> ""
   }

}

proc Strut {w h} {
   set ret [frame $w.strut -width 0 -height $h]
   pack propagate $ret 0
   pack $ret -side left
   return $ret
}

proc CheckTag {widget tag} {

   set newtag [SetHtxTag $widget $tag]
   if { $newtag == "" } {
      set newtag [string toupper $tag]
   }

   return $newtag
}

proc InsertText {widget text tags} {
   global SUN

   regsub -all { +} $text { } com3
   if { $com3 != "" } {

      if { [lsearch -exact $tags COMMAND] != -1 } {
         set label [string trim [string toupper $text]]
         if { ![catch {exec showme -n $SUN $label} mess] } {
            set htxtag [SetHtxTag $widget "htx:$SUN|$label"]
            if { $htxtag != "" } { lappend tags $htxtag }
         } {
            Diag "Failed to find htx target $SUN $label - $mess"
         }

      } elseif { [lsearch -exact $tags DOC] != -1 } {
         if { [regexp -nocase {SUN[^0-9]*([0-9]+)} $text match num] } {
            set doc "sun$num"
            if { ![catch {exec showme -n $doc} mess] } {
               set htxtag [SetHtxTag $widget "htx:$doc"]
               if { $htxtag != "" } { lappend tags $htxtag }
            } {
               Diag "Failed to find htx target $doc - $mess"
            }
         }

      } elseif { [lsearch -exact $tags URL] != -1 } {
         if { [regexp -nocase {http:} $text] } {
            set urltag [SetHtxTag $widget $text]
            if { $urltag != "" } { lappend tags $urltag }
         }

      }

      if { [llength $tags] > 0 } {
         $widget insert end $com3 $tags
      } {
         $widget insert end $com3
      }
   }
}

proc SetHtxTag {widget tag} {
   global LINKCOL
   global NET

# See if the supplied tag is a htx hyper-link, and if so, extract the htx
# document reference from within it.
   if { [regexp -nocase {htx:(.*)} $tag match doc] } {

# Create the tag name to be returned.
      set newtag "HTX:$doc"

# If anything already has this tag, do not add it again.
      if { [$widget tag ranges $newtag] == "" } {

# Set up a binding which causes ShowLink to be executed when any button is
# pressed over the tagged text.
         $widget tag bind $newtag <Button> "ShowLink htx $doc"

# Set up bindings which cause the cursor and help text to change whenever
# text with this tag is clicked on. Also set the colour of link text.
         $widget tag configure $newtag -foreground $LINKCOL
         $widget tag bind $newtag <Enter> \
            "set OLDCURSOR \[$widget cget -cursor\]
             $widget config -cursor right_ptr
             set OLDHELP \$HELP
             set HELP \".   Click to display $doc in a WWW browser. Any existing browser will be used, otherwise a new one will be created.\"
             "
         $widget tag bind $newtag <Leave> \
            "set HELP \$OLDHELP
             $widget config -cursor \$OLDCURSOR"
      } {
         set newtag ""
      }

# See if the supplied tag is a URL hyper-link.
   } elseif { [regexp -nocase {http:} $tag] } {

# Create the tag name to be returned.
      set newtag $tag

# Check that network access has not been disabled.
      if { $NET } {

# Set up a binding which causes ShowLink to be executed when any button is
# pressed over the tagged text.
         $widget tag bind $newtag <Button> "ShowLink url $tag"

# Set up bindings which cause the cursor and help text to change whenever
# text with this tag is clicked on. Also set the colour of link text.
         $widget tag configure $newtag -foreground $LINKCOL
         $widget tag bind $newtag <Enter> \
            "set OLDCURSOR \[$widget cget -cursor\]
             $widget config -cursor right_ptr
             set OLDHELP \$HELP
             set HELP \".   Click to display $tag in a WWW browser. Any existing browser will be used, otherwise a new one will be created.\"
             "
         $widget tag bind $newtag <Leave> \
            "set HELP \$OLDHELP
             $widget config -cursor \$OLDCURSOR"
      }

   } {
      set newtag ""
   }

   return $newtag

}

proc MakeAlpha {} {
   global ALPHATEXT
   global TT_FONT
   global AF
   global CAN

#  Create a frame containing a text widget and a scroll bar if this has
#  not already been done.
   if { ![info exists AF] } {

# Create the Frame.
      set AF [toplevel .stardemoAlpha]

# Create the scroll bar.
      set sc [scrollbar $AF.sc -command "$AF.lab yview" -width 15 -relief sunken]

# Create the text widget.
      set ALPHATEXT [text $AF.lab -state disabled -relief flat -wrap word -bd 0 \
                   -highlightthickness 0 -font $TT_FONT \
                   -background black -foreground white -yscrollcommand "$AF.sc set"]

# Pack the text widget and scroll par into the parent frame.
      pack $sc -side right -fill y
      pack $ALPHATEXT -side left -fill both -expand 1

# Put the text window in the top right corner and make it the saem size
# as the canvas.
      update idletasks
      regsub {\+1\+1} [winfo geometry $CAN] "\-1\+1" res
      wm geometry $AF $res

# If the toplevel already exists, ensure it is visible and empty the text
# widget.
   } else {
       $ALPHATEXT delete 1.0 end
       wm deiconify $AF
   }

}

proc Diag {mess} {
   global DEBUG
   if { $DEBUG } {
      puts "$mess"
   }
}

proc ShowMe {doc args} {
   set ret 0

   if { $args != "" } {
      if { ![catch {exec showme -n $doc $args} mess] } {
         set ret [CCDShowHelp $mess]
      } {
         Diag "Failed to find document $doc (label $args) - $mess."
      }
   } {
      if { ![catch {exec showme -n $doc} mess] } {
         set ret [CCDShowHelp $mess]
      } {
         Diag "Failed to find document $doc - $mess"
      }
   }

   return $ret

}



proc GetEnv {varnam} {
   global env
   global ENV_VARS

   Diag "      $varnam"

   if { [info exists env($varnam)] } {
      lappend ENV_VARS $varnam
      set ret 1
   } {
      Message "Environment variable \"$varnam\" is not defined."
      set ret 0
   }

   return $ret
}


   proc CCDShowHelp {url} {
#+
#  Name:
#     CCDShowHelp

#  Purpose:
#     Displays a help file in a WWW browser.

#  Language:
#     TCL

#  Description:
#     This routine controls the display of help pages in a HTML WWW
#     browser. The argument is simply the URL of a local file to be
#     displayed. The type of browser used is controlled by the
#     CCDbrowser variable. This should be set to the name of the
#     executable (short name if on the PATH otherwise a full name).
#     If CCDbrowser isn't set it defaults to "Mosaic".

#  Arguments:
#     url = string (read)
#        The URL of the help page to be displayed.

#  Copyright:
#     Copyright (C) 1993 Science & Engineering Research Council.
#     Copyright (C) 1995, 1997-1998 Central Laboratory of the Research
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
#     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
#     02111-1307, USA

#  Authors:
#     DLT: D L Terrett (Starlink, RAL)
#     PDRAPER: Peter Draper (STARLINK - Durham University)
#     DSB: David Berry (STARLINK- Manchester University)
#     {enter_new_authors_here}

#  History:
#     29-NOV-1993 (DLT):
#              Original version.
#     21-MAR-1995 (PDRAPER):
#        Brought into CCDPACK from Xadam (was named gethelp).
#     22-MAR-1995 (PDRAPER):
#        Added facility to use netscape (1.1) as well as Mosaic.
#     29-JUN-1997 (DSB):
#        Brought into POLPACK from CCDPACK. Calls to CCDIssueInfo changed
#        to Message. Check environment variable HTX_BROWSER instead of
#        Tcl variable CCDbrowser to determine the browser to use.
#        Supplied argument changed from a file name to a URL (and the
#        netscape remote openFILE command changed to openURL).
#     6-JUL-1998 (DSB):
#        Modified to use HTX_BROWSER as supplied instead of only using
#        "netscape" or "mosaic".
#     {enter_further_changes_here}

#  Bugs:
#     {note_any_bugs_here}

#-

#  Global variables.
      global env
      global netscapepid
      global mosaicpid

      set ret 1

#  Check the browser to use. If HTX_BROWSER doesn't exist use Netscape.
#  If it does, use it.
      if { ! [info exists env(HTX_BROWSER)] } {
         set CCDbrowser netscape
      } {
         set CCDbrowser $env(HTX_BROWSER)
      }

      switch -glob $CCDbrowser {
	 *[Mm]osaic* {

#  Use Mosaic. This relies on the remote-command mechanisms prior to CCI.
	    set mosaicpid 0
	    catch {
	       set in [open ~/.mosaicpid r]
	       gets $in mosaicpid
	       close $in
	    }
	    if { $mosaicpid != 0 } {
	       set fid [open /tmp/Mosaic.$mosaicpid w]
	       puts $fid "goto"
	       puts $fid $url
	       close $fid
	       if { [catch {exec kill -USR1 $mosaicpid}] } {
		  set mosaicpid 0
	       }
	    }
	    if { $mosaicpid == 0 } {
	       exec  $CCDbrowser $url &
	    }
	 }

	 *[Nn]etscape* {

#  Use Mozilla. This uses the NCAPIs methods as of netscape 1.1b1.
#  Attempt to make browser goto the required page. If this fails then the
#  browser has exited for some reason, so restart it.
            if { ! [info exists netscapepid] } { set netscapepid 1 }
	    if { [catch {exec $CCDbrowser -remote openURL($url)} mess] } {
	       set netscapepid 0
	    }
            if { $netscapepid == 0 } {
               if { [catch { set netscapepid [exec $CCDbrowser $url &]} mess] } {
                  Message "Failed to start $CCDbrowser - $mess"
                  set ret 0
               }
	    }
	 }
      }

      return $ret
   }

#  Kill all process created by this script.
#  ----------------------------------------
   proc killNew {} {
      global OLDKAPPA
      global env

      set re {([0-9]+) .+ .*}

#  Get information about all processes owned by the current user.
      catch {exec ps -eo "pid ruser comm" | grep $env(USER)} procs

#  Process each line of this info. Each line has a pid, a user id, and a
#  command name.
      foreach line [split $procs "\n"] {
         set fields [split $line]

#  Do each process to be killed in turn.
         foreach process [list kappa kapview ndfpack adamMessa] {

#  If the command (index 2 in the line) contains the process name...
            if { [regexp $re$process $line match pid] } {

#  Check if the process was active when this script started.
               set dokill 1
               foreach oldpid $OLDKAPPA {
                  if { $pid == $oldpid } {
                     set dokill 0
                  }
               }

#  If not, kill the process
               if { $dokill } {
                  catch "exec kill $pid"
               }

#  Creak out of the process name loop.
               break
            }
         }
      }
   }

   proc printPid {label} {
      global OLDKAPPA
      global env

      set re {([0-9]+) .+ .*}

      puts "\n$label"

#  Get information about all processes owned by the current user.
      catch {exec ps -eo "pid ruser comm" | grep $env(USER)} procs

#  Process each line of this info. Each line has a pid, a user id, and a
#  command name.
      foreach line [split $procs "\n"] {

#  Do each process in turn.
         foreach process [list kappa kapview ndfpack adamMess] {

#  If the command (index 2 in the line) contains the process name...
            if { [regexp $re$process $line match pid] } {

#  Add this line to the returned value.
               puts $line

#  Creak out of the process name loop.
               break
            }
         }
      }
   }
