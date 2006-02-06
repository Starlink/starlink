#+
# adamtask.tcl
#
# This version is for use with the CURSA catalogue browser xcatview.
#
# History:
#  8/9/99 (ACD): Created xcatview version from STARTCL template/example.
#  6/2/06 (PWD): Cut down to just adamtask.showMessage, this should replace
#                the version in the distributed adamtask.tcl.
#

proc adamtask.locals {} {
   # Does nothing, but make sure it is called immediately after the first
   # adamtask procedure. That sources this file and make sure the local
   # versions of any procedures in this file are used in preference to those in
   # the adamtask.tcl file.
}


proc adamtask.showMessage {task message} {
#+
# Displays any information messages from a task in a scrolling text widget.
#
# The message from the task is returned in $message.
#-

#   puts stdout "hello"
#   puts stdout $message

#
#  Remove carriage returns and new line characters from the output.

    set current_line [string trim $message \n]
    set current_line [string trim $current_line \r]
#   puts stdout $current_line

#
#  Check whether the current line starts with a vertical bar ('|').
#  If it does then it contains genuine output.  If it does not
#  it contains either an error or warning message or an echo of
#  commands sent to the ADAM application.

    set vbar_pos [string first | $current_line]
#   puts stdout $vbar_pos

    if {$vbar_pos == 0} then {

#
#     Remove the leading vertical bar and either send the line to
#     the output window, or append it to the list, as appropriate.

       global captureList
       global catalogueList

       if {$captureList == 0} then {
          .display.output configure -state normal
          .display.output insert end [string range $current_line 1 end]
          .display.output insert end "\n"
          .display.output configure -state disabled
       } else {
          lappend catalogueList [string range $current_line 1 end]
       }

    } else {

#
#     Either send every line to the message window, or just send
#     those which are genuine error or warning messages, as appropriate.

       global echoCommand

#       puts stdout $current_line

       if {$echoCommand != 0} then {
          .messages.output configure -state normal
          .messages.output insert end $current_line
          .messages.output insert end "\n"
          .messages.output configure -state disabled
       } else {
          set excl_pos [string first ! $current_line]

          if {$excl_pos == 0} then {
             .messages.output configure -state normal
             .messages.output insert end $current_line
             .messages.output insert end "\n"
             .messages.output configure -state disabled
          }
       }
    }

#
#  Ensure that the last line displayed in the message window is
#  visible.

    .messages.output see end
    .messages.output  yview      -pickplace end
}
