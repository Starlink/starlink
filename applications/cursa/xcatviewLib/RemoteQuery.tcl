proc RemoteQuery { } {

#+ RemoteQuery
#
#  Perform a selection on a catalogue available via a remote server.
#  Note that the only type of selection available is a 'cone search'
#  to choose the objects which lie within a given circular area of sky.
#
#  Given.
#    None.
#
#  Author:
#   ACD: A C Davenhall (Edinburgh)
#
#  History:
#   26/3/97  (ACD): Original version.
#   15/7/97  (ACD): Fixed bug in extracting the remote catalogue name
#      on Linux.
#   22/11/99 (ACD): Modified to use the standard tcl/tk 'exec' rather
#      than 'expect/spawn'.
#   23/11/99 (ACD): First stable version with exec.
#   11/4/01  (ACD): Modified for the new version of catremote written
#      Perl.
#   18/5/01  (ACD): Tidied up by removing redundant and commented-out
#      code.
#-

#
#  Declare global variables.
#  ------------------------

    global catremoteExe

    global remoteQueryCat
    global remoteQueryRA
    global remoteQueryDec
    global remoteQueryRadius

    global catalogueName

#
#  Create the top-level window and lower its stacking order.

    toplevel     .remotequery   -class Dialog   -bd 10

    wm withdraw  .remotequery

    wm title     .remotequery   "Remote Query"
    wm iconname  .remotequery   Remote
    wm transient .remotequery   .

#  Write text into it.

    label .remotequery.text -text  \
      "Please wait (querying remote catalogue)..."
    pack  .remotequery.text

#
#  Withdraw the window, then update all the geometry information
#  to determine how big the window wants to be, then centre the window
#  in parent and de-iconify it.

    update idletasks
    set x [expr [winfo width .]/2 - [winfo reqwidth .remotequery]/2 + \
      [winfo x .]]
    set y [expr [winfo height .]/2 - [winfo reqheight .remotequery]/2 + \
      [winfo y .]]
    wm geom .remotequery +$x+$y
    wm deiconify .remotequery

#
#  Set a grab and claim the focus.

    set oldFocus [focus]
    grab  .remotequery
    focus .remotequery
    update idletasks

#
#  Attempt to execute catremote to generate the catalogue and proceed
#  if ok.

    set queryStatus [catch {
      exec $catremoteExe "query" $remoteQueryCat $remoteQueryRA \
        $remoteQueryDec $remoteQueryRadius > XCATVIEW_QUERYRES } queryMsg]

    if {$queryStatus == 0} then {

#
#     Read back the output from catremote to determine the remote file
#     name.

       set crf [open "XCATVIEW_QUERYRES" r]

       set more 1
       set catalogueName ""

       while {$more != 0} {
          gets $crf current_line
#         puts stdout $current_line

          if {$current_line != ""} then {


#
#           Remove carriage returns and new line characters from the output.

             set current_line [string trim $current_line \n]
             set current_line [string trim $current_line \r]

#
#           Check whether the current line contains the name of the
#           catalogue.  If it does then extract the name of the catalogue.
#           Otherwise send the line to the error message window.

             set catLine [string first "!(Info.) Catalogue" $current_line]

             if {$catLine >= 0} then {

#
#              Extract the name of the new catalogue.

                set catalogueTemp [string range $current_line 19 end]
                set firstSpace [string first " " $catalogueTemp]
                set catalogueName [string range $catalogueTemp 0 $firstSpace]

             } else {

#
#              Send the line to the error message window.

                .messages.output  configure  -state normal

                .messages.output insert end $current_line
                .messages.output insert end "\n"

                .messages.output  configure  -state disabled
                .messages.output  yview      -pickplace end
             }
          } else {

#
#           An empty line was encountered; set the termination flag.

             set more 0
          }
       }

#
#     Close and delete the file.

       close $crf

       set queryStatus [catch {exec rm -f XCATVIEW_QUERYRES} queryMsg]

    }

#
#  Report any error.

    if {$queryStatus != 0} then {
       Error "Failed to run catremote (to query remote catalogue)."
       Error "$queryMsg"

       set catalogueName ""
    }

#
#  Destroy the dialogue box and restore the focus.

    destroy  .remotequery
    focus    $oldFocus
}
