proc RemoteUrl { } {

#+ RemoteUrl
#
#  Interrogate a configuration to obtain the URL of a `directory' entry.
#
#  Given.
#    None.
#
#  Author:
#   ACD: A C Davenhall (Edinburgh)
#
#  History:
#   13/4/01 (ACD): Original version.
#-

#
#  Declare global variables.
#  ------------------------

    global catremoteExe

    global remoteDir
    global remoteUrl

#
#  Create the top-level window and lower its stacking order.

    toplevel     .remoteurl   -class Dialog   -bd 10

    wm withdraw .remoteurl

    wm title     .remoteurl   "Remote URL"
    wm iconname  .remoteurl   Remote
    wm transient .remoteurl   .

#  Write text into it.

    label .remoteurl.text -text  \
      "Please wait (obtaining details of directory from remote server)..."
    pack  .remoteurl.text

#
#  Withdraw the window, then update all the geometry information
#  to determine how big the window wants to be, then centre the window
#  in parent and de-iconify it.

    update idletasks
    set x [expr [winfo width .]/2 - [winfo reqwidth .remoteurl]/2 + \
      [winfo x .]]
    set y [expr [winfo height .]/2 - [winfo reqheight .remoteurl]/2 + \
      [winfo y .]]
    wm geom .remoteurl +$x+$y
    wm deiconify .remoteurl

#
#  Set a grab and claim the focus.

    set oldFocus [focus]
    grab  .remoteurl
    focus .remoteurl
    update idletasks

#
#  Attempt to execute catremote to generate the catalogue and proceed
#  if ok.

    set queryStatus [catch {
      exec $catremoteExe "details" $remoteDir > XCATVIEW_DETAILS } queryMsg]

    if {$queryStatus == 0} then {

#
#     Read back the output from catremote to obtain the list of catalogues.

       set crf [open "XCATVIEW_DETAILS" r]

       set remoteUrl ""

       set more 1

       while {$more != 0} {
          gets $crf current_line
#         puts stdout $current_line

          if {$current_line != ""} then {

#
#           Remove carriage returns and new line characters from the output.

             set current_line [string trim $current_line \n]
             set current_line [string trim $current_line \r]

#
#           Check whether the current line starts with an exclamation
#           mark ('!').  If it does then it is an error or information
#           message; otherwise it is genuine output.

             set exclPos [string first ! $current_line]

             if {$exclPos < 0} then {

#
#              Check for the URL and attempt to extract the value.

                set foundUrl [string first "url:" $current_line]

                if {$foundUrl > -1} then {
                   set firstSpace [string first " " $current_line]
                   set remoteUrlTemp \
                     [string range $current_line $firstSpace end]
                   set remoteUrl [string trimleft $remoteUrlTemp]
                }

             } else {

#
#              An error or information message has been encountered.
#              Send it to the error message window.

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

       set queryStatus [catch {exec rm -f XCATVIEW_DETAILS} queryMsg]

    }

#
#  Report any error.

    if {$queryStatus != 0} then {
       Error "Failed to run catremote (to obtain details of a directory)."
       Error "$queryMsg"

       set remoteCatsShort ""
       set remoteCatsLong  ""
    }

#
#  Destroy the dialogue box and restore the focus.

    destroy  .remoteurl
    focus    $oldFocus

}
