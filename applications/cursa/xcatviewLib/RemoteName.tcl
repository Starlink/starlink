proc RemoteName { } {

#+ RemoteName
#
#  Interrogate a remote server to attempt to resolve an object name
#  to obtain its Right Ascension and Declination.
#
#  Given.
#    None.
#
#  Author:
#   ACD: A C Davenhall (Edinburgh)
#
#  History:
#   11/4/01 (ACD): Original version.
#   17/4/01 (ACD): First stable version.
#-

#
#  Declare global variables.
#  ------------------------

    global catremoteExe

    global remoteQueryName

    global remoteQueryRA
    global remoteQueryDec

    global env

    global catDirLevel
    global catDirList


#
#  Create the top-level window and lower its stacking order.

    toplevel     .remotename   -class Dialog   -bd 10

    wm withdraw .remotename

    wm title     .remotename   "Remote Name"
    wm iconname  .remotename   Remote
    wm transient .remotename   .

#  Write text into it.

    label .remotename.text -text  \
      "Please wait (attempting to resolve object name using remote server)..."
    pack  .remotename.text

#
#  Withdraw the window, then update all the geometry information
#  to determine how big the window wants to be, then centre the window
#  in parent and de-iconify it.

    update idletasks
    set x [expr [winfo width .]/2 - [winfo reqwidth .remotename]/2 + \
      [winfo x .]]
    set y [expr [winfo height .]/2 - [winfo reqheight .remotename]/2 + \
      [winfo y .]]
    wm geom .remotename +$x+$y
    wm deiconify .remotename

#
#  Set a grab and claim the focus.

    set oldFocus [focus]
    grab  .remotename
    focus .remotename
    update idletasks

#
#  Set the remote server to one which includes the SIMBAD name resolver.

    set env(CATREM_CONFIG) "http://dev.starlink.ac.uk/~pwd/catremote/cursa.cfg"

#
#  Attempt to execute catremote to resolve the object name.

    set queryStatus [catch {
      exec $catremoteExe "name" "simbad_ns@eso" $remoteQueryName \
        > XCATVIEW_NAME } queryMsg]

    if {$queryStatus == 0} then {

#
#     Read back the output from catremote to obtain the list of catalogues.

       set crf [open "XCATVIEW_NAME" r]

       set remoteQueryRA  ""
       set remoteQueryDec ""

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
#           message; otherwise an Right Ascension of Declination has been
#           returned.

             set exclPos [string first ! $current_line]

             if {$exclPos < 0} then {

#
#              Check for the Right Ascension and attempt to extract the
#              value.

                set foundRA [string first "Right Ascension:" $current_line]

                if {$foundRA > -1} then {
                   set firstSpace [string first " " $current_line]
                   set remoteQueryTemp \
                     [string range $current_line $firstSpace end]
                   set remoteQueryTemp [string trimleft $remoteQueryTemp]

                   set firstSpace [string first " " $remoteQueryTemp]
                   set remoteQueryRA \
                     [string range $remoteQueryTemp $firstSpace end]
                }

#
#              Check for the Declination and attempt to extract the
#              value.

                set foundDec [string first "Declination:" $current_line]

                if {$foundDec > -1} then {
                   set firstSpace [string first " " $current_line]
                   set remoteQueryDec \
                     [string range $current_line $firstSpace end]
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

       set queryStatus [catch {exec rm -f XCATVIEW_NAME} queryMsg]

#      puts stdout "remoteQueryRA: $remoteQueryRA"
#      puts stdout "remoteQueryDec: $remoteQueryDec"

    }

#
#  Report any error.

    if {$queryStatus != 0} then {
       Error "Failed to run catremote (to access name resolver)."
       Error "$queryMsg"

       set remoteQueryRA  ""
       set remoteQueryDec ""
    }

#
#  Restore the remote server to its value one entry.

    set env(CATREM_CONFIG) $catDirList($catDirLevel)

#
#  Destroy the dialogue box and restore the focus.

    destroy  .remotename
    focus    $oldFocus

}
