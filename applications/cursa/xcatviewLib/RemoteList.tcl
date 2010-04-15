proc RemoteList { } {

#+ RemoteList
#
#  Interrogate a remote server to obtain a list of the databasees
#  available on it.
#
#  Given.
#    None.
#
#  Author:
#   ACD: A C Davenhall (Edinburgh)
#
#  History:
#   25/3/97  (ACD): Original version.
#   22/11/99 (ACD): Modified to use the standard tcl/tk 'exec' rather
#      than 'expect/spawn'.
#   23/11/99 (ACD): First stable version with exec.
#   10/4/01  (ACD): Modified for the new version of catremote written
#      Perl.
#   13/4/01  (ACD): Corrected output of error messages.
#   17/4/01  (ACD): Excluded name servers and image servers from the list.
#-

#
#  Declare global variables.
#  ------------------------

    global catremoteExe

    global remoteCatsShort
    global remoteCatsLong

#
#  Create the top-level window and lower its stacking order.

    toplevel     .remotelist   -class Dialog   -bd 10

    wm withdraw .remotelist

    wm title     .remotelist   "Remote List"
    wm iconname  .remotelist   Remote
    wm transient .remotelist   .

#  Write text into it.

    label .remotelist.text -text  \
      "Please wait (obtaining list of catalogues from remote server)..."
    pack  .remotelist.text

#
#  Withdraw the window, then update all the geometry information
#  to determine how big the window wants to be, then centre the window
#  in parent and de-iconify it.

    update idletasks
    set x [expr [winfo width .]/2 - [winfo reqwidth .remotelist]/2 + \
      [winfo x .]]
    set y [expr [winfo height .]/2 - [winfo reqheight .remotelist]/2 + \
      [winfo y .]]
    wm geom .remotelist +$x+$y
    wm deiconify .remotelist

#
#  Set a grab and claim the focus.

    set oldFocus [focus]
    grab  .remotelist
    focus .remotelist
    update idletasks

#
#  Set the query string.  Note that here the query is simply a
#  question mark, which will cause catremote to list the catalogues
#  available.

    set    queryString "list"

#
#  Attempt to execute catremote to generate the catalogue and proceed
#  if ok.

    set queryStatus [catch {
      exec $catremoteExe $queryString > XCATVIEW_LIST } queryMsg]

    if {$queryStatus == 0} then {

#
#     Read back the output from catremote to obtain the list of catalogues.

       set crf [open "XCATVIEW_LIST" r]

       set remoteCatsShort ""
       set remoteCatsLong  ""

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
#              Genuine output comprising the list of catalogues etc.
#              available.  Extract the short name and reformat the
#              long name so that the server type appears in brackets
#              at the end.

                set firstSpace [string first " " $current_line]

                set currentCatsShort \
                  [string range $current_line 0 $firstSpace]

                set currentLong [string range $current_line $firstSpace end]
                set currentLong [string trimleft $currentLong]

                set firstSpace [string first " " $currentLong]

                set currentServeType \
                  [string range $currentLong 0 $firstSpace]
                set currentServeType [string trimright $currentServeType]

                if {($currentServeType != "namesvr") &&
                    ($currentServeType != "imagesvr")} then {
                   set currentName \
                     [string range $currentLong $firstSpace end]
                   set currentName [string trimleft $currentName]

                   if {$currentServeType == "catalog"} then {
                      set currentServeType "catalogue"
                   }

                   append currentName " ("
                   append currentName "$currentServeType"
                   append currentName ")"

                   lappend remoteCatsLong  $currentName

                   lappend remoteCatsShort $currentCatsShort
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

       set queryStatus [catch {exec rm -f XCATVIEW_LIST} queryMsg]

    }

#
#  Report any error.

    if {$queryStatus != 0} then {
       Error "Failed to run catremote (to obtain list of cats. on server)."
       Error "$queryMsg"

       set remoteCatsShort ""
       set remoteCatsLong  ""
    }

#
#  Destroy the dialogue box and restore the focus.

    destroy  .remotelist
    focus    $oldFocus

}
