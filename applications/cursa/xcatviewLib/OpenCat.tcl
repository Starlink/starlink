proc OpenCat access {

#+ OpenCat
#
#  Open a specified catalogue.
#
#  access (GIVEN)
#    Type of message to be displayed.  The options are:
#    local  -  local catalogue,
#    remote -  subset of a remote database,
#    none   -  no message.
#
#  Author:
#   ACD: A C Davenhall (Edinburgh)
#
#  History:
#   28/3/97  (ACD): First dated version.
#   1/7/99   (ACD): Modified to use STARTCL rather than EXPECT.
#   10/9/99  (ACD): First stable version.
#   22/11/99 (ACD): Improved error trapping when loading a catalogue.
#-

#
#  Declare global variables.
#  ------------------------

    global catalogueName
    global catviewExe


#
#  Create the top-level window and lower its stacking order.

    toplevel     .opencat   -class Dialog   -bd 10

    wm withdraw  .opencat

    wm title     .opencat   "Open Catalogue"
    wm iconname  .opencat   Open
    wm transient .opencat   .

#  Write text into it.

    label .opencat.text -text "Opening catalogue..."
    pack  .opencat.text

#
#  Withdraw the window, then update all the geometry information
#  to determine how big the window wants to be, then centre the window
#  in parent and de-iconify it.

    update idletasks
    set x [expr [winfo width .]/2 - [winfo reqwidth .opencat]/2 + \
      [winfo x .]]
    set y [expr [winfo height .]/2 - [winfo reqheight .opencat]/2 + \
      [winfo y .]]
    wm geom .opencat +$x+$y
    wm deiconify .opencat

#
#  Set a grab and claim the focus.

    set oldFocus [focus]
    grab  .opencat
    focus .opencat

#
#  Load catview and wait for loading to complete.  Proceed if loading
#  completes before a time-out period elapses.

    adamtask catview $catviewExe
    adamtask.locals               ;# get local versions of procedures.

    set count 0
    set catviewLoaded 0

    while {[catview path] == 0} {
       after 100
       incr count
       if {$count > 100} then {
          catch {catview kill}
          set catviewLoaded 1
       }
    }

    if {$catviewLoaded == 0} then {
       global chosenColumns
       set    chosenColumns  ""

       global statsColumns
       set    statsColumns   ""

       global catalogueName
       if {$catalogueName != ""} then {

          if {$access == "local"} then {
             .messages.output  configure  -state normal

             .messages.output insert end \
  "--------------------------------------------------------------------------"
             .messages.output insert end "\n"
             .messages.output insert end "Opening new catalogue..."
             .messages.output insert end "\n"

             .messages.output  configure  -state disabled

             .messages.output  yview      -pickplace end
          }

          if {$access == "remote"} then {
             .messages.output  configure  -state normal

             .messages.output insert end \
  "--------------------------------------------------------------------------"
             .messages.output insert end "\n"
             .messages.output insert end  \
                "Opening selection from remote catalogue..."
             .messages.output insert end "\n"

             .messages.output  configure  -state disabled

             .messages.output  yview      -pickplace end
          }

          Action OPEN
          Action SETCONF
          Action LIST

       } else {
          Error "No catalogue specified."
       }
    } else {
       puts stdout "Failed to load the catview catalogue browser."
       Error "Failed to load the catview catalogue browser."
       set catalogueName ""
    }

#
#  Destroy the dialogue box and restore the focus.

    destroy  .opencat
    focus    $oldFocus

}
