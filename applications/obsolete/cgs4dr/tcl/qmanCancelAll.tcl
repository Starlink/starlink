proc qmanCancelAll {taskname} {
#+
# Cancels all queue entries.
#-

# Check that the user really wants to go ahead
    if {[winfo exists .qmanDialogue]} {destroy .qmanDialogue}
    set frame [dialogStart .qmanDialogue "Remove All Entries Confirmation" 0 OK Cancel]
    cgs4drCursor pirate orange black
    .qmanDialogue config -cursor {arrow green black}

    set label1 [label $frame.lab1 -text "This action will remove all entries from the queue." -width 60]
    set label2 [label $frame.lab2 -text "Proceed?" -width 60]
    pack $label1 $label2 -in $frame -side top

# Disable all buttons except the interrupt button
    set bv [dialogShow .qmanDialogue .qmanDialogue]
    if {$bv==0} {
      cgs4drCursor watch red white
      global QmanAccess
      global QmanWidgets
      grab $QmanWidgets(INTERRUPT)

# Delete the commands
      $taskname obey lock $QmanAccess -inform "cgs4drInform $taskname %V"
      $taskname obey delete "$QmanAccess delete_mode=all" -inform "cgs4drInform $taskname %V"
      $taskname obey unlock $QmanAccess -inform "cgs4drInform $taskname %V" -endmsg "grab release $QmanWidgets(INTERRUPT)"
    }

# Restore the cursor
    cgs4drCursor arrow green black
    destroy .qmanDialogue
}
