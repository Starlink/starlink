proc qmanCheckRange {w1 w2} {
#+
#  Checks that the contents of a pair of entry widget are a valid
#-

# Get the widget contents.
   set start [$w1 get]
   set end   [$w2 get]

# Display an error dialog if the end is not greater than or equal to the start.
   if {$start>$end} {
     if {[winfo exists .qmanDialogue]} {destroy .qmanDialogue}
     set frame [dialogStart .qmanDialogue "Check Range" error Dismiss]
     cgs4drCursor pirate orange black
     .qmanDialogue config -cursor {arrow green black}
     set label [label $frame.lab -text "The start number must be less than or equal to the end number!" -width 60]
     pack $label -in $frame -side left
     set bv [dialogShow .qmanDialogue .qmanDialogue]
     destroy .qmanDialogue
     cgs4drCursor arrow green black
     return 1
   }

# If the range is more than 50 entries then ask the user to confirm that this is OK.
   set nument [expr $end - $start]
   if {$nument>50} {
     if {[winfo exists .qmanDialogue]} {destroy .qmanDialogue}
     set frame2 [dialogStart .qmanDialogue "Range Confirmation" 0 OK Cancel]
     cgs4drCursor pirate orange black
     .qmanDialogue config -cursor {arrow green black}

     set label2 [label $frame2.lab2 -text "You have specified more than 50 entries!" -width 60]
     set label3 [label $frame2.lab3 -text "Continue?" -width 60]
     pack $label2 $label3 -in $frame2 -side top
     set bv [dialogShow .qmanDialogue .qmanDialogue]
     destroy .qmanDialogue
     cgs4drCursor arrow green black
     if {$bv!=0} {return 1}
   }
   return 0
}
