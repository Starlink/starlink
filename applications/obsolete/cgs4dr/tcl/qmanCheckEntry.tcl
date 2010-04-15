proc qmanCheckEntry {w description} {
#+
#  Checks that the contents of an entry widget are a valid
#-

# Get the contents of the widget.
   set value [$w get]

# See if it is a valid integer by trying to add zero to it.
   set status [catch {incr value 0}]
   if {$status==0} {
     if {$value<=0} {set status 1}
   }

# Display an error dialog box if there is something wrong.
   if {$status!=0} {
     if {[winfo exists .qmanDialogue]} {destroy .qmanDialogue}
     set frame [dialogStart .qmanDialogue "Check Entry" error Dismiss]
     cgs4drCursor pirate orange black
     .qmanDialogue config -cursor {arrow green black}
     set label [label $frame.lab -text "$description must be a positive integer" -width 60]
     pack $label -in $frame -side left
     set bv [dialogShow .qmanDialogue .qmanDialogue]
     destroy .qmanDialogue
   }
   cgs4drCursor arrow green black
   return $status
}
