proc cgs4drCheckNbs {nb} {
   global Cred4NoticeBoard
   global P4NoticeBoard

# Check which notiecboard is to be verified
   if {[string tolower [string trim $nb]] == "cred4"} {
     set nbs_exists [info exists Cred4NoticeBoard]
   } elseif {[string tolower [string trim $nb]] == "p4"} {
     set nbs_exists [info exists P4NoticeBoard]
   } else {
     set nbs_exists 0
   }

# Pop-up a dialogue box if nbs doesn't exist
   if {$nbs_exists == 0} {
     if {[winfo exists .cgs4drDialogue]} {destroy .cgs4drDialogue}
     set frame [dialogStart .cgs4drDialogue "NBS Not Available" error Dismiss]
     cgs4drCursor pirate orange black
     .cgs4drDialogue config -cursor {arrow green black}

     set label1 [label $frame.lab1 -text "Failed to get NBS value from $nb!" -width 60]
     pack $label1 -in $frame -side top

     set bv [dialogShow .cgs4drDialogue .cgs4drDialogue]
     destroy .cgs4drDialogue
     cgs4drCursor arrow green black
     return 1
   } else {
     return 0
   }
}
