proc red4Iarith {taskname action} {
#+
# Creates a dialog box for red4 action
#-
    global env
    global Red4Widgets

# Check to see if task is busy
    set status [cgs4drCheckTask red4]
    if {$status!=0} {return}

# Create dialog box
    if {[winfo exists .red4Dialogue]} {destroy .red4Dialogue}
    set frame [dialogStart .red4Dialogue "Red4 $action Image(s)" 0 OK Cancel]
    cgs4drCursor pirate orange black
    .red4Dialogue config -cursor {arrow green black}

# Create and pack dialog box widgets
    if {$action=="INOT4" || $action=="MEND"} {
      set top [frame $frame.top]
      set bot [frame $frame.bot]
      pack $top $bot -in $frame -side top
      set Red4Widgets(AR_LAB01) [label $top.l1 -text "Input Image" -width 15]
      set Red4Widgets(AR_ENT01) [entry $top.en1 -width 40]
      pack $Red4Widgets(AR_LAB01) $Red4Widgets(AR_ENT01) -in $top -side left
      $Red4Widgets(AR_ENT01) insert end $Red4Widgets(IRO1)
      set Red4Widgets(AR_LAB02) [label $bot.l2 -text "Output Image" -width 15]
      set Red4Widgets(AR_ENT02) [entry $bot.en2 -width 40]
      pack $Red4Widgets(AR_LAB02) $Red4Widgets(AR_ENT02) -in $bot -side left
      $Red4Widgets(AR_ENT02) insert end $Red4Widgets(IRO2)
      bind $Red4Widgets(AR_LAB01) <Button-2> "red4Update red4Iarith2 ALL"
      bind $Red4Widgets(AR_LAB02) <Button-2> "red4Update red4Iarith2 ALL"
      bind $Red4Widgets(AR_ENT01) <Button-2> "red4Update red4Iarith2 AR_ENT01"
      bind $Red4Widgets(AR_ENT01) <Double-Button-2> "$Red4Widgets(AR_ENT01) delete 0 end"
      bind $Red4Widgets(AR_ENT02) <Button-2> "red4Update red4Iarith2 AR_ENT02"
      bind $Red4Widgets(AR_ENT02) <Double-Button-2> "$Red4Widgets(AR_ENT02) delete 0 end"
    } else {
      set top [frame $frame.top]
      set mid [frame $frame.mid]
      set bot [frame $frame.bot]
      pack $top $mid $bot -in $frame -side top
      set Red4Widgets(AR_LAB01) [label $top.l1 -text "First Image" -width 15]
      set Red4Widgets(AR_ENT01) [entry $top.en1 -width 40]
      pack $Red4Widgets(AR_LAB01) $Red4Widgets(AR_ENT01) -in $top -side left
      $Red4Widgets(AR_ENT01) insert end $Red4Widgets(IRO1)
      set Red4Widgets(AR_LAB02) [label $mid.l2 -text "Second Image" -width 15]
      set Red4Widgets(AR_ENT02) [entry $mid.en2 -width 40]
      pack $Red4Widgets(AR_LAB02) $Red4Widgets(AR_ENT02) -in $mid -side left
      $Red4Widgets(AR_ENT02) insert end $Red4Widgets(IRO2)
      set Red4Widgets(AR_LAB03) [label $bot.l3 -text "Output Image" -width 15]
      set Red4Widgets(AR_ENT03) [entry $bot.en3 -width 40]
      pack $Red4Widgets(AR_LAB03) $Red4Widgets(AR_ENT03) -in $bot -side left
      $Red4Widgets(AR_ENT03) insert end $Red4Widgets(IRO3)
      bind $Red4Widgets(AR_LAB01) <Button-2> "red4Update red4Iarith ALL"
      bind $Red4Widgets(AR_LAB02) <Button-2> "red4Update red4Iarith ALL"
      bind $Red4Widgets(AR_LAB03) <Button-2> "red4Update red4Iarith ALL"
      bind $Red4Widgets(AR_ENT01) <Button-2> "red4Update red4Iarith AR_ENT01"
      bind $Red4Widgets(AR_ENT01) <Double-Button-2> "$Red4Widgets(AR_ENT01) delete 0 end"
      bind $Red4Widgets(AR_ENT02) <Button-2> "red4Update red4Iarith AR_ENT02"
      bind $Red4Widgets(AR_ENT02) <Double-Button-2> "$Red4Widgets(AR_ENT02) delete 0 end"
      bind $Red4Widgets(AR_ENT03) <Button-2> "red4Update red4Iarith AR_ENT03"
      bind $Red4Widgets(AR_ENT03) <Double-Button-2> "$Red4Widgets(AR_ENT03) delete 0 end"
    }

# Show the dialog box
    set bv [dialogShow .red4Dialogue .red4Dialogue]
    if {$bv==0} {
      cgs4drCursor watch red white
      if {$action=="INOT4"} {
        set Red4Widgets(IRO1) [string trim [$Red4Widgets(AR_ENT01) get]]
        set Red4Widgets(IRO2) [string trim [$Red4Widgets(AR_ENT02) get]]
        $taskname obey inot4 "input=$Red4Widgets(IRO1) output=$Red4Widgets(IRO2)" -inform "cgs4drInform $taskname %V"
      } elseif {$action=="MEND"} {
        set Red4Widgets(IRO1) [string trim [$Red4Widgets(AR_ENT01) get]]
        set Red4Widgets(IRO2) [string trim [$Red4Widgets(AR_ENT02) get]]
        $taskname obey mend "input=$Red4Widgets(IRO1) output=$Red4Widgets(IRO2)" -inform "cgs4drInform $taskname %V"
      } elseif {$action=="IADD4"} {
        set Red4Widgets(IRO1) [string trim [$Red4Widgets(AR_ENT01) get]]
        set Red4Widgets(IRO2) [string trim [$Red4Widgets(AR_ENT02) get]]
        set Red4Widgets(IRO3) [string trim [$Red4Widgets(AR_ENT03) get]]
        $taskname obey iadd4 "image1=$Red4Widgets(IRO1) image2=$Red4Widgets(IRO2) output=$Red4Widgets(IRO3) errors='GAUSSIAN'" -inform "cgs4drInform $taskname %V"
      } elseif {$action=="ISUB4"} {
        set Red4Widgets(IRO1) [string trim [$Red4Widgets(AR_ENT01) get]]
        set Red4Widgets(IRO2) [string trim [$Red4Widgets(AR_ENT02) get]]
        set Red4Widgets(IRO3) [string trim [$Red4Widgets(AR_ENT03) get]]
        $taskname obey isub4 "image1=$Red4Widgets(IRO1) image2=$Red4Widgets(IRO2) output=$Red4Widgets(IRO3) errors='GAUSSIAN'" -inform "cgs4drInform $taskname %V"
      } elseif {$action=="IDIV4"} {
        set Red4Widgets(IRO1) [string trim [$Red4Widgets(AR_ENT01) get]]
        set Red4Widgets(IRO2) [string trim [$Red4Widgets(AR_ENT02) get]]
        set Red4Widgets(IRO3) [string trim [$Red4Widgets(AR_ENT03) get]]
        $taskname obey idiv4 "image1=$Red4Widgets(IRO1) image2=$Red4Widgets(IRO2) output=$Red4Widgets(IRO3) errors='GAUSSIAN'" -inform "cgs4drInform $taskname %V"
      } elseif {$action=="IMULT4"} {
        set Red4Widgets(IRO1)(IRO1) [string trim [$Red4Widgets(AR_ENT01) get]]
        set Red4Widgets(IRO2) [string trim [$Red4Widgets(AR_ENT02) get]]
        set Red4Widgets(IRO3) [string trim [$Red4Widgets(AR_ENT03) get]]
        $taskname obey imult4 "image1=$Red4Widgets(IRO1) image2=$Red4Widgets(IRO2) output=$Red4Widgets(IRO3) errors='GAUSSIAN'" -inform "cgs4drInform $taskname %V"
      } elseif {$action=="IAND4"} {
        set Red4Widgets(IRO1) [string trim [$Red4Widgets(AR_ENT01) get]]
        set Red4Widgets(IRO2) [string trim [$Red4Widgets(AR_ENT02) get]]
        set Red4Widgets(IRO3) [string trim [$Red4Widgets(AR_ENT03) get]]
        $taskname obey iand4 "image1=$Red4Widgets(IRO1) image2=$Red4Widgets(IRO2) output=$Red4Widgets(IRO3)" -inform "cgs4drInform $taskname %V"
      } elseif {$action=="IOR4"} {
        set Red4Widgets(IRO1) [string trim [$Red4Widgets(AR_ENT01) get]]
        set Red4Widgets(IRO2) [string trim [$Red4Widgets(AR_ENT02) get]]
        set Red4Widgets(IRO3) [string trim [$Red4Widgets(AR_ENT03) get]]
        $taskname obey ior4 "image1=$Red4Widgets(IRO1) image2=$Red4Widgets(IRO2) output=$Red4Widgets(IRO3)" -inform "cgs4drInform $taskname %V"
      } elseif {$action=="IEOR4"} {
        set Red4Widgets(IRO1) [string trim [$Red4Widgets(AR_ENT01) get]]
        set Red4Widgets(IRO2) [string trim [$Red4Widgets(AR_ENT02) get]]
        set Red4Widgets(IRO3) [string trim [$Red4Widgets(AR_ENT03) get]]
        $taskname obey ieor4 "image1=$Red4Widgets(IRO1) image2=$Red4Widgets(IRO2) output=$Red4Widgets(IRO3)" -inform "cgs4drInform $taskname %V"
      } else {
        cgs4drClear $taskname
        cgs4drInform $taskname "red4Iarith error : Action $action not recognised!"
      }
    }

# Remove the dialog box
    cgs4drCursor arrow green black
    destroy .red4Dialogue
}
