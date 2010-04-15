proc red4Efficiency {taskname} {
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
    set frame [dialogStart .red4Dialogue "Red4 Observation Efficiency" 0 OK Cancel]
    cgs4drCursor pirate orange black
    .red4Dialogue config -cursor {arrow green black}

# Create and pack dialog box widgets
    set Red4Widgets(EF_LABEL) [label $frame.lb -text "Filename"]
    set Red4Widgets(EF_ENTRY) [entry $frame.en -width 40]
    pack $Red4Widgets(EF_LABEL) $Red4Widgets(EF_ENTRY) -in $frame -side left
    $Red4Widgets(EF_ENTRY) insert end $Red4Widgets(RO)

# Bind the defaults button
    bind $Red4Widgets(EF_LABEL) <Button-2> "red4Update red4Efficiency EF_ENTRY"
    bind $Red4Widgets(EF_ENTRY) <Button-2> "red4Update red4Efficiency EF_ENTRY"
    bind $Red4Widgets(EF_ENTRY) <Double-Button-2> "$Red4Widgets(EF_ENTRY) delete 0 end"

# Show the dialog box
    set bv [dialogShow .red4Dialogue .red4Dialogue]
    if {$bv==0} {
      cgs4drCursor watch red white
      set data [string trim [$Red4Widgets(EF_ENTRY) get]]
      if {$data=="" || $data==$Red4Widgets(DRO)} {
        cgs4drClear $taskname
        cgs4drInform $taskname "red4Efficiency error : A dataset has not been specified properly!"
      } else {
        set Red4Widgets(RO) $data
        $taskname obey efficiency "data=$data" -inform "cgs4drInform $taskname %V"
      }
    }

# Remove the dialog box
    cgs4drCursor arrow green black
    destroy .red4Dialogue
}
