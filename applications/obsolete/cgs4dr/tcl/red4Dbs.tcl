proc red4Dbs {taskname} {

# Get some global values
    global env
    global Red4Widgets

# Check to see if task is busy
    set status [cgs4drCheckTask red4]
    if {$status!=0} {return}

# Create dialog box
    if {[winfo exists .red4Dialogue]} {destroy .red4Dialogue}
    set frame [dialogStart .red4Dialogue "Red4 Divide By Standard" 0 OK Cancel]
    cgs4drCursor pirate orange black
    .red4Dialogue config -cursor {arrow green black}

# Create and pack dialog box widgets
    set Red4Widgets(DBS_LABEL) [label $frame.lb -text "Filename"]
    set Red4Widgets(DBS_ENTRY) [entry $frame.en -width 40]
    pack $Red4Widgets(DBS_LABEL) $Red4Widgets(DBS_ENTRY) -padx 2 -in $frame -side left
    $Red4Widgets(DBS_ENTRY) insert end $Red4Widgets(RG)

# Bind defaults to Button-2
    bind $Red4Widgets(DBS_LABEL) <Button-2> "red4Update red4Dbs DBS_ENTRY"
    bind $Red4Widgets(DBS_ENTRY) <Button-2> "red4Update red4Dbs DBS_ENTRY"
    bind $Red4Widgets(DBS_ENTRY) <Double-Button-2> "$Red4Widgets(DBS_ENTRY) delete 0 end"

# Show the dialog box
    set bv [dialogShow .red4Dialogue .red4Dialogue]
    if {$bv==0} {
      cgs4drCursor watch red white
      set data [string trim [$Red4Widgets(DBS_ENTRY) get]]
      if {$data=="" || $data==$Red4Widgets(DRG)} {
        cgs4drClear $taskname
        set message "red4Dbs error : A dataset has not been specified properly!"
        cgs4drInform $taskname $message
      } else {
        set output ${data}_dbs
        set Red4Widgets(RG) $data
        $taskname obey divide_by_std "group=$data output=$output standard_mode='BOTH'" -inform "cgs4drInform $taskname %V"
      }
    }

# Remove the dialog box
    cgs4drCursor arrow green black
    destroy .red4Dialogue
}
