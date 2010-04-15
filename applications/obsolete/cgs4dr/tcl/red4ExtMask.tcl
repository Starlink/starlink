proc red4ExtMask {taskname} {
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
    set frame [dialogStart .red4Dialogue "Red4 Extract Bad Pixel Mask" 0 OK Cancel]
    cgs4drCursor pirate orange black
    .red4Dialogue config -cursor {arrow green black}

# Create and pack dialog box widgets
    set top [frame $frame.top]
    set bot [frame $frame.bot]
    pack $bot $top -in $frame -side top

    set Red4Widgets(ME_LAB01) [label $top.l1 -text "Mask"]
    set Red4Widgets(ME_ENT01) [entry $top.e1 -width 40]
    pack $Red4Widgets(ME_LAB01) $Red4Widgets(ME_ENT01) -in $top -side left
    $Red4Widgets(ME_ENT01) insert end $Red4Widgets(MBPM)

    set Red4Widgets(ME_LAB02) [label $bot.l2 -text "Data"]
    set Red4Widgets(ME_ENT02) [entry $bot.e2 -width 40]
    pack $Red4Widgets(ME_LAB02) $Red4Widgets(ME_ENT02) -in $bot -side left -pady 2m
    $Red4Widgets(ME_ENT02) insert end $Red4Widgets(RO)

# Bind defaults to Button-2
    bind $Red4Widgets(ME_LAB01) <Button-2> "red4Update red4ExtMask ALL"
    bind $Red4Widgets(ME_LAB02) <Button-2> "red4Update red4ExtMask ALL"
    bind $Red4Widgets(ME_ENT01) <Button-2> "red4Update red4ExtMask ME_ENT01"
    bind $Red4Widgets(ME_ENT02) <Button-2> "red4Update red4ExtMask ME_ENT02"
    bind $Red4Widgets(ME_ENT01) <Double-Button-2> "$Red4Widgets(ME_ENT01) delete 0 end"
    bind $Red4Widgets(ME_ENT02) <Double-Button-2> "$Red4Widgets(ME_ENT02) delete 0 end"

# Show the dialog box
    set bv [dialogShow .red4Dialogue .red4Dialogue]
    if {$bv==0} {
      cgs4drCursor watch red white
      set Red4Widgets(MBPM) [string trim [$Red4Widgets(ME_ENT01) get]]
      set Red4Widgets(RO)   [string trim [$Red4Widgets(ME_ENT02) get]]
      if {$Red4Widgets(RO)=="" || $Red4Widgets(RO)==$Red4Widgets(DRO) || $Red4Widgets(MBPM)=="#"} {
        cgs4drClear $taskname
        cgs4drInform $taskname "red4ExtMask error : A dataset has not been specified properly!"
      } else {

# Extract the mask
        $taskname obey extract_mask "data=$Red4Widgets(RO) mask=$Red4Widgets(MBPM)" -inform "cgs4drInform $taskname %V"
      }
    }

# Destroy the box
    cgs4drCursor arrow green black
    destroy .red4Dialogue
}
