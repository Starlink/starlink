proc red4ModelBB {taskname} {
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
    set frame [dialogStart .red4Dialogue "Red4 Model Black Body" 0 OK Cancel]
    cgs4drCursor pirate orange black
    .red4Dialogue config -cursor {arrow green black}

# Create and pack dialog box widgets
    set top [frame $frame.top]
    set bot [frame $frame.bot]
    pack $top $bot -in $frame -side top

    set Red4Widgets(BB_LAB01) [label $top.l1 -text "Filename"]
    set Red4Widgets(BB_ENT01) [entry $top.e1 -width 40]
    pack $Red4Widgets(BB_LAB01) $Red4Widgets(BB_ENT01) -in $top -side left
    $Red4Widgets(BB_ENT01) insert end $Red4Widgets(RO)

    set Red4Widgets(BB_LAB02) [label $bot.l2 -text "Temperature"]
    set Red4Widgets(BB_ENT02) [entry $bot.e2]
    set Red4Widgets(BB_LAB03) [label $bot.l3 -text "Wavelength"]
    set Red4Widgets(BB_ENT03) [entry $bot.e3]
    set Red4Widgets(BB_LAB04) [label $bot.l4 -text "Flux"]
    set Red4Widgets(BB_ENT04) [entry $bot.e4]
    pack $Red4Widgets(BB_LAB02) $Red4Widgets(BB_ENT02) $Red4Widgets(BB_LAB03) \
      $Red4Widgets(BB_ENT03) $Red4Widgets(BB_LAB04) $Red4Widgets(BB_ENT04) -in $bot -side left -pady 2m
    $Red4Widgets(BB_ENT02) insert end $Red4Widgets(BB_DT)
    $Red4Widgets(BB_ENT03) insert end $Red4Widgets(BB_DW)
    $Red4Widgets(BB_ENT04) insert end $Red4Widgets(BB_DF)

# Bind the defaults to Button-2
    bind $Red4Widgets(BB_LAB01) <Button-2> "red4Update red4ModelBB ALL"
    bind $Red4Widgets(BB_LAB02) <Button-2> "red4Update red4ModelBB ALL"
    bind $Red4Widgets(BB_LAB03) <Button-2> "red4Update red4ModelBB ALL"
    bind $Red4Widgets(BB_LAB04) <Button-2> "red4Update red4ModelBB ALL"
    bind $Red4Widgets(BB_ENT01) <Button-2> "red4Update red4ModelBB BB_ENT01"
    bind $Red4Widgets(BB_ENT02) <Button-2> "red4Update red4ModelBB BB_ENT02"
    bind $Red4Widgets(BB_ENT03) <Button-2> "red4Update red4ModelBB BB_ENT03"
    bind $Red4Widgets(BB_ENT04) <Button-2> "red4Update red4ModelBB BB_ENT04"
    bind $Red4Widgets(BB_ENT01) <Double-Button-2> "$Red4Widgets(BB_ENT01) delete 0 end"
    bind $Red4Widgets(BB_ENT02) <Double-Button-2> "$Red4Widgets(BB_ENT02) delete 0 end"
    bind $Red4Widgets(BB_ENT03) <Double-Button-2> "$Red4Widgets(BB_ENT03) delete 0 end"
    bind $Red4Widgets(BB_ENT04) <Double-Button-2> "$Red4Widgets(BB_ENT04) delete 0 end"

# Show the dialog box
    set bv [dialogShow .red4Dialogue .red4Dialogue]
    if {$bv==0} {
      cgs4drCursor watch red white
      set Red4Widgets(RO) [string trim [$Red4Widgets(BB_ENT01) get]]
      set Red4Widgets(BB_DT) [string trim [$Red4Widgets(BB_ENT02) get]]
      set Red4Widgets(BB_DW) [string trim [$Red4Widgets(BB_ENT03) get]]
      set Red4Widgets(BB_DF) [string trim [$Red4Widgets(BB_ENT04) get]]
      if {$Red4Widgets(RO)=="" || $Red4Widgets(RO)==$Red4Widgets(DOB)} {
        cgs4drClear $taskname
        cgs4drInform $taskname "red4ModelBB error : A dataset has not been specified properly!"
      } else {

# Remove observation
        set out $Red4Widgets(RO)_bb
        set message "Generating model black body from $Red4Widgets(RO) output to $out"
        cgs4drInform $taskname $message
        set param "template=$Red4Widgets(RO) bb_temp=$Red4Widgets(BB_DT) refwave=$Red4Widgets(BB_DW)"
        set param "$param refflux=$Red4Widgets(BB_DF) output=$out"
        $taskname obey black_body "$param" -inform "cgs4drInform $taskname %V"
      }
    }

# Destroy the box
    cgs4drCursor arrow green black
    destroy .red4Dialogue
}
