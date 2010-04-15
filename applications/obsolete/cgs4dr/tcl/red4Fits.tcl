proc red4Fits {taskname method} {
#+
# Creates a dialog box for red4 action
#-
    global Red4Widgets

# Check to see if task is busy
    set status [cgs4drCheckTask red4]
    if {$status!=0} {return}

# Create dialog box
    if {[winfo exists .red4Dialogue]} {destroy .red4Dialogue}
    set frame [dialogStart .red4Dialogue "Red4 FITS ($method)" 0 OK Cancel]
    cgs4drCursor pirate orange black
    .red4Dialogue config -cursor {arrow green black}

# Create and pack dialog box widgets
    set l1 [label $frame.l1 -text "Filename"]
    set Red4Widgets(FI_ENT01) [entry $frame.e1 -width 40]
    set l2 [label $frame.l2 -text "FITS Item"]
    set Red4Widgets(FI_ENT02) [entry $frame.e2]
    pack $l1 $Red4Widgets(FI_ENT01) $l2 $Red4Widgets(FI_ENT02) -in $frame -side left
    $Red4Widgets(FI_ENT01) insert end $Red4Widgets(OB)

# Bind the buttons
    bind $l1 <Button-2> "red4Update red4Fits ALL"
    bind $l2 <Button-2> "red4Update red4Fits ALL"
    bind $Red4Widgets(FI_ENT01) <Button-2> "red4Update red4Fits FI_ENT01"
    bind $Red4Widgets(FI_ENT01) <Double-Button-2> "$Red4Widgets(FI_ENT01) delete 0 end"
    bind $Red4Widgets(FI_ENT02) <Button-2> "red4Update red4Fits FI_ENT02"
    bind $Red4Widgets(FI_ENT02) <Double-Button-2> "$Red4Widgets(FI_ENT02) delete 0 end"

# Show the dialog box
    set bv [dialogShow .red4Dialogue .red4Dialogue]
    if {$bv==0} {
      cgs4drCursor watch red white
      set data [string trim [$Red4Widgets(FI_ENT01) get]]
      if {$data=="" || $data==$Red4Widgets(DOB)} {
        cgs4drClear $taskname
        cgs4drInform $taskname "red4Fits error : A dataset has not been specified properly!"
      } else {
        set item [string trim [$Red4Widgets(FI_ENT02) get]]
        if {$item==""} {
          cgs4drClear $taskname
          cgs4drInform $taskname "red4Fits error : A FITS item has not been specified properly!"
        } else {
          set Red4Widgets(OB) $data
          $taskname obey poke_fits "data=$data item=$item method=$method mode='VERBOSE'" -inform "cgs4drInform $taskname %V"
        }
      }
    }

# Remove the dialog box
    cgs4drCursor arrow green black
    destroy .red4Dialogue
}
