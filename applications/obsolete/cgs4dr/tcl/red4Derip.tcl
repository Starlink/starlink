proc red4Derip {taskname} {
#+
# Creates a dialog box for red4 action 
#-
    global Red4Widgets

# Check to see if task is busy
    set status [cgs4drCheckTask red4]
    if {$status!=0} {return}

# Create dialog box
    if {[winfo exists .red4Dialogue]} {destroy .red4Dialogue}
    set frame [dialogStart .red4Dialogue "Red4 Deripple Spectrum" 0 OK Cancel]
    cgs4drCursor pirate orange black
    .red4Dialogue config -cursor {arrow green black}

# Create and pack dialog box widgets
    set toptop [frame $frame.tt]
    set bottop [frame $frame.bt]
    pack $toptop $bottop -in $frame -side top

    set Red4Widgets(DR_LAB01) [label $toptop.lb -text "Filename"]
    set Red4Widgets(DR_ENT01) [entry $toptop.e0 -width 40]
    pack $Red4Widgets(DR_LAB01) $Red4Widgets(DR_ENT01) -in $toptop -side left
    $Red4Widgets(DR_ENT01) insert end $Red4Widgets(SP)

    set Red4Widgets(DR_LAB02) [label $bottop.l1 -text "Start"]  
    set Red4Widgets(DR_LAB03) [label $bottop.l2 -text "End"]  
    set Red4Widgets(DR_LAB04) [label $bottop.l3 -text "Deripple Region" -width 20]  
    set Red4Widgets(DR_LAB05) [label $bottop.l4 -text " "]
    set Red4Widgets(DR_ENT02) [entry $bottop.s1]
    set Red4Widgets(DR_ENT03) [entry $bottop.e1]
    pack $Red4Widgets(DR_LAB04) $Red4Widgets(DR_LAB04) -in $bottop -side left -pady 2m
    pack $Red4Widgets(DR_ENT03) $Red4Widgets(DR_LAB03) $Red4Widgets(DR_ENT02) $Red4Widgets(DR_LAB02) \
       -in $bottop -side right -pady 2m
    $Red4Widgets(DR_ENT02) insert end 0.5 
    $Red4Widgets(DR_ENT03) insert end 255.5 

# Bind default values to Button-2
    bind $Red4Widgets(DR_LAB01) <Button-2> "red4Update red4Derip ALL"
    bind $Red4Widgets(DR_LAB02) <Button-2> "red4Update red4Derip ALL"
    bind $Red4Widgets(DR_LAB03) <Button-2> "red4Update red4Derip ALL"
    bind $Red4Widgets(DR_LAB04) <Button-2> "red4Update red4Derip ALL"
    bind $Red4Widgets(DR_LAB05) <Button-2> "red4Update red4Derip ALL"
    bind $Red4Widgets(DR_ENT01) <Button-2> "red4Update red4Derip DR_ENT01"
    bind $Red4Widgets(DR_ENT02) <Button-2> "red4Update red4Derip DR_ENT02"
    bind $Red4Widgets(DR_ENT03) <Button-2> "red4Update red4Derip DR_ENT03"
    bind $Red4Widgets(DR_ENT01) <Double-Button-2> "$Red4Widgets(DR_ENT01) delete 0 end"
    bind $Red4Widgets(DR_ENT02) <Double-Button-2> "$Red4Widgets(DR_ENT02) delete 0 end"
    bind $Red4Widgets(DR_ENT03) <Double-Button-2> "$Red4Widgets(DR_ENT03) delete 0 end"

# Show the dialog box
    set bv [dialogShow .red4Dialogue .red4Dialogue]
    if {$bv==0} {
      cgs4drCursor watch red white
      set data [string trim [$Red4Widgets(DR_ENT01) get]]
      if {$data=="" || $data==$Red4Widgets(DSP)} {
        cgs4drClear $taskname
        cgs4drInform $taskname "red4Derip error : A dataset has not been specified properly!"
      } else {
        set Red4Widgets(SP) $data
        set irff ${data}_irff
        set derip ${data}_derip
        set xs [string trim [$Red4Widgets(DR_ENT02) get]]
        set xe [string trim [$Red4Widgets(DR_ENT03) get]]
        set message "Derippling $data to form $derip"
        cgs4drInform $taskname $message
        $taskname obey irflat "spectrum=$data output=$irff xstart=$xs xend=$xe more='NO'" -inform "cgs4drInform $taskname %V" \
          -endmsg {set done 1}
        tkwait variable done
        $taskname obey idiv4 "image1=$data image2=$irff output=$derip errors='GAUSSIAN'" -inform "cgs4drInform $taskname %V"
      }
    }

# Remove the dialog box
    cgs4drCursor arrow green black
    destroy .red4Dialogue
}

