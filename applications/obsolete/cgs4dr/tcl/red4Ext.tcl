proc red4Ext {taskname} {
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
    set frame [dialogStart .red4Dialogue "Red4 Extract Spectrum" 0 OK Cancel]
    cgs4drCursor pirate orange black
    .red4Dialogue config -cursor {arrow green black}

# Create and pack dialog box widgets
    set toptop [frame $frame.tt]
    set midtop [frame $frame.mt]
    set bottop [frame $frame.bt]
    set basetop [frame $frame.at]
    set lbasetop [frame $frame.lt]
    pack $toptop $midtop $bottop $basetop $lbasetop -in $frame -side top

# Create, pack and bind filename widget
    set Red4Widgets(SPC_LAB01) [label $toptop.lb -text "Filename"]
    set Red4Widgets(SPC_ENT01) [entry $toptop.e0 -width 40]
    pack $Red4Widgets(SPC_LAB01) $Red4Widgets(SPC_ENT01) -in $toptop -side left
    $Red4Widgets(SPC_ENT01) insert end $Red4Widgets(RG)
    bind $Red4Widgets(SPC_LAB01) <Button-2> "red4Update red4Ext ALL"
    bind $Red4Widgets(SPC_ENT01) <Button-2> "red4Update red4Ext SPC_ENT01"
    bind $Red4Widgets(SPC_ENT01) <Double-Button-2> "$Red4Widgets(SPC_ENT01) delete 0 end"
    
# Create, pack and bind algorithm and invert widgets
    set Red4Widgets(SPC_LAB02) [label $midtop.lb -text "Algorithm"]
    set f1 [radiobutton $midtop.f1 -text "Bright" -variable Red4Widgets(SPC_ALGORITHM) -value "BRIGHT"]
    set f2 [radiobutton $midtop.f2 -text "Faint" -variable Red4Widgets(SPC_ALGORITHM) -value "FAINT"]
    set iv [checkbutton $midtop.we -text "Invert" -variable Red4Widgets(SPC_INVERT)]
    set Red4Widgets(SPC_LAB03) [label $midtop.l4 -text " "]
    pack $Red4Widgets(SPC_LAB02) $f1 $f2 $Red4Widgets(SPC_LAB03) -in $midtop -side left -pady 2m
    pack $iv -in $midtop -side right -pady 2m
    set Red4Widgets(SPC_ALGORITHM) BRIGHT
    set Red4Widgets(SPC_INVERT) 0
    bind $Red4Widgets(SPC_LAB02) <Button-2> "red4Update red4Ext ALL"
    bind $Red4Widgets(SPC_LAB03) <Button-2> "red4Update red4Ext ALL"
    bind $f1 <Button-2> "red4Update red4Ext SPC_ALGORITHM"
    bind $f2 <Button-2> "red4Update red4Ext SPC_ALGORITHM"
    bind $iv <Button-2> "red4Update red4Ext SPC_INVERT"

# Create, pack and bind first extraction widget
    set Red4Widgets(SPC_LAB04) [label $bottop.l1 -text "Start"]  
    set Red4Widgets(SPC_LAB05) [label $bottop.l2 -text "End"]  
    set Red4Widgets(SPC_LAB06) [label $bottop.l3 -text "Top Extraction Row" -width 20]  
    set Red4Widgets(SPC_LAB07) [label $bottop.l4 -text " "]
    set Red4Widgets(SPC_ENT02) [entry $bottop.s1]
    set Red4Widgets(SPC_ENT03) [entry $bottop.e1]
    pack $Red4Widgets(SPC_LAB06) $Red4Widgets(SPC_LAB07) -in $bottop -side left -pady 2m
    pack $Red4Widgets(SPC_ENT03) $Red4Widgets(SPC_LAB05) $Red4Widgets(SPC_ENT02) $Red4Widgets(SPC_LAB04) \
      -in $bottop -side right -pady 2m
    $Red4Widgets(SPC_ENT02) insert end 29 
    $Red4Widgets(SPC_ENT03) insert end 29 
    bind $Red4Widgets(SPC_LAB04) <Button-2> "red4Update red4Ext ALL"
    bind $Red4Widgets(SPC_LAB05) <Button-2> "red4Update red4Ext ALL"
    bind $Red4Widgets(SPC_LAB06) <Button-2> "red4Update red4Ext ALL"
    bind $Red4Widgets(SPC_LAB07) <Button-2> "red4Update red4Ext ALL"
    bind $Red4Widgets(SPC_ENT02) <Button-2> "red4Update red4Ext SPC_ENT02"
    bind $Red4Widgets(SPC_ENT02) <Double-Button-2> "$Red4Widgets(SPC_ENT02) delete 0 end"
    bind $Red4Widgets(SPC_ENT03) <Button-2> "red4Update red4Ext SPC_ENT03"
    bind $Red4Widgets(SPC_ENT03) <Double-Button-2> "$Red4Widgets(SPC_ENT03) delete 0 end"

# Create, pack and bind second extraction widget
    set Red4Widgets(SPC_LAB08) [label $basetop.l1 -text "Start"]  
    set Red4Widgets(SPC_LAB09) [label $basetop.l2 -text "End"]  
    set Red4Widgets(SPC_LAB10) [label $basetop.l3 -text "Middle Extraction Row" -width 20]  
    set Red4Widgets(SPC_LAB11) [label $basetop.l4 -text " "]
    set Red4Widgets(SPC_ENT04) [entry $basetop.s2]
    set Red4Widgets(SPC_ENT05) [entry $basetop.e2]
    pack $Red4Widgets(SPC_LAB10) $Red4Widgets(SPC_LAB11) -in $basetop -side left -pady 2m
    pack $Red4Widgets(SPC_ENT05) $Red4Widgets(SPC_LAB09) $Red4Widgets(SPC_ENT04) $Red4Widgets(SPC_LAB08) \
       -in $basetop -side right -pady 2m
    $Red4Widgets(SPC_ENT04) insert end -1 
    $Red4Widgets(SPC_ENT05) insert end -1 
    bind $Red4Widgets(SPC_LAB08) <Button-2> "red4Update red4Ext ALL"
    bind $Red4Widgets(SPC_LAB09) <Button-2> "red4Update red4Ext ALL"
    bind $Red4Widgets(SPC_LAB10) <Button-2> "red4Update red4Ext ALL"
    bind $Red4Widgets(SPC_LAB11) <Button-2> "red4Update red4Ext ALL"
    bind $Red4Widgets(SPC_ENT04) <Button-2> "red4Update red4Ext SPC_ENT04"
    bind $Red4Widgets(SPC_ENT04) <Double-Button-2> "$Red4Widgets(SPC_ENT04) delete 0 end"
    bind $Red4Widgets(SPC_ENT05) <Button-2> "red4Update red4Ext SPC_ENT05"
    bind $Red4Widgets(SPC_ENT05) <Double-Button-2> "$Red4Widgets(SPC_ENT05) delete 0 end"

# Create, pack and bind third extraction widget
    set Red4Widgets(SPC_LAB12) [label $lbasetop.l1 -text "Start"]  
    set Red4Widgets(SPC_LAB13) [label $lbasetop.l2 -text "End"]  
    set Red4Widgets(SPC_LAB14) [label $lbasetop.l3 -text "Bottom Extraction Row" -width 20]  
    set Red4Widgets(SPC_LAB15) [label $lbasetop.l4 -text " "]
    set Red4Widgets(SPC_ENT06) [entry $lbasetop.s3]
    set Red4Widgets(SPC_ENT07) [entry $lbasetop.e3]
    pack $Red4Widgets(SPC_LAB14) $Red4Widgets(SPC_LAB15) -in $lbasetop -side left -pady 2m
    pack $Red4Widgets(SPC_ENT07) $Red4Widgets(SPC_LAB13) $Red4Widgets(SPC_ENT06) $Red4Widgets(SPC_LAB12) \
       -in $lbasetop -side right -pady 2m
    $Red4Widgets(SPC_ENT06) insert end -1 
    $Red4Widgets(SPC_ENT07) insert end -1 
    bind $Red4Widgets(SPC_LAB12) <Button-2> "red4Update red4Ext ALL"
    bind $Red4Widgets(SPC_LAB13) <Button-2> "red4Update red4Ext ALL"
    bind $Red4Widgets(SPC_LAB14) <Button-2> "red4Update red4Ext ALL"
    bind $Red4Widgets(SPC_LAB15) <Button-2> "red4Update red4Ext ALL"
    bind $Red4Widgets(SPC_ENT06) <Button-2> "red4Update red4Ext SPC_ENT06"
    bind $Red4Widgets(SPC_ENT06) <Double-Button-2> "$Red4Widgets(SPC_ENT06) delete 0 end"
    bind $Red4Widgets(SPC_ENT07) <Button-2> "red4Update red4Ext SPC_ENT07"
    bind $Red4Widgets(SPC_ENT07) <Double-Button-2> "$Red4Widgets(SPC_ENT07) delete 0 end"

# Show the dialog box
    set bv [dialogShow .red4Dialogue .red4Dialogue]
    if {$bv==0} {
      cgs4drCursor watch red white
      set data [string trim [$Red4Widgets(SPC_ENT01) get]]
      if {$data=="" || $data==$Red4Widgets(DRG)} {
        cgs4drClear $taskname
        cgs4drInform $taskname "red4Ext error : A dataset has not been specified properly!"
      } else {
        set Red4Widgets(RG) $data
        set spect ${data}_spc
        set ispect ${data}_imspc
        set row1s [string trim [$Red4Widgets(SPC_ENT02) get]]
        set row1e [string trim [$Red4Widgets(SPC_ENT03) get]]
        set row2s [string trim [$Red4Widgets(SPC_ENT04) get]]
        set row2e [string trim [$Red4Widgets(SPC_ENT05) get]]
        set row3s [string trim [$Red4Widgets(SPC_ENT06) get]]
        set row3e [string trim [$Red4Widgets(SPC_ENT07) get]]

# Convert 0 to true/false 
        set message "Extracting spectrum from $data into $spect and $ispect"
        cgs4drInform $taskname $message
        if {$Red4Widgets(SPC_INVERT) == 0} {
          $taskname obey nodextract4 "image=$data invert_spec=FALSE algorithm=$Red4Widgets(SPC_ALGORITHM) \
            row1s=$row1s row1e=$row1e row2s=$row2s row2e=$row2e row3s=$row3s row3e=$row3e \
            spect=$spect ispect=$ispect" -inform "cgs4drInform $taskname %V"
        } else {
          $taskname obey nodextract4 "image=$data invert_spec=TRUE algorithm=$Red4Widgets(SPC_ALGORITHM) \
            row1s=$row1s row1e=$row1e row2s=$row2s row2e=$row2e row3s=$row3s row3e=$row3e \
            spect=$spect ispect=$ispect" -inform "cgs4drInform $taskname %V"
        }
      }
    }

# Remove the dialog box
    cgs4drCursor arrow green black
    destroy .red4Dialogue
}
