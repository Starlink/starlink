proc cred4Extract {taskname} {

# Get some global values
    global Cred4NoticeBoard
    global Cred4Widgets
    global cgs4drHtml

# Create dialog box
    if {[winfo exists .cred4Dialogue]} {destroy .cred4Dialogue}
    set frame [dialogStart .cred4Dialogue "Cred4 Extract Spectra Parameters Setup" 0 OK Cancel]
    cgs4drCursor pirate orange black
    .cred4Dialogue config -cursor {arrow green black}

# Middle-Right contains Extract Nodded Spectrum Parameters
    set midtop [frame $frame.mid]
    set bottop [frame $frame.bot]
    set basetop [frame $frame.bas]
    set lbasetop [frame $frame.lba]
    pack $midtop $bottop $basetop $lbasetop -in $frame

    set label [label $midtop.lb -text "Algorithm"]
    set f1 [radiobutton $midtop.f1 -text "Bright" -variable Cred4Widgets(SPC_ALGORITHM) -value "BRIGHT"]
    set f2 [radiobutton $midtop.f2 -text "Faint" -variable Cred4Widgets(SPC_ALGORITHM) -value "FAINT"]
    set iv [checkbutton $midtop.we -text "Invert" -variable Cred4Widgets(SPC_INVERT)]
    set l4 [label $midtop.l4 -text " "]
    pack $label $f1 $f2 $l4 -in $midtop -side left
    pack $iv -in $midtop -side right
    set Cred4Widgets(SPC_ALGORITHM) [string trim [string toupper [nbs get ${Cred4NoticeBoard}.reduction.extract_spc.algorithm]]]
    set Cred4Widgets(SPC_INVERT) [nbs get ${Cred4NoticeBoard}.reduction.extract_spc.invert]
    bind $label <Button-2> "cred4Update cred4Extract ALL"
    bind $l4 <Button-2> "cred4Update cred4Extract ALL"
    bind $f1 <Button-2> "set Cred4Widgets(SPC_ALGORITHM) BRIGHT"
    bind $f2 <Button-2> "set Cred4Widgets(SPC_ALGORITHM) BRIGHT"
    bind $iv <Button-2> "set Cred4Widgets(SPC_INVERT) 0"
    bind $label <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/cred4ExtractBox1.html"
    bind $l4 <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/cred4ExtractBox1.html"
    bind $f1 <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/cred4ExtractBox1.html"
    bind $f2 <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/cred4ExtractBox1.html"
    bind $iv <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/cred4ExtractBox1.html"

    set l1 [label $bottop.l1 -text "Start"]
    set l2 [label $bottop.l2 -text "End"]
    set l3 [label $bottop.l3 -text "Top Extraction Row" -width 20]
    set l4 [label $bottop.l4 -text " "]
    set Cred4Widgets(SPC_ROW1S) [entry $bottop.s1 -relief sunken -bd 2]
    set Cred4Widgets(SPC_ROW1E) [entry $bottop.e1 -relief sunken -bd 2]
    pack $l3 $l4 -in $bottop -side left
    pack $Cred4Widgets(SPC_ROW1E) $l2 $Cred4Widgets(SPC_ROW1S) $l1 -in $bottop -side right
    $Cred4Widgets(SPC_ROW1S) delete 0 end
    $Cred4Widgets(SPC_ROW1S) insert end [string trim [nbs get ${Cred4NoticeBoard}.reduction.extract_spc.row1s]]
    $Cred4Widgets(SPC_ROW1E) delete 0 end
    $Cred4Widgets(SPC_ROW1E) insert end [string trim [nbs get ${Cred4NoticeBoard}.reduction.extract_spc.row1e]]
    bind $Cred4Widgets(SPC_ROW1S) <Button-2> "cred4Update cred4Extract SPC_ROW1S"
    bind $Cred4Widgets(SPC_ROW1E) <Button-2> "cred4Update cred4Extract SPC_ROW1E"
    bind $Cred4Widgets(SPC_ROW1S) <Double-Button-2> "$Cred4Widgets(SPC_ROW1S) delete 0 end"
    bind $Cred4Widgets(SPC_ROW1E) <Double-Button-2> "$Cred4Widgets(SPC_ROW1E) delete 0 end"
    bind $Cred4Widgets(SPC_ROW1S) <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/cred4ExtractBox1.html"
    bind $Cred4Widgets(SPC_ROW1E) <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/cred4ExtractBox1.html"
    bind $l1 <Button-2> "cred4Update cred4Extract ALL"
    bind $l2 <Button-2> "cred4Update cred4Extract ALL"
    bind $l3 <Button-2> "cred4Update cred4Extract ALL"
    bind $l4 <Button-2> "cred4Update cred4Extract ALL"
    bind $l1 <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/cred4ExtractBox1.html"
    bind $l2 <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/cred4ExtractBox1.html"
    bind $l3 <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/cred4ExtractBox1.html"
    bind $l4 <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/cred4ExtractBox1.html"

    set l1 [label $basetop.l1 -text "Start"]
    set l2 [label $basetop.l2 -text "End"]
    set l3 [label $basetop.l3 -text "Middle Extraction Row" -width 20]
    set l4 [label $basetop.l4 -text " "]
    set Cred4Widgets(SPC_ROW2S) [entry $basetop.s1 -relief sunken -bd 2]
    set Cred4Widgets(SPC_ROW2E) [entry $basetop.e1 -relief sunken -bd 2]
    pack $l3 $l4 -in $basetop -side left
    pack $Cred4Widgets(SPC_ROW2E) $l2 $Cred4Widgets(SPC_ROW2S) $l1 -in $basetop -side right
    $Cred4Widgets(SPC_ROW2S) delete 0 end
    $Cred4Widgets(SPC_ROW2S) insert end [string trim [nbs get ${Cred4NoticeBoard}.reduction.extract_spc.row2s]]
    $Cred4Widgets(SPC_ROW2E) delete 0 end
    $Cred4Widgets(SPC_ROW2E) insert end [string trim [nbs get ${Cred4NoticeBoard}.reduction.extract_spc.row2e]]
    bind $Cred4Widgets(SPC_ROW2S) <Button-2> "cred4Update cred4Extract SPC_ROW2S"
    bind $Cred4Widgets(SPC_ROW2E) <Button-2> "cred4Update cred4Extract SPC_ROW2E"
    bind $Cred4Widgets(SPC_ROW2S) <Double-Button-2> "$Cred4Widgets(SPC_ROW2S) delete 0 end"
    bind $Cred4Widgets(SPC_ROW2E) <Double-Button-2> "$Cred4Widgets(SPC_ROW2E) delete 0 end"
    bind $Cred4Widgets(SPC_ROW2S) <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/cred4ExtractBox1.html"
    bind $Cred4Widgets(SPC_ROW2E) <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/cred4ExtractBox1.html"
    bind $l1 <Button-2> "cred4Update cred4Extract ALL"
    bind $l2 <Button-2> "cred4Update cred4Extract ALL"
    bind $l3 <Button-2> "cred4Update cred4Extract ALL"
    bind $l4 <Button-2> "cred4Update cred4Extract ALL"
    bind $l1 <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/cred4ExtractBox1.html"
    bind $l2 <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/cred4ExtractBox1.html"
    bind $l3 <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/cred4ExtractBox1.html"
    bind $l4 <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/cred4ExtractBox1.html"

    set l1 [label $lbasetop.l1 -text "Start"]
    set l2 [label $lbasetop.l2 -text "End"]
    set l3 [label $lbasetop.l3 -text "Bottom Extraction Row" -width 20]
    set l4 [label $lbasetop.l4 -text " "]
    set Cred4Widgets(SPC_ROW3S) [entry $lbasetop.s1 -relief sunken -bd 2]
    set Cred4Widgets(SPC_ROW3E) [entry $lbasetop.e1 -relief sunken -bd 2]
    pack $l3 $l4 -in $lbasetop -side left
    pack $Cred4Widgets(SPC_ROW3E) $l2 $Cred4Widgets(SPC_ROW3S) $l1 -in $lbasetop -side right
    $Cred4Widgets(SPC_ROW3S) delete 0 end
    $Cred4Widgets(SPC_ROW3S) insert end [string trim [nbs get ${Cred4NoticeBoard}.reduction.extract_spc.row3s]]
    $Cred4Widgets(SPC_ROW3E) delete 0 end
    $Cred4Widgets(SPC_ROW3E) insert end [string trim [nbs get ${Cred4NoticeBoard}.reduction.extract_spc.row3e]]
    bind $Cred4Widgets(SPC_ROW3S) <Button-2> "cred4Update cred4Extract SPC_ROW3S"
    bind $Cred4Widgets(SPC_ROW3E) <Button-2> "cred4Update cred4Extract SPC_ROW3E"
    bind $Cred4Widgets(SPC_ROW3S) <Double-Button-2> "$Cred4Widgets(SPC_ROW3S) delete 0 end"
    bind $Cred4Widgets(SPC_ROW3E) <Double-Button-2> "$Cred4Widgets(SPC_ROW3E) delete 0 end"
    bind $Cred4Widgets(SPC_ROW3S) <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/cred4ExtractBox1.html"
    bind $Cred4Widgets(SPC_ROW3E) <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/cred4ExtractBox1.html"
    bind $l1 <Button-2> "cred4Update cred4Extract ALL"
    bind $l2 <Button-2> "cred4Update cred4Extract ALL"
    bind $l3 <Button-2> "cred4Update cred4Extract ALL"
    bind $l4 <Button-2> "cred4Update cred4Extract ALL"
    bind $l1 <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/cred4ExtractBox1.html"
    bind $l2 <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/cred4ExtractBox1.html"
    bind $l3 <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/cred4ExtractBox1.html"
    bind $l4 <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/cred4ExtractBox1.html"

# Show the dialog box
    set bv [dialogShow .cred4Dialogue .cred4Dialogue]
    if {$bv == 0} {
      cgs4drCursor watch red white

#   Set the extract nodded spectra parameters
      nbs put ${Cred4NoticeBoard}.reduction.extract_spc.algorithm $Cred4Widgets(SPC_ALGORITHM)
      nbs put ${Cred4NoticeBoard}.reduction.extract_spc.invert $Cred4Widgets(SPC_INVERT)
      set row1s [string trim [$Cred4Widgets(SPC_ROW1S) get]]
      nbs put ${Cred4NoticeBoard}.reduction.extract_spc.row1s $row1s
      set row1e [string trim [$Cred4Widgets(SPC_ROW1E) get]]
      nbs put ${Cred4NoticeBoard}.reduction.extract_spc.row1e $row1e
      set row2s [string trim [$Cred4Widgets(SPC_ROW2S) get]]
      nbs put ${Cred4NoticeBoard}.reduction.extract_spc.row2s $row2s
      set row2e [string trim [$Cred4Widgets(SPC_ROW2E) get]]
      nbs put ${Cred4NoticeBoard}.reduction.extract_spc.row2e $row2e
      set row3s [string trim [$Cred4Widgets(SPC_ROW3S) get]]
      nbs put ${Cred4NoticeBoard}.reduction.extract_spc.row3s $row3s
      set row3e [string trim [$Cred4Widgets(SPC_ROW3E) get]]
      nbs put ${Cred4NoticeBoard}.reduction.extract_spc.row3e $row3e
    }

# Remove the dialog box
    cgs4drCursor arrow green black
    destroy .cred4Dialogue
}
