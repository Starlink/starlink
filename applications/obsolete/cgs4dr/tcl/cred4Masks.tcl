proc cred4Masks {taskname} {

# Get some global values
    global Cred4NoticeBoard
    global Cred4Widgets
    global cgs4drHtml

# Create dialog box
    if {[winfo exists .cred4Dialogue]} {destroy .cred4Dialogue}
    set frame [dialogStart .cred4Dialogue "Cred4 Masks/Lincoeffs Setup" 0 OK Cancel]
    cgs4drCursor pirate orange black
    .cred4Dialogue config -cursor {arrow green black}

# Create sub-frames for data widgets
    set mid [frame $frame.mid]
    set bot [frame $frame.bot]
    pack $mid $bot -side top

    set labpm [label $mid.labpm -text "Bad Pixel Mask    "]
    set Cred4Widgets(MASK) [entry $mid.mask -relief sunken -bd 2 -width 20]
    pack $labpm $Cred4Widgets(MASK) -in $mid -side left
    $Cred4Widgets(MASK) insert 0 [string trim [nbs get ${Cred4NoticeBoard}.miscellaneous.mask]]
    bind $labpm <Button-2> "cred4Update cred4Masks ALL"
    bind $labpm <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/cred4MasksBox1.html"
    bind $Cred4Widgets(MASK) <Button-2> "cred4Update cred4Masks MASK"
    bind $Cred4Widgets(MASK) <Double-Button-2> "$Cred4Widgets(MASK) delete 0 end"
    bind $Cred4Widgets(MASK) <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/cred4MasksBox1.html"

    set lalin [label $bot.lalin -text "Linearisation File"]
    set Cred4Widgets(LINCOEFFS) [entry $bot.line -relief sunken -bd 2 -width 20]
    pack $lalin $Cred4Widgets(LINCOEFFS) -in $bot -side left
    $Cred4Widgets(LINCOEFFS) insert 0 [string trim [nbs get ${Cred4NoticeBoard}.miscellaneous.lincoeffs]]
    bind $lalin <Button-2> "cred4Update cred4Masks ALL"
    bind $lalin <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/cred4MasksBox1.html"
    bind $Cred4Widgets(LINCOEFFS) <Button-2> "cred4Update cred4Masks LINCOEFFS"
    bind $Cred4Widgets(LINCOEFFS) <Double-Button-2> "$Cred4Widgets(LINCOEFFS) delete 0 end"
    bind $Cred4Widgets(LINCOEFFS) <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/cred4MasksBox1.html"

# Show the dialog box
    set bv [dialogShow .cred4Dialogue .cred4Dialogue]
    if {$bv == 0} {
      cgs4drCursor watch red white
      cgs4drClear $taskname

#   Set the bad pixel mask and linearisation file
      set file [string trim [$Cred4Widgets(MASK) get]]
      nbs put ${Cred4NoticeBoard}.miscellaneous.mask $file
      set file [string trim [$Cred4Widgets(LINCOEFFS) get]]
      nbs put ${Cred4NoticeBoard}.miscellaneous.lincoeffs $file
    }

# Remove the dialog box
    cgs4drCursor arrow green black
    destroy .cred4Dialogue
}
