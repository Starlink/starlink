proc qmanMisc {taskname} {

# Get some global variables
    global env
    global QmanWidgets
    global cgs4drHtml

# Create dialog box.
    if {[winfo exists .qmanDialogue]} {destroy .qmanDialogue}
    set frame [dialogStart .qmanDialogue "Enter or Remove String" 0 Enter Remove Cancel]
    cgs4drCursor pirate orange black
    .qmanDialogue config -cursor {arrow green black}

# Create end group layout panel.
    set frame1 [frame $frame.f1]
    set frame2 [frame $frame.f2]
    pack $frame1 $frame2
    set l1 [label $frame1.l1 -text "String"]
    set QmanWidgets(STRING) [entry $frame1.entry -width 30 -relief sunken]
    $QmanWidgets(STRING) delete 0 end
    $QmanWidgets(STRING) insert 0 $QmanWidgets(EW_STRING)
    pack $l1 $QmanWidgets(STRING) -side left -padx 1m
    set drmask    [radiobutton $frame2.drmask -text "Mask" -value DRMASK -variable QmanWidgets(RB_STRING)]
    set drconfig  [radiobutton $frame2.drconfig -text "Config" -value DRCONFIG -variable QmanWidgets(RB_STRING)]
    set drskywt   [radiobutton $frame2.drskywt -text "Skywt" -value DRSKYWT -variable QmanWidgets(RB_STRING)]
    set drvarwt   [radiobutton $frame2.drvarwt -text "Varwt" -value DRVARWT -variable QmanWidgets(RB_STRING)]
    set drcomment [radiobutton $frame2.drstring -text "Comment" -value DRCOMMENT -variable QmanWidgets(RB_STRING)]
    pack $drmask $drconfig $drskywt $drvarwt $drcomment -side left -padx 2m

# Bind the widgets
    bind $l1 <Button-2> "$QmanWidgets(STRING) delete 0 end; set QmanWidgets(RB_STRING) DRCOMMENT"
    bind $QmanWidgets(STRING) <Button-2> "$QmanWidgets(STRING) delete 0 end"
    bind $QmanWidgets(STRING) <Double-Button-2> "$QmanWidgets(STRING) delete 0 end"
    bind $l1 <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/qmanMiscBox1.html"
    bind $QmanWidgets(STRING) <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/qmanMiscBox1.html"
    bind $drmask <Button-2> "set QmanWidgets(RB_STRING) DRCOMMENT"
    bind $drconfig <Button-2> "set QmanWidgets(RB_STRING) DRCOMMENT"
    bind $drskywt <Button-2> "set QmanWidgets(RB_STRING) DRCOMMENT"
    bind $drvarwt <Button-2> "set QmanWidgets(RB_STRING) DRCOMMENT"
    bind $drcomment <Button-2> "set QmanWidgets(RB_STRING) DRCOMMENT"
    bind $drmask <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/qmanMiscBox1.html"
    bind $drconfig <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/qmanMiscBox1.html"
    bind $drskywt <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/qmanMiscBox1.html"
    bind $drvarwt <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/qmanMiscBox1.html"
    bind $drcomment <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/qmanMiscBox1.html"

# Show the dialog box
    set bv [dialogShow .qmanDialogue .qmanDialogue]
    if {$bv == 0} {
      cgs4drCursor watch red white
      qmanEnterString $taskname
    } elseif {$bv == 1} {
      cgs4drCursor watch red white
      qmanCancelString $taskname
    }

# Remove the dialog box
    cgs4drCursor arrow green black
    if {[winfo exists .qmanDialogue]} {destroy .qmanDialogue}
}
