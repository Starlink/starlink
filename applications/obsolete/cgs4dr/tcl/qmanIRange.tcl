proc qmanIRange {taskname} {

# Get some global variables
    global env
    global QmanWidgets
    global cgs4drHtml

# Create dialog box.
    if {[winfo exists .qmanDialogue]} {destroy .qmanDialogue}
    set frame [dialogStart .qmanDialogue "Enter or Remove Integration Range" 0 Enter Remove Cancel]
    cgs4drCursor pirate orange black
    .qmanDialogue config -cursor {arrow green black}

# Create end group layout panel.
    set l0 [label $frame.l0 -text "Observation Number"]
    set QmanWidgets(OBS_NUMBER) [entry $frame.e0]
    $QmanWidgets(OBS_NUMBER) delete 0 end
    $QmanWidgets(OBS_NUMBER) insert end $QmanWidgets(DON)
    set l1 [label $frame.l1 -text "First Integration"]
    set QmanWidgets(INT_RANGE_FIRST) [entry $frame.e1]
    $QmanWidgets(INT_RANGE_FIRST) delete 0 end
    $QmanWidgets(INT_RANGE_FIRST) insert end $QmanWidgets(DIF)
    set l2 [label $frame.l2 -text "Last Integration"]
    set QmanWidgets(INT_RANGE_LAST) [entry $frame.e2]
    $QmanWidgets(INT_RANGE_LAST) delete 0 end
    $QmanWidgets(INT_RANGE_LAST) insert end $QmanWidgets(DIL)
    pack $l0 $QmanWidgets(OBS_NUMBER) $l1 $QmanWidgets(INT_RANGE_FIRST) $l2 $QmanWidgets(INT_RANGE_LAST) -side left -padx 1m

# Bind the widgets
    bind $l0 <Button-2> "qmanUpdate qmanIRange ALL"
    bind $l1 <Button-2> "qmanUpdate qmanIRange ALL"
    bind $l2 <Button-2> "qmanUpdate qmanIRange ALL"
    bind $QmanWidgets(OBS_NUMBER) <Button-2> \
      "$QmanWidgets(OBS_NUMBER) delete 0 end; $QmanWidgets(OBS_NUMBER) insert end 1"
    bind $QmanWidgets(OBS_NUMBER) <Double-Button-2> "$QmanWidgets(OBS_NUMBER) delete 0 end"
    bind $QmanWidgets(INT_RANGE_FIRST) <Button-2> \
      "$QmanWidgets(INT_RANGE_FIRST) delete 0 end; $QmanWidgets(INT_RANGE_FIRST) insert end 1"
    bind $QmanWidgets(INT_RANGE_FIRST) <Double-Button-2> "$QmanWidgets(INT_RANGE_FIRST) delete 0 end"
    bind $QmanWidgets(INT_RANGE_LAST) <Button-2> \
      "$QmanWidgets(INT_RANGE_LAST) delete 0 end; $QmanWidgets(INT_RANGE_LAST) insert end 1"
    bind $QmanWidgets(INT_RANGE_LAST) <Double-Button-2> "$QmanWidgets(INT_RANGE_LAST) delete 0 end"
    bind $l0 <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/qmanIRangeBox1.html"
    bind $l1 <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/qmanIRangeBox1.html"
    bind $l2 <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/qmanIRangeBox1.html"
    bind $QmanWidgets(OBS_NUMBER) <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/qmanIRangeBox1.html"
    bind $QmanWidgets(INT_RANGE_FIRST) <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/qmanIRangeBox1.html"
    bind $QmanWidgets(INT_RANGE_LAST) <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/qmanIRangeBox1.html"

# Show the dialog box
    set bv [dialogShow .qmanDialogue .qmanDialogue]
    if {$bv == 0} {
      cgs4drCursor watch red white
      qmanEnterIntRange $taskname
    } elseif {$bv == 1} {
      cgs4drCursor watch red white
      qmanCancelIntRange $taskname
    }

# Remove the dialog box
    cgs4drCursor arrow green black
    if {[winfo exists .qmanDialogue]} {destroy .qmanDialogue}
}
