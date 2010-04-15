proc qmanERange {taskname} {

# Get some global variables
    global env
    global QmanWidgets
    global cgs4drHtml

# Create dialog box.
    if {[winfo exists .qmanDialogue]} {destroy .qmanDialogue}
    set frame [dialogStart .qmanDialogue "Enter or Remove End Range" 0 Enter Remove Cancel]
    cgs4drCursor pirate orange black
    .qmanDialogue config -cursor {arrow green black}

# Create end group layout panel.
    set l1 [label $frame.l1 -text "First"]
    set QmanWidgets(END_RANGE_FIRST) [entry $frame.e1]
    $QmanWidgets(END_RANGE_FIRST) delete 0 end
    $QmanWidgets(END_RANGE_FIRST) insert end $QmanWidgets(DEF)
    set l2 [label $frame.l2 -text "Last"]
    set QmanWidgets(END_RANGE_LAST) [entry $frame.e2]
    $QmanWidgets(END_RANGE_LAST) delete 0 end
    $QmanWidgets(END_RANGE_LAST) insert end $QmanWidgets(DEL)
    pack $l1 $QmanWidgets(END_RANGE_FIRST) $l2 $QmanWidgets(END_RANGE_LAST) -side left -padx 1m

# Bind the widgets
    bind $l1 <Button-2> "qmanUpdate qmanERange ALL"
    bind $l2 <Button-2> "qmanUpdate qmanERange ALL"
    bind $QmanWidgets(END_RANGE_FIRST) <Button-2> \
      "$QmanWidgets(END_RANGE_FIRST) delete 0 end; $QmanWidgets(END_RANGE_FIRST) insert end 1"
    bind $QmanWidgets(END_RANGE_FIRST) <Double-Button-2> "$QmanWidgets(END_RANGE_FIRST) delete 0 end"
    bind $QmanWidgets(END_RANGE_LAST) <Button-2> \
      "$QmanWidgets(END_RANGE_LAST) delete 0 end; $QmanWidgets(END_RANGE_LAST) insert end 1"
    bind $QmanWidgets(END_RANGE_LAST) <Double-Button-2> "$QmanWidgets(END_RANGE_LAST) delete 0 end"
    bind $l1 <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/qmanERangeBox1.html"
    bind $QmanWidgets(END_RANGE_FIRST) <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/qmanERangeBox1.html"
    bind $l2 <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/qmanERangeBox1.html"
    bind $QmanWidgets(END_RANGE_LAST) <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/qmanERangeBox1.html"

# Show the dialog box
    set bv [dialogShow .qmanDialogue .qmanDialogue]
    if {$bv == 0} {
      cgs4drCursor watch red white
      qmanEnterEndRange $taskname
    } elseif {$bv == 1} {
      cgs4drCursor watch red white
      qmanCancelEndRange $taskname
    }

# Remove the dialog box
    cgs4drCursor arrow green black
    if {[winfo exists .qmanDialogue]} {destroy .qmanDialogue}
}
