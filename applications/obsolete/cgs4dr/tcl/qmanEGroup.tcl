proc qmanEGroup {taskname} {

# Get some global variables
    global env
    global QmanWidgets
    global cgs4drHtml

# Create dialog box.
    if {[winfo exists .qmanDialogue]} {destroy .qmanDialogue}
    set frame [dialogStart .qmanDialogue "Enter or Remove End Group" 0 Enter Remove Cancel]
    cgs4drCursor pirate orange black
    .qmanDialogue config -cursor {arrow green black}

# Create end group layout panel.
    set label [label $frame.l1 -text "Group Number"]
    set QmanWidgets(GROUP_NUMBER) [entry $frame.e1]
    $QmanWidgets(GROUP_NUMBER) delete 0 end
    $QmanWidgets(GROUP_NUMBER) insert end $QmanWidgets(DGN)
    pack $label $QmanWidgets(GROUP_NUMBER) -side left -padx 1m
    bind $label <Button-2> "$QmanWidgets(GROUP_NUMBER) delete 0 end; $QmanWidgets(GROUP_NUMBER) insert end 1"
    bind $QmanWidgets(GROUP_NUMBER) <Button-2> "$QmanWidgets(GROUP_NUMBER) delete 0 end; $QmanWidgets(GROUP_NUMBER) insert end 1"
    bind $QmanWidgets(GROUP_NUMBER) <Double-Button-2> "$QmanWidgets(GROUP_NUMBER) delete 0 end"
    bind $label <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/qmanEGroupBox1.html"
    bind $QmanWidgets(GROUP_NUMBER) <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/qmanEGroupBox1.html"

# Show the dialog box
    set bv [dialogShow .qmanDialogue .qmanDialogue]
    if {$bv == 0} {
      cgs4drCursor watch red white
      qmanEnterEndGroup $taskname
    } elseif {$bv == 1} {
      cgs4drCursor watch red white
      qmanCancelEndGroup $taskname
    }

# Remove the dialog box
    cgs4drCursor arrow green black
    if {[winfo exists .qmanDialogue]} {destroy .qmanDialogue}
}
