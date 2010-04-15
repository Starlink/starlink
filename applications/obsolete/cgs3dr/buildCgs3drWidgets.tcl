proc buildCgs3drWidgets w {
    global Cgs3drWidgets
    global Red3Widgets

# Create frame for the widget tree.
    set mainFrame [frame $w.main]
    pack $mainFrame -expand yes -fill both

# Build panel layout frames.
    set topFrame [frame $mainFrame.ft -relief flat -bd 2]
    set bottomFrame [frame $mainFrame.fb -relief sunken -bd 2]
    pack $topFrame $bottomFrame -fill both -expand yes

# Create scrolling region for output in bottom frame
    set scrollbar [scrollbar $bottomFrame.scrollbar -orient vertical -relief sunken -bd 2]
    set Cgs3drWidgets(OUTPUT) [text $bottomFrame.text -state disabled -wrap word -relief sunken -bd 2 -width 60]
    $scrollbar configure -command "$Cgs3drWidgets(OUTPUT) yview"
    $Cgs3drWidgets(OUTPUT) configure -yscroll "$scrollbar set"
    pack $scrollbar -side right -fill y
    pack $Cgs3drWidgets(OUTPUT) -side left -fill both -expand yes

# Sub-divide the top-frame for cgs3dr and red3
    set leftFrame [frame $topFrame.left -relief sunken -bd 2]
    set rightFrame [frame $topFrame.right -relief sunken -bd 2]
    pack $leftFrame $rightFrame -side left -expand yes -fill both

# Sub-divide the left into ltop and lbot
    set ltopFrame [frame $leftFrame.ltop -relief ridge -bd 2]
    set lbotFrame [frame $leftFrame.lbot -relief ridge -bd 2]
    pack $ltopFrame $lbotFrame -expand yes -fill both

# Sub-divide the right into ltop and lbot
    set rtopFrame [frame $rightFrame.rtop -relief ridge -bd 2]
    set rbotFrame [frame $rightFrame.rbot -relief ridge -bd 2]
    pack $rtopFrame $rbotFrame -expand yes -fill both

# Create buttons in leftFrame for actions
    set label [label $ltopFrame.lab -text "CGS3 Task Actions" -fg blue]
    pack $label

    set lb1 [frame $lbotFrame.lb1 -relief flat]
    set lb2 [frame $lbotFrame.lb2 -relief flat]
    set lb3 [frame $lbotFrame.lb3 -relief flat]
    set lb4 [frame $lbotFrame.lb4 -relief flat]
    set lb5 [frame $lbotFrame.lb5 -relief flat]
    set lb6 [frame $lbotFrame.lb6 -relief flat]
    pack $lb1 $lb2 $lb3 $lb4 $lb5 $lb6

    set label [label $lb1.lab -text "Run Number"]
    set Cgs3drWidgets(RNUM) [entry $lb1.entry -relief sunken -bd 2]
    pack $label $Cgs3drWidgets(RNUM) -side left

    set Cgs3drWidgets(REDUCE_RUN) [button $lb2.rr -text "Reduce Run"]
    pack $Cgs3drWidgets(REDUCE_RUN)

    set Cgs3drWidgets(REDUCE_GRP) [button $lb3.rg -text "Reduce Group"]
    pack $Cgs3drWidgets(REDUCE_GRP)

    set Cgs3drWidgets(REDUCE_PHOT) [button $lb4.rp -text "Reduce Phot"]
    pack $Cgs3drWidgets(REDUCE_PHOT)

    set Cgs3drWidgets(SETPAR) [button $lb5.set -text "Set Parameters"]
    pack $Cgs3drWidgets(SETPAR)

    set Cgs3drWidgets(SHOPAR) [button $lb6.sho -text "Show Parameters"]
    pack $Cgs3drWidgets(SHOPAR)

# Create right-frame red3 actions and scrollbar
    set label [label $rtopFrame.lb1 -text "RED3 Task Actions" -fg red]
    pack $label -expand yes -fill both

    set Red3Widgets(LB) [listbox $rbotFrame.listbox -relief flat -bd 2]
    set Red3Widgets(SB) [scrollbar $rbotFrame.scroll -relief sunken -bd 2 -orient vertical]

    $Red3Widgets(LB) configure -yscrollcommand "$Red3Widgets(SB) set"
    $Red3Widgets(SB) configure -command "$Red3Widgets(LB) yview"
    pack $Red3Widgets(LB) -side left -expand yes -fill both
    pack $Red3Widgets(SB) -side right -fill y

# Put some stuff in the Red3Widgets(LB)
    $Red3Widgets(LB) insert end black_body
    $Red3Widgets(LB) insert end cgs3_41
    $Red3Widgets(LB) insert end cgs3_42
    $Red3Widgets(LB) insert end cgs3_43
    $Red3Widgets(LB) insert end cgs3_bad_cycle
    $Red3Widgets(LB) insert end cgs3_det
    $Red3Widgets(LB) insert end extract3
    $Red3Widgets(LB) insert end adjoin3
    $Red3Widgets(LB) insert end scale
    $Red3Widgets(LB) insert end cgs3_phred
    $Red3Widgets(LB) insert end cgs3pol
    return $mainFrame
}
