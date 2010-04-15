proc buildQmanWidgets {w} {
#+
# This procedure builds the qman interface widget tree. The names of the
# "active" widgets are stored in the global array "QmanWidgets" so that
# the names of the widgets can be changed without effecting the rest of
# the application.
#
# The only widget bindings set up here are the connection between the
# message text window and its scroll bar.
#
# The return value is the name of the frame widget that contains the
# widget tree.
#-
    global QmanWidgets
    global cgs4drHtml

# Create frame for the widget tree.
    set mainFrame [frame $w.main]

# Build panel layout frames.
    set topFrame [frame $mainFrame.ft]
    set bottomFrame [frame $mainFrame.fb]
    pack $topFrame
    pack $bottomFrame -fill both -expand yes -side left

    set leftFrame [frame $topFrame.fl]
    set rightFrame [frame $topFrame.fr -bd 2 -relief sunken]
    pack $leftFrame $rightFrame -side left -expand yes -fill both

    set obsRangeFrame [frame $leftFrame.orf -bd 2 -relief sunken]
    set queuePosFrame [frame $leftFrame.egf -bd 2 -relief sunken]
    pack $obsRangeFrame $queuePosFrame -expand yes -fill both
    bind $obsRangeFrame <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/qmanObsRangeBox1.html"
    bind $queuePosFrame <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/qmanObsRangeBox1.html"

# Create observation layout panel.
    set tf1 [frame $obsRangeFrame.tf1]
    set tf2 [frame $obsRangeFrame.tf2]
    set tf3 [frame $obsRangeFrame.tf3]
    pack $tf1 $tf2 $tf3

    set title [label $tf1.title -text "Observation Range"]
    bind $title <Button-2> "qmanUpdate buildQmanWidgets ALL"
    bind $title <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/qmanObsRangeBox1.html"
    pack $title

    set label1 [label $tf2.l1 -text First]
    set QmanWidgets(OBS_RANGE_FIRST) [entry $tf2.e1]
    $QmanWidgets(OBS_RANGE_FIRST) delete 0 end
    $QmanWidgets(OBS_RANGE_FIRST) insert end 1
    bind $label1 <Button-2> "qmanUpdate buildQmanWidgets ALL"
    bind $label1 <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/qmanObsRangeBox1.html"
    bind $QmanWidgets(OBS_RANGE_FIRST) <Button-2> \
      "$QmanWidgets(OBS_RANGE_FIRST) delete 0 end; $QmanWidgets(OBS_RANGE_FIRST) insert 0 1"
    bind $QmanWidgets(OBS_RANGE_FIRST) <Double-Button-2> "$QmanWidgets(OBS_RANGE_FIRST) delete 0 end"
    bind $QmanWidgets(OBS_RANGE_FIRST) <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/qmanObsRangeBox1.html"

    set label2 [label $tf2.l2 -text Last]
    set QmanWidgets(OBS_RANGE_LAST) [entry $tf2.e2]
    $QmanWidgets(OBS_RANGE_LAST) delete 0 end
    $QmanWidgets(OBS_RANGE_LAST) insert end 1
    bind $label2 <Button-2> "qmanUpdate buildQmanWidgets ALL"
    bind $label2 <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/qmanObsRangeBox1.html"
    bind $QmanWidgets(OBS_RANGE_LAST) <Button-2> \
      "$QmanWidgets(OBS_RANGE_LAST) delete 0 end; $QmanWidgets(OBS_RANGE_LAST) insert 0 1"
    bind $QmanWidgets(OBS_RANGE_LAST) <Double-Button-2> "$QmanWidgets(OBS_RANGE_LAST) delete 0 end"
    bind $QmanWidgets(OBS_RANGE_LAST) <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/qmanObsRangeBox1.html"

    pack $label1 $QmanWidgets(OBS_RANGE_FIRST) -side left
    pack $QmanWidgets(OBS_RANGE_LAST) $label2 -side right

    set QmanWidgets(ENTER_OBSERVATION_RANGE) [button $tf3.b1 -text Enter]
    set QmanWidgets(CANCEL_OBSERVATION_RANGE) [button $tf3.b2 -text Remove]
    pack $QmanWidgets(ENTER_OBSERVATION_RANGE) -side left -padx 5m -pady 2m
    pack $QmanWidgets(CANCEL_OBSERVATION_RANGE) -side right -padx 5m -pady 2m
    bind $QmanWidgets(ENTER_OBSERVATION_RANGE) <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/qmanObsRangeBox1.html"
    bind $QmanWidgets(CANCEL_OBSERVATION_RANGE) <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/qmanObsRangeBox1.html"

# Now do the queue position panel
    set tf1 [frame $queuePosFrame.tf1]
    set tf2 [frame $queuePosFrame.tf2]
    pack $tf1 $tf2

    set title [label $tf1.title -text "Queue Position"]
    bind $title <Button-2> "qmanUpdate buildQmanWidgets ALL"
    bind $title <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/qmanPositionBox1.html"
    pack $title

    set old [radiobutton $tf2.old -text Oldest -value oldest -variable QmanWidgets(QUEUE_POSITION)]
    set new [radiobutton $tf2.new -text Newest -value newest -variable QmanWidgets(QUEUE_POSITION)]
    set QmanWidgets(QUEUE_POSITION) oldest
    bind $old <Button-2> "set QmanWidgets(QUEUE_POSITION) oldest"
    bind $old <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/qmanPositionBox1.html"
    bind $new <Button-2> "set QmanWidgets(QUEUE_POSITION) oldest"
    bind $new <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/qmanPositionBox1.html"
    pack $old -side left
    pack $new -side right

# Create side panel.
    set title [label $rightFrame.title -text "Task Actions"]
    bind $title <Button-2> "qmanUpdate buildQmanWidgets ALL"
    bind $title <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/qman.html"
    set QmanWidgets(LIST_QUEUE) [button $rightFrame.lq -text "List Queue" -width 15]
    bind $QmanWidgets(LIST_QUEUE) <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/qmanListQueueBox1.html"
    set QmanWidgets(CANCEL_ALL_ENTRIES) [button $rightFrame.cae -text "Remove All Entries" -width 15]
    bind $QmanWidgets(CANCEL_ALL_ENTRIES) <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/qmanCancelAllBox1.html"
    set QmanWidgets(INTERRUPT) [button $rightFrame.abo -text "Interrupt" -width 15]
    bind $QmanWidgets(INTERRUPT) <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/qmanInterruptBox1.html"
    pack $title $QmanWidgets(LIST_QUEUE) $QmanWidgets(CANCEL_ALL_ENTRIES) $QmanWidgets(INTERRUPT) -padx 5m -pady 2m

# Create scrolling region for output. The width of the text widget is set to
# zero so that it expands to width of the top panel when it is packed.
    set scrollbar [scrollbar $bottomFrame.scrollbar -orient vertical -relief sunken -bd 2]
    set QmanWidgets(OUTPUT) [text $bottomFrame.text -state disabled -wrap word -relief sunken -bd 2 -width 0]
    $scrollbar configure -command "$QmanWidgets(OUTPUT) yview"
    $QmanWidgets(OUTPUT) configure -yscroll "$scrollbar set"

    pack $scrollbar -side right -fill y
    pack $QmanWidgets(OUTPUT) -side right -fill both -expand yes
    bind $QmanWidgets(OUTPUT) <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/cgs4drTextpane.html"
    bind $scrollbar <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/cgs4drScroll.html"

# Set some defaults
    set QmanWidgets(RB_STRING) DRCOMMENT
    set QmanWidgets(EW_STRING) ""
    set QmanWidgets(DGN) 1
    set QmanWidgets(DEF) 1
    set QmanWidgets(DEL) 1
    set QmanWidgets(DON) 1
    set QmanWidgets(DIF) 1
    set QmanWidgets(DIL) 1

# Return the frame
    return $mainFrame
}
