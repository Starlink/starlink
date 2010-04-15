proc qmanSetUT {} {

# Get some global variables
    global env
    global cgs4drHtml

# Create dialog box.
    if {[winfo exists .qmanDialogue]} {destroy .qmanDialogue}
    set frame [dialogStart .qmanDialogue "Qman UT Date" 0 OK Cancel]
    cgs4drCursor pirate orange black
    .qmanDialogue config -cursor {arrow green black}

# Top frame consists of radiobutton for data-type
    set label [label $frame.label -text "UT Date"]
    set new_date [entry $frame.qdt -relief sunken -bd 2]
    pack $label $new_date -in $frame -side left -padx 2m -pady 2m

    $new_date delete 0 end
    $new_date insert end [string trim $env(QMAN_DATE)]
    set today [exec /usr/bin/date -u +%y%m%d]
    bind $label <Button-2> "$new_date delete 0 end; $new_date insert 0 $today"
    bind $new_date <Button-2> "$new_date delete 0 end; $new_date insert 0 $today"
    bind $new_date <Double-Button-2> "$new_date delete 0 end"
    bind $label <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/qmanSetUTBox1.html"
    bind $new_date <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/qmanSetUTBox1.html"
# Show the dialog box
    set bv [dialogShow .qmanDialogue .qmanDialogue]
    if {$bv==0} {
      cgs4drCursor watch red white
      set date [string trim [$new_date get]]
      if {[string range $date 0 1]=="19" || [string range $date 0 1]=="20"} {
        set env(QMAN_DATE) [string trim [string range $date 2 end]]
      } {
	set env(QMAN_DATE) $date
      }
    }

# Remove the dialog box
    cgs4drCursor arrow green black
    if {[winfo exists .qmanDialogue]} {destroy .qmanDialogue}
}
