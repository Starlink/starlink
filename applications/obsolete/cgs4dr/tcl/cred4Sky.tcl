proc cred4Sky {taskname} {

# Get some global values
    global Cred4NoticeBoard
    global Cred4Widgets
    global cgs4drHtml

# Create dialog box
    if {[winfo exists .cred4Dialogue]} {destroy .cred4Dialogue}
    set frame [dialogStart .cred4Dialogue "Cred4 Sky Parameters Setup" 0 OK Cancel]
    cgs4drCursor pirate orange black
    .cred4Dialogue config -cursor {arrow green black}

# Top-Right contains Normal Sky Subtraction Parameters
    set midtop [frame $frame.mid]
    set bottop [frame $frame.bot]
    pack $midtop $bottop -in $frame

    set l1 [checkbutton $midtop.l1 -text "Add ROs in Pairs" -variable Cred4Widgets(ADD_IN_PAIRS)]
    set l2 [label $midtop.l2 -text " "]
    set fi [radiobutton $midtop.fi -text "Errors from Int" -variable Cred4Widgets(ERRORS) -value "FROM_INT" -width 15]
    set fo [radiobutton $midtop.fo -text "Errors from Obs" -variable Cred4Widgets(ERRORS) -value "FROM_OBS" -width 15]
    pack $l1 $l2 -in $midtop -side left
    pack $fo $fi -in $midtop -side right
    bind $l1 <Button-2> "cred4Update cred4Sky ADD_IN_PAIRS"
    bind $l1 <Button-3> "cred4HelpDialog .helpDialog $cgs4drHtml/cred4SkyBox1.html"
    bind $l2 <Button-2> "cred4Update cred4Sky ALL"
    bind $l2 <Button-3> "cred4HelpDialog .helpDialog $cgs4drHtml/cred4SkyBox1.html"
    bind $fi <Button-2> "cred4Update cred4Sky ERRORS"
    bind $fi <Button-3> "cred4HelpDialog .helpDialog $cgs4drHtml/cred4SkyBox1.html"
    bind $fo <Button-2> "cred4Update cred4Sky ERRORS"
    bind $fo <Button-3> "cred4HelpDialog .helpDialog $cgs4drHtml/cred4SkyBox1.html"
    set Cred4Widgets(ADD_IN_PAIRS) 1
    if {[nbs get ${Cred4NoticeBoard}.miscellaneous.add_in_pairs]==0} {set Cred4Widgets(ADD_IN_PAIRS) 0}
    set Cred4Widgets(ERRORS) "FROM_OBS"

    set l3 [label $bottop.l3 -text "Sky Weighting Factor"]
    set Cred4Widgets(SKYWT) [entry $bottop.sk -width 15]
    set vw [checkbutton $bottop.l2 -text "Variance Weight" -variable Cred4Widgets(VARWT)]
    set l4 [label $bottop.l4 -text " "]
    pack $vw $l4 -in $bottop -side left
    pack $Cred4Widgets(SKYWT) $l3 -in $bottop -side right
    bind $l3 <Button-2> "cred4Update cred4Sky ALL"
    bind $l3 <Button-3> "cred4HelpDialog .helpDialog $cgs4drHtml/cred4SkyBox1.html"
    bind $l4 <Button-2> "cred4Update cred4Sky ALL"
    bind $l4 <Button-3> "cred4HelpDialog .helpDialog $cgs4drHtml/cred4SkyBox1.html"
    bind $vw <Button-2> "cred4Update cred4Sky VARWT"
    bind $vw <Button-3> "cred4HelpDialog .helpDialog $cgs4drHtml/cred4SkyBox1.html"
    bind $Cred4Widgets(SKYWT) <Button-2> "cred4Update cred4Sky SKYWT"
    bind $Cred4Widgets(SKYWT) <Double-Button-2> "$Cred4Widgets(SKYWT) delete 0 end"
    bind $Cred4Widgets(SKYWT) <Button-3> "cred4HelpDialog .helpDialog $cgs4drHtml/cred4SkyBox1.html"
    set Cred4Widgets(VARWT) [nbs get ${Cred4NoticeBoard}.miscellaneous.variance_wt]
    $Cred4Widgets(SKYWT) delete 0 end
    $Cred4Widgets(SKYWT) insert end [string trim [nbs get ${Cred4NoticeBoard}.miscellaneous.sky_wt]]

# Show the dialog box
    set bv [dialogShow .cred4Dialogue .cred4Dialogue]
    if {$bv == 0} {
      cgs4drCursor watch red white

#   Set the sky subtraction parameters
      nbs put ${Cred4NoticeBoard}.miscellaneous.errors $Cred4Widgets(ERRORS)
      nbs put ${Cred4NoticeBoard}.miscellaneous.add_in_pairs $Cred4Widgets(ADD_IN_PAIRS)
      nbs put ${Cred4NoticeBoard}.miscellaneous.variance_wt $Cred4Widgets(VARWT)
      set skywt [string trim [$Cred4Widgets(SKYWT) get]]
      nbs put ${Cred4NoticeBoard}.miscellaneous.sky_wt $skywt
    }

# Remove the dialog box
    cgs4drCursor arrow green black
    destroy .cred4Dialogue
}
