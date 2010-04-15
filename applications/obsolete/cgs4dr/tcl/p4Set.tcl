proc p4Set {taskname} {
#-
# Displays a dialog box with controls for everything else and sets them upon exit
#-

# Get some default values
    global env
    global P4NoticeBoard
    global P4Widgets
    global cgs4drHtml
    set nbsroot ${P4NoticeBoard}.port_$P4Widgets(PORT_NO)

# Create dialog box.
    if {[winfo exists .p4Dialogue]} {destroy .p4Dialogue}
    set frame [dialogStart .p4Dialogue "Plot4 Set Port Attributes" 0 OK Cancel]
    cgs4drCursor pirate orange black
    .p4Dialogue config -cursor {arrow green black}

# Create panel layout
    set top      [frame $frame.top -relief sunken -bd 2]
    set middle   [frame $frame.middle -relief sunken -bd 2]
    set bottom   [frame $frame.bottom -relief sunken -bd 2]
    pack $top $middle $bottom -side top -fill both -expand yes

# Top frame consists of device name and plot title
    set top2    [frame $top.top]
    set middle2 [frame $top.middle]
    set bottom2 [frame $top.bottom]
    pack $top2 $middle2 $bottom2 -side top -fill both -expand yes

    set title [label $top2.title -text "Device Attributes"]
    set l1 [label $middle2.l1 -text "Device Name"]
    set P4Widgets(DEVICE) [entry $middle2.device -width 75 -relief sunken -bd 2]
    set l2 [label $bottom2.l2 -text "Title of Plot"]
    set P4Widgets(TITLE)  [entry $bottom2.title -width 75 -relief sunken -bd 2]
    pack $title -in $top2
    pack $l1 -in $middle2 -side left -padx 2 -pady 2
    pack $P4Widgets(DEVICE) -in $middle2 -side right -padx 2 -pady 2
    pack $l2 -in $bottom2 -side left -padx 2 -pady 2
    pack $P4Widgets(TITLE) -in $bottom2 -side right -padx 2 -pady 2

    bind $title <Button-2> "p4Update p4Set ALL"
    bind $l1 <Button-2> "p4Update p4Set ALL"
    bind $l2 <Button-2> "p4Update p4Set ALL"
    bind $P4Widgets(DEVICE) <Button-2> {$P4Widgets(DEVICE) delete 0 end; $P4Widgets(DEVICE) insert 0 xwindows\;$env(PID)xwin}
    bind $P4Widgets(DEVICE) <Double-Button-2> "$P4Widgets(DEVICE) delete 0 end"
    bind $P4Widgets(TITLE) <Button-2> "$P4Widgets(TITLE) delete 0 end; $P4Widgets(TITLE) insert 0 A_U_T_O"
    bind $P4Widgets(TITLE) <Double-Button-2> "$P4Widgets(TITLE) delete 0 end"
    bind $title <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/p4SetBox1.html"
    bind $l1 <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/p4SetBox1.html"
    bind $l2 <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/p4SetBox1.html"
    bind $P4Widgets(DEVICE) <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/p4SetBox1.html"
    bind $P4Widgets(TITLE) <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/p4SetBox1.html"

    $P4Widgets(DEVICE) delete 0 end
    $P4Widgets(DEVICE) insert end [string trim [nbs get ${nbsroot}.device_name]]
    $P4Widgets(TITLE) delete 0 end
    $P4Widgets(TITLE) insert end [string trim [nbs get ${nbsroot}.title]]

# Middle frame contains axis options
    set top3    [frame $middle.top]
    set bottom3 [frame $middle.bottom]
    pack $top3 $bottom3 -side top -fill both -expand yes

    set title [label $top3.title -text "Axis Attributes"]
    set l1 [label $bottom3.l1 -text "X-axis Options"]
    set P4Widgets(XOPT) [entry $bottom3.xopt -relief sunken -bd 2]
    set l2 [label $bottom3.l2 -text "Y-axis Options"]
    set P4Widgets(YOPT) [entry $bottom3.yopt -relief sunken -bd 2]
    set pa [checkbutton $bottom3.pa -text "Plot Axes" -variable P4Widgets(PLOT_AXES)]
    set pe [checkbutton $bottom3.pe -text "Pre-Erase Plots" -variable P4Widgets(PRE_ERASE_PLOT)]
    pack $title -in $top3
    pack $l1 $P4Widgets(XOPT) $l2 $P4Widgets(YOPT) -in $bottom3 -side left -padx 2 -pady 2
    pack $pe $pa -in $bottom3 -side right -padx 2 -pady 2

    bind $title <Button-2> "p4Update p4Set ALL"
    bind $l1 <Button-2> "p4Update p4Set ALL"
    bind $l2 <Button-2> "p4Update p4Set ALL"
    bind $pa <Button-2> "set P4Widgets(PLOT_AXES) 1"
    bind $pe <Button-2> "set P4Widgets(PRE_ERASE_PLOT) 1"
    bind $P4Widgets(XOPT) <Button-2> "$P4Widgets(XOPT) delete 0 end; $P4Widgets(XOPT) insert 0 BCNTSI"
    bind $P4Widgets(XOPT) <Double-Button-2> "$P4Widgets(XOPT) delete 0 end"
    bind $P4Widgets(YOPT) <Button-2> "$P4Widgets(YOPT) delete 0 end; $P4Widgets(YOPT) insert 0 BCNTSI"
    bind $P4Widgets(YOPT) <Double-Button-2> "$P4Widgets(YOPT) delete 0 end"
    bind $title <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/p4SetBox1.html"
    bind $l1 <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/p4SetBox1.html"
    bind $l2 <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/p4SetBox1.html"
    bind $P4Widgets(XOPT) <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/p4SetBox1.html"
    bind $P4Widgets(YOPT) <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/p4SetBox1.html"
    bind $pa <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/p4SetBox1.html"
    bind $pe <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/p4SetBox1.html"

    $P4Widgets(XOPT) delete 0 end
    $P4Widgets(XOPT) insert end [string trim [nbs get ${nbsroot}.device_xopt]]
    $P4Widgets(YOPT) delete 0 end
    $P4Widgets(YOPT) insert end [string trim [nbs get ${nbsroot}.device_yopt]]
    if {[nbs get ${nbsroot}.plot_axes]} {
	set P4Widgets(PLOT_AXES) 1
    } else {
	set P4Widgets(PLOT_AXES) 0
    }
    if {[nbs get ${nbsroot}.pre_erase_plot]} {
	set P4Widgets(PRE_ERASE_PLOT) 1
    } else {
	set P4Widgets(PRE_ERASE_PLOT) 0
    }

# Bottom frame contains colour style, character height and sub-array
    set top4    [frame $bottom.top]
    set middle4 [frame $bottom.middle]
    set bottom4 [frame $bottom.bottom]
    pack $top4 $middle4 $bottom4 -side top -fill both -expand yes

    set title [label $top4.title -text "Miscellaneous Attributes"]
    set l1 [label $middle4.l1 -text "Character Height"]
    set l2 [label $bottom4.l2 -text "I start"]
    set l3 [label $bottom4.l3 -text "I end"]
    set l4 [label $bottom4.l4 -text "J start"]
    set l5 [label $bottom4.l5 -text "J end"]
    set P4Widgets(CHAR_HEIGHT) [entry $middle4.ch -width 15 -relief sunken -bd 2 -width 5]
    set fc [radiobutton $middle4.fc -text "Full Colour" -variable P4Widgets(CSTYLE) -value "COLOUR"]
    set mo [radiobutton $middle4.mo -text "Monochrome" -variable P4Widgets(CSTYLE) -value "MONOCHROME"]
    set P4Widgets(ISTART) [entry $bottom4.is -relief sunken -bd 2 -width 5]
    set P4Widgets(IEND) [entry $bottom4.ie -relief sunken -bd 2 -width 5]
    set P4Widgets(JSTART) [entry $bottom4.js -relief sunken -bd 2 -width 5]
    set P4Widgets(JEND) [entry $bottom4.je -relief sunken -bd 2 -width 5]
    pack $title -in $top4
    pack $l1 $P4Widgets(CHAR_HEIGHT) $fc $mo -in $middle4 -side left -padx 2 -pady 2
    pack $mo $fc -in $middle4 -side right -padx 2 -pady 2
    pack $l2 $P4Widgets(ISTART) $l3 $P4Widgets(IEND) $l4 $P4Widgets(JSTART) $l5 $P4Widgets(JEND) -in $bottom4 \
	-side left -padx 2 -pady 2 -expand yes -fill x

    bind $fc <Button-2> "set P4Widgets(CSTYLE) COLOUR"
    bind $mo <Button-2> "set P4Widgets(CSTYLE) COLOUR"
    bind $title <Button-2> "p4Update p4Set ALL"
    bind $l1 <Button-2> "p4Update p4Set ALL"
    bind $l2 <Button-2> "p4Update p4Set ALL"
    bind $l3 <Button-2> "p4Update p4Set ALL"
    bind $l4 <Button-2> "p4Update p4Set ALL"
    bind $l5 <Button-2> "p4Update p4Set ALL"
    bind $title <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/p4SetBox1.html"
    bind $l1 <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/p4SetBox1.html"
    bind $l2 <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/p4SetBox1.html"
    bind $l3 <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/p4SetBox1.html"
    bind $l4 <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/p4SetBox1.html"
    bind $l5 <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/p4SetBox1.html"
    bind $fc <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/p4SetBox1.html"
    bind $mo <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/p4SetBox1.html"
    bind $P4Widgets(CHAR_HEIGHT) <Button-2> "$P4Widgets(CHAR_HEIGHT) delete 0 end; $P4Widgets(CHAR_HEIGHT) insert 0 1.0"
    bind $P4Widgets(CHAR_HEIGHT) <Double-Button-2> "$P4Widgets(CHAR_HEIGHT) delete 0 end"
    bind $P4Widgets(ISTART) <Button-2> "$P4Widgets(ISTART) delete 0 end; $P4Widgets(ISTART) insert 0 -1"
    bind $P4Widgets(ISTART) <Double-Button-2> "$P4Widgets(ISTART) delete 0 end"
    bind $P4Widgets(IEND) <Button-2> "$P4Widgets(IEND) delete 0 end; $P4Widgets(IEND) insert 0 -1"
    bind $P4Widgets(IEND) <Double-Button-2> "$P4Widgets(IEND) delete 0 end"
    bind $P4Widgets(JSTART) <Button-2> "$P4Widgets(JSTART) delete 0 end; $P4Widgets(JSTART) insert 0 -1"
    bind $P4Widgets(JSTART) <Double-Button-2> "$P4Widgets(JSTART) delete 0 end"
    bind $P4Widgets(JEND) <Button-2> "$P4Widgets(JEND) delete 0 end; $P4Widgets(JEND) insert 0 -1"
    bind $P4Widgets(JEND) <Double-Button-2> "$P4Widgets(JEND) delete 0 end"
    bind $P4Widgets(CHAR_HEIGHT) <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/p4SetBox1.html"
    bind $P4Widgets(ISTART) <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/p4SetBox1.html"
    bind $P4Widgets(IEND) <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/p4SetBox1.html"
    bind $P4Widgets(JSTART) <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/p4SetBox1.html"
    bind $P4Widgets(JEND) <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/p4SetBox1.html"

    $P4Widgets(CHAR_HEIGHT) delete 0 end
    $P4Widgets(CHAR_HEIGHT) insert end [string trim [nbs get ${nbsroot}.char_height]]
    set P4Widgets(CSTYLE) [string trim [nbs get ${nbsroot}.colour_style]]
    $P4Widgets(ISTART) delete 0 end
    $P4Widgets(ISTART) insert 0 [nbs get ${nbsroot}.istart]
    $P4Widgets(IEND) delete 0 end
    $P4Widgets(IEND) insert 0 [nbs get ${nbsroot}.iend]
    $P4Widgets(JSTART) delete 0 end
    $P4Widgets(JSTART) insert 0 [nbs get ${nbsroot}.jstart]
    $P4Widgets(JEND) delete 0 end
    $P4Widgets(JEND) insert 0 [nbs get ${nbsroot}.jend]

# Show the dialog box
    set bv [dialogShow .p4Dialogue .p4Dialogue]

# Do something in response to OK button
    if {$bv == 0} {
      cgs4drCursor watch red white
      p4SetNbs $nbsroot
    }

# Remove the dialog box
    cgs4drCursor arrow green black
    destroy .p4Dialogue
}
