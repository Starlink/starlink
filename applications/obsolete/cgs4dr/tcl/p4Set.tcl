proc p4Set taskname {
#-
# Displays a dialog box with controls for everything else and sets them upon exit
#-

# Get some default values
    global env
    global P4NoticeBoard
    global P4Widgets
    global cgs4drHtml
    set port $P4Widgets(PORT_NO)
    set root ${P4NoticeBoard}.port_${port}.

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
    $P4Widgets(DEVICE) insert end [string trim [nbs get ${root}device_name]]
    $P4Widgets(TITLE) delete 0 end
    $P4Widgets(TITLE) insert end [string trim [nbs get ${root}title]]

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
    $P4Widgets(XOPT) insert end [string trim [nbs get ${root}device_xopt]]
    $P4Widgets(YOPT) delete 0 end
    $P4Widgets(YOPT) insert end [string trim [nbs get ${root}device_yopt]]
    if {[nbs get ${root}plot_axes]} {
	set P4Widgets(PLOT_AXES) 1
    } {
	set P4Widgets(PLOT_AXES) 0
    }
    if {[nbs get ${root}pre_erase_plot]} {
	set P4Widgets(PRE_ERASE_PLOT) 1
    } {
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
    $P4Widgets(CHAR_HEIGHT) insert end [string trim [nbs get ${root}char_height]]
    set P4Widgets(CSTYLE) [string trim [nbs get ${root}colour_style]]
    $P4Widgets(ISTART) delete 0 end
    $P4Widgets(ISTART) insert 0 [nbs get ${root}istart]
    $P4Widgets(IEND) delete 0 end
    $P4Widgets(IEND) insert 0 [nbs get ${root}iend]
    $P4Widgets(JSTART) delete 0 end
    $P4Widgets(JSTART) insert 0 [nbs get ${root}jstart]
    $P4Widgets(JEND) delete 0 end
    $P4Widgets(JEND) insert 0 [nbs get ${root}jend]

# Show the dialog box
    set bv [dialogShow .p4Dialogue .p4Dialogue]

# Do something in response to OK button
    if {$bv == 0} {
      cgs4drCursor watch red white

      set device [string trim [$P4Widgets(DEVICE) get]]
      nbs put ${root}device_name $device
      set xopt [string trim [$P4Widgets(XOPT) get]]
      nbs put ${root}device_xopt $xopt
      set yopt [string trim [$P4Widgets(YOPT) get]]
      nbs put ${root}device_yopt $yopt
      set display_plane $P4Widgets(DISPLAY_PLANE)
      nbs put ${root}display_plane $display_plane
      set display_type $P4Widgets(DISPLAY_TYPE)
      nbs put ${root}display_type $display_type
      nbs put ${root}colour_style $P4Widgets(CSTYLE)
      set title [string trim [$P4Widgets(TITLE) get]]
      nbs put ${root}title $title

      set autoscale $P4Widgets(AUTOSCALE)
      nbs put ${root}autoscale $autoscale
      if {$autoscale == 0} {
        set high [$P4Widgets(HIGH) get]
        nbs put ${root}high $high
        set low [$P4Widgets(LOW) get]
        nbs put ${root}low $low
      }

      set plot_whole $P4Widgets(PLOT_WHOLE)
      nbs put ${root}plot_whole $plot_whole
      if {$plot_whole == 0} {
        set xstart [$P4Widgets(XSTART) get]
        nbs put ${root}xstart $xstart
        set xend [$P4Widgets(XEND) get]
        nbs put ${root}xend $xend
        set ystart [$P4Widgets(YSTART) get]
        nbs put ${root}ystart $ystart
        set yend [$P4Widgets(YEND) get]
        nbs put ${root}yend $yend
      }

      nbs put ${root}device_lut $P4Widgets(LUT)
      nbs put ${root}display_data $P4Widgets(DATA)
      nbs put ${root}plot_axes $P4Widgets(PLOT_AXES)

      switch [string toupper $display_type] {
	HISTOGRAM {
	  set histogram_xstep [$P4Widgets(HISTOGRAM_XSTEP) get]
	  nbs put ${root}histogram_xstep $histogram_xstep
	  set histogram_ystep [$P4Widgets(HISTOGRAM_YSTEP) get]
	  nbs put ${root}histogram_ystep $histogram_ystep
	  set histogram_bins [$P4Widgets(HISTOGRAM_BINS) get]
	  nbs put ${root}histogram_bins $histogram_bins
	  set hist_smooth [$P4Widgets(HIST_SMOOTH) get]
	  nbs put ${root}hist_smooth $hist_smooth
	}
	CONTOUR {
	  set contour_levels [$P4Widgets(CONTOUR_LEVELS) get]
	  nbs put ${root}contour_levels $contour_levels
	  set contour_type $P4Widgets(CONTOUR_TYPE)
	  nbs put ${root}contour_type $contour_type
	}
	OVERGRAPH {
	  set plot_errors $P4Widgets(PLOT_ERRORS)
	  nbs put ${root}plot_errors $plot_errors
	  set cut_direction $P4Widgets(CUT_DIRECTION)
	  nbs put ${root}cut_direction $cut_direction
	  set slice_start [$P4Widgets(SLICE_START) get]
	  nbs put ${root}slice_start $slice_start
	  set slice_end [$P4Widgets(SLICE_END) get]
	  nbs put ${root}slice_end $slice_end
	  set overcolour $P4Widgets(OVERCOLOUR)
	  nbs put ${root}overcolour $overcolour
	}
	GRAPH {
	  set plot_errors $P4Widgets(PLOT_ERRORS)
	  nbs put ${root}plot_errors $plot_errors
	  set cut_direction $P4Widgets(CUT_DIRECTION)
	  nbs put ${root}cut_direction $cut_direction
	  set slice_start [$P4Widgets(SLICE_START) get]
	  nbs put ${root}slice_start $slice_start
	  set slice_end [$P4Widgets(SLICE_END) get]
	  nbs put ${root}slice_end $slice_end
	}
      }

      nbs put ${root}pre_erase_plot $P4Widgets(PRE_ERASE_PLOT)
      set cheight [string trim [$P4Widgets(CHAR_HEIGHT) get]]
      nbs put ${root}char_height $cheight
      set istart [string trim [$P4Widgets(ISTART) get]]
      nbs put ${root}istart $istart
      set iend [string trim [$P4Widgets(IEND) get]]
      nbs put ${root}iend $iend
      set jstart [string trim [$P4Widgets(JSTART) get]]
      nbs put ${root}jstart $jstart
      set jend [string trim [$P4Widgets(JEND) get]]
      nbs put ${root}jend $jend
    }

# Remove the dialog box
    cgs4drCursor arrow green black
    destroy .p4Dialogue
}
