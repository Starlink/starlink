proc p4Colours taskname {
#-
# Displays a dialog box with controls for setting the foreground, background
# and overlay colours.
#-
    global P4Widgets
    global P4NoticeBoard
    global cgs4drBitmaps
    global cgs4drHtml

# Create dialog box.
    if {[winfo exists .p4Dialogue]} {destroy .p4Dialogue}
    set frame [dialogStart .p4Dialogue "Plot4 Display Colours" 0 "All Ports" "This Port" Cancel]
    cgs4drCursor pirate orange black
    .p4Dialogue config -cursor {arrow green black}

# Create and pack dialog box widgets.
    set ff [frame $frame.ff]
    set bf [frame $frame.bf]
    set of [frame $frame.of]
    pack $ff $bf $of -fill x

    set label [label $ff.label -text "Foreground Colour"]
    set bl [radiobutton $ff.bl -bitmap @$cgs4drBitmaps/patch.xbm -variable P4Widgets(FGCOLOUR) \
	-foreground black -activeforeground black -value BLACK]
    set wh [radiobutton $ff.wh -bitmap @$cgs4drBitmaps/patch.xbm -variable P4Widgets(FGCOLOUR) \
	-foreground white -activeforeground white -value WHITE]
    set r [radiobutton $ff.r -bitmap @$cgs4drBitmaps/patch.xbm -variable P4Widgets(FGCOLOUR) \
	-foreground red -activeforeground red -value RED]
    set o [radiobutton $ff.o -bitmap @$cgs4drBitmaps/patch.xbm -variable P4Widgets(FGCOLOUR) \
	-foreground orange -activeforeground orange -value ORANGE]
    set y [radiobutton $ff.y -bitmap @$cgs4drBitmaps/patch.xbm -variable P4Widgets(FGCOLOUR) \
	-foreground yellow -activeforeground yellow -value YELLOW]
    set g [radiobutton $ff.g -bitmap @$cgs4drBitmaps/patch.xbm -variable P4Widgets(FGCOLOUR) \
	-foreground green -activeforeground green -value GREEN]
    set b [radiobutton $ff.b -bitmap @$cgs4drBitmaps/patch.xbm -variable P4Widgets(FGCOLOUR) \
	-foreground blue -activeforeground blue -value BLUE]
    pack $label -side left
    pack $b $g $y $o $r $wh $bl -side right
    bind $label <Button-2> "set P4Widgets(FGCOLOUR) BLACK; set P4Widgets(BGCOLOUR) WHITE; set P4Widgets(OVERCOLOUR) RED"
    bind $bl <Button-2> "set P4Widgets(FGCOLOUR) BLACK"
    bind $wh <Button-2> "set P4Widgets(FGCOLOUR) BLACK"
    bind $r <Button-2> "set P4Widgets(FGCOLOUR) BLACK"
    bind $o <Button-2> "set P4Widgets(FGCOLOUR) BLACK"
    bind $y <Button-2> "set P4Widgets(FGCOLOUR) BLACK"
    bind $g <Button-2> "set P4Widgets(FGCOLOUR) BLACK"
    bind $b <Button-2> "set P4Widgets(FGCOLOUR) BLACK"
    bind $label <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/p4ColoursBox1.html"
    bind $bl <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/p4ColoursBox1.html"
    bind $wh <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/p4ColoursBox1.html"
    bind $r <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/p4ColoursBox1.html"
    bind $o <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/p4ColoursBox1.html"
    bind $y <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/p4ColoursBox1.html"
    bind $g <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/p4ColoursBox1.html"
    bind $b <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/p4ColoursBox1.html"

    set label [label $bf.label -text "Background Colour"]
    set bl [radiobutton $bf.bl -bitmap @$cgs4drBitmaps/patch.xbm -variable P4Widgets(BGCOLOUR) \
	-foreground black -activeforeground black -value BLACK]
    set wh [radiobutton $bf.wh -bitmap @$cgs4drBitmaps/patch.xbm -variable P4Widgets(BGCOLOUR) \
	-foreground white -activeforeground white -value WHITE]
    set r [radiobutton $bf.r -bitmap @$cgs4drBitmaps/patch.xbm -variable P4Widgets(BGCOLOUR) \
	-foreground red -activeforeground red -value RED]
    set o [radiobutton $bf.o -bitmap @$cgs4drBitmaps/patch.xbm -variable P4Widgets(BGCOLOUR) \
	-foreground orange -activeforeground orange -value ORANGE]
    set y [radiobutton $bf.y -bitmap @$cgs4drBitmaps/patch.xbm -variable P4Widgets(BGCOLOUR) \
	-foreground yellow -activeforeground yellow -value YELLOW]
    set g [radiobutton $bf.g -bitmap @$cgs4drBitmaps/patch.xbm -variable P4Widgets(BGCOLOUR) \
	-foreground green -activeforeground green -value GREEN]
    set b [radiobutton $bf.b -bitmap @$cgs4drBitmaps/patch.xbm -variable P4Widgets(BGCOLOUR) \
	-foreground blue -activeforeground blue -value BLUE]
    pack $label -side left
    pack $b $g $y $o $r $wh $bl -side right
    bind $label <Button-2> "set P4Widgets(FGCOLOUR) BLACK; set P4Widgets(BGCOLOUR) WHITE; set P4Widgets(OVERCOLOUR) RED"
    bind $bl <Button-2> "set P4Widgets(BGCOLOUR) WHITE"
    bind $wh <Button-2> "set P4Widgets(BGCOLOUR) WHITE"
    bind $r <Button-2> "set P4Widgets(BGCOLOUR) WHITE"
    bind $o <Button-2> "set P4Widgets(BGCOLOUR) WHITE"
    bind $y <Button-2> "set P4Widgets(BGCOLOUR) WHITE"
    bind $g <Button-2> "set P4Widgets(BGCOLOUR) WHITE"
    bind $b <Button-2> "set P4Widgets(BGCOLOUR) WHITE"
    bind $label <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/p4ColoursBox1.html"
    bind $bl <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/p4ColoursBox1.html"
    bind $wh <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/p4ColoursBox1.html"
    bind $r <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/p4ColoursBox1.html"
    bind $o <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/p4ColoursBox1.html"
    bind $y <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/p4ColoursBox1.html"
    bind $g <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/p4ColoursBox1.html"
    bind $b <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/p4ColoursBox1.html"

    set label [label $of.label -text "Overgraph Colour"]
    set bl [radiobutton $of.bl -bitmap @$cgs4drBitmaps/patch.xbm -variable P4Widgets(OVERCOLOUR) \
	-foreground black -activeforeground black -value BLACK]
    set wh [radiobutton $of.wh -bitmap @$cgs4drBitmaps/patch.xbm -variable P4Widgets(OVERCOLOUR) \
	-foreground white -activeforeground white -value WHITE]
    set r [radiobutton $of.r -bitmap @$cgs4drBitmaps/patch.xbm -variable P4Widgets(OVERCOLOUR) \
	-foreground red -activeforeground red -value RED]
    set o [radiobutton $of.o -bitmap @$cgs4drBitmaps/patch.xbm -variable P4Widgets(OVERCOLOUR) \
	-foreground orange -activeforeground orange -value ORANGE]
    set y [radiobutton $of.y -bitmap @$cgs4drBitmaps/patch.xbm -variable P4Widgets(OVERCOLOUR) \
	-foreground yellow -activeforeground yellow -value YELLOW]
    set g [radiobutton $of.g -bitmap @$cgs4drBitmaps/patch.xbm -variable P4Widgets(OVERCOLOUR) \
	-foreground green -activeforeground green -value GREEN]
    set b [radiobutton $of.b -bitmap @$cgs4drBitmaps/patch.xbm -variable P4Widgets(OVERCOLOUR) \
	-foreground blue -activeforeground blue -value BLUE]
    pack $label -side left
    pack $b $g $y $o $r $wh $bl -side right
    bind $label <Button-2> "set P4Widgets(FGCOLOUR) BLACK; set P4Widgets(BGCOLOUR) WHITE; set P4Widgets(OVERCOLOUR) RED"
    bind $bl <Button-2> "set P4Widgets(OVERCOLOUR) RED"
    bind $wh <Button-2> "set P4Widgets(OVERCOLOUR) RED"
    bind $r <Button-2> "set P4Widgets(OVERCOLOUR) RED"
    bind $o <Button-2> "set P4Widgets(OVERCOLOUR) RED"
    bind $y <Button-2> "set P4Widgets(OVERCOLOUR) RED"
    bind $g <Button-2> "set P4Widgets(OVERCOLOUR) RED"
    bind $b <Button-2> "set P4Widgets(OVERCOLOUR) RED"
    bind $label <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/p4ColoursBox1.html"
    bind $bl <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/p4ColoursBox1.html"
    bind $wh <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/p4ColoursBox1.html"
    bind $r <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/p4ColoursBox1.html"
    bind $o <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/p4ColoursBox1.html"
    bind $y <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/p4ColoursBox1.html"
    bind $g <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/p4ColoursBox1.html"
    bind $b <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/p4ColoursBox1.html"

# Set radio buttons to the current colour.
    set port $P4Widgets(PORT_NO)
    set root ${P4NoticeBoard}.port_${port}.
    set P4Widgets(FGCOLOUR) [string trim [nbs get ${root}fg_colour]]
    set P4Widgets(BGCOLOUR) [string trim [nbs get ${root}bg_colour]]
    set P4Widgets(OVERCOLOUR) [string trim [nbs get ${root}overcolour]]

# Show the dialog box
    set bv [dialogShow .p4Dialogue .p4Dialogue]
    cgs4drCursor watch red white

# Set all ports
    if {$bv == 0} {
      nbs put ${P4NoticeBoard}.port_0.fg_colour  $P4Widgets(FGCOLOUR)
      nbs put ${P4NoticeBoard}.port_1.fg_colour  $P4Widgets(FGCOLOUR)
      nbs put ${P4NoticeBoard}.port_2.fg_colour  $P4Widgets(FGCOLOUR)
      nbs put ${P4NoticeBoard}.port_3.fg_colour  $P4Widgets(FGCOLOUR)
      nbs put ${P4NoticeBoard}.port_4.fg_colour  $P4Widgets(FGCOLOUR)
      nbs put ${P4NoticeBoard}.port_5.fg_colour  $P4Widgets(FGCOLOUR)
      nbs put ${P4NoticeBoard}.port_6.fg_colour  $P4Widgets(FGCOLOUR)
      nbs put ${P4NoticeBoard}.port_7.fg_colour  $P4Widgets(FGCOLOUR)
      nbs put ${P4NoticeBoard}.port_8.fg_colour  $P4Widgets(FGCOLOUR)
      nbs put ${P4NoticeBoard}.port_0.bg_colour  $P4Widgets(BGCOLOUR)
      nbs put ${P4NoticeBoard}.port_1.bg_colour  $P4Widgets(BGCOLOUR)
      nbs put ${P4NoticeBoard}.port_2.bg_colour  $P4Widgets(BGCOLOUR)
      nbs put ${P4NoticeBoard}.port_3.bg_colour  $P4Widgets(BGCOLOUR)
      nbs put ${P4NoticeBoard}.port_4.bg_colour  $P4Widgets(BGCOLOUR)
      nbs put ${P4NoticeBoard}.port_5.bg_colour  $P4Widgets(BGCOLOUR)
      nbs put ${P4NoticeBoard}.port_6.bg_colour  $P4Widgets(BGCOLOUR)
      nbs put ${P4NoticeBoard}.port_7.bg_colour  $P4Widgets(BGCOLOUR)
      nbs put ${P4NoticeBoard}.port_8.bg_colour  $P4Widgets(BGCOLOUR)
      nbs put ${P4NoticeBoard}.port_0.overcolour $P4Widgets(OVERCOLOUR)
      nbs put ${P4NoticeBoard}.port_1.overcolour $P4Widgets(OVERCOLOUR)
      nbs put ${P4NoticeBoard}.port_2.overcolour $P4Widgets(OVERCOLOUR)
      nbs put ${P4NoticeBoard}.port_3.overcolour $P4Widgets(OVERCOLOUR)
      nbs put ${P4NoticeBoard}.port_4.overcolour $P4Widgets(OVERCOLOUR)
      nbs put ${P4NoticeBoard}.port_5.overcolour $P4Widgets(OVERCOLOUR)
      nbs put ${P4NoticeBoard}.port_6.overcolour $P4Widgets(OVERCOLOUR)
      nbs put ${P4NoticeBoard}.port_7.overcolour $P4Widgets(OVERCOLOUR)
      nbs put ${P4NoticeBoard}.port_8.overcolour $P4Widgets(OVERCOLOUR)

# If this port only
    } elseif {$bv == 1} {
      nbs put ${root}fg_colour $P4Widgets(FGCOLOUR)
      nbs put ${root}bg_colour $P4Widgets(BGCOLOUR)
      nbs put ${root}overcolour $P4Widgets(OVERCOLOUR)
    }

# Remove the dialog box.
    cgs4drCursor arrow green black
    destroy .p4Dialogue
}
