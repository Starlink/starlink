proc p4Lut {taskname} {
#+
# Creates a dialog box for entering the name of a lookup table.
#-
    global P4NoticeBoard
    global P4Widgets
    global cgs4drHtml

# Set a list of look-up-tables
    set lutlis [list alan1.sdf alan2.sdf alan3.sdf alan4.sdf alan5.sdf alan6.sdf byr.sdf \
                col.sdf col1.sdf col10.sdf col11.sdf col12.sdf col13.sdf col14.sdf col15.sdf \
                col16.sdf col17.sdf col18.sdf col19.sdf col19_grey_heat.sdf col19x2.sdf col19x3.sdf \
                col2.sdf col20.sdf col21.sdf col22.sdf col23.sdf col24.sdf col25.sdf col26.sdf \
                col27.sdf col28.sdf col29.sdf col3.sdf col30.sdf col31.sdf col32.sdf col33.sdf  \
                col34.sdf col35.sdf col36.sdf col37.sdf col38.sdf col39.sdf col4.sdf col41.sdf \
                col42.sdf col43.sdf col44.sdf col45.sdf col5.sdf col7.sdf col7x2.sdf col7x3.sdf col8.sdf \
                col9.sdf colin.sdf default.sdf grey.sdf heat.sdf hot.sdf lyndsey.sdf spectrum.sdf \
                suze.sdf zebra.sdf zebra1.sdf]

# Create dialog box
    if {[winfo exists .p4Dialogue]} {destroy .p4Dialogue}
    set frame [dialogStart .p4Dialogue "Plot4 Look-Up-Table" 0 "All Ports" "This Port" Cancel]
    cgs4drCursor pirate orange black
    .p4Dialogue config -cursor {arrow green black}

# Create and pack dialog box widgets
    set P4Widgets(LB) [listbox $frame.lb -relief raised -bd 2]
    set P4Widgets(SB) [scrollbar $frame.sb -relief sunken -bd 2 -orient vertical]
    $P4Widgets(LB) configure -yscrollcommand "$P4Widgets(SB) set"
    $P4Widgets(SB) configure -command "$P4Widgets(LB) yview"
    pack $P4Widgets(LB) -in $frame -side left -expand yes -fill x
    pack $P4Widgets(SB) -in $frame -side right -fill y

# Load the entry widget with the current lut name
    set P4Widgets(LUT) [string trim [file tail [nbs get ${P4NoticeBoard}.port_$P4Widgets(PORT_NO).device_lut]]]
    set old_lut $P4Widgets(LUT)

# Load the list box with values
    set lnum 0
    while {$lnum < [llength $lutlis]} {
      set coltab [string trim [lindex $lutlis $lnum]]
      $P4Widgets(LB) insert end $coltab
      if {$P4Widgets(LUT) == [file rootname $coltab]} {$P4Widgets(LB) selection set $lnum}
      incr lnum
    }

# Do the binding
   bind $P4Widgets(LB) <Double-Button-1> "p4SetLut $taskname \[selection get\] "
   bind $P4Widgets(LB) <Button-2> "cgs4drInform $taskname \"There is no default for this listbox!\""
   bind $P4Widgets(LB) <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/p4LutBox1.html"
   bind $P4Widgets(SB) <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/cgs4drScroll.html"

# Show the dialog box
   set bv [dialogShow .p4Dialogue .p4Dialogue]
   cgs4drCursor watch red white

# Set all ports
   if {$bv == 0} {
     set coltab [string trim [selection get]]
     cgs4drInform $taskname "Resetting all port's look-up-tables to selection ($coltab)"
     nbs put ${P4NoticeBoard}.port_0.device_lut [file rootname $coltab]
     nbs put ${P4NoticeBoard}.port_1.device_lut [file rootname $coltab]
     nbs put ${P4NoticeBoard}.port_2.device_lut [file rootname $coltab]
     nbs put ${P4NoticeBoard}.port_3.device_lut [file rootname $coltab]
     nbs put ${P4NoticeBoard}.port_4.device_lut [file rootname $coltab]
     nbs put ${P4NoticeBoard}.port_5.device_lut [file rootname $coltab]
     nbs put ${P4NoticeBoard}.port_6.device_lut [file rootname $coltab]
     nbs put ${P4NoticeBoard}.port_7.device_lut [file rootname $coltab]
     nbs put ${P4NoticeBoard}.port_8.device_lut [file rootname $coltab]

# This port only (already done)
   } elseif {$bv == 1} {

# If cancel, reset the original colour table
   } elseif {$bv == 2} {
     cgs4drInform $taskname "Resetting look-up-table to original ($old_lut)"
     nbs put ${P4NoticeBoard}.port_$P4Widgets(PORT_NO).device_lut [file rootname $old_lut]
     $taskname obey lut "port=$P4Widgets(PORT_NO)" -inform "cgs4drInform $taskname %V"
   }

# Remove the dialog box
   cgs4drCursor arrow green black
   destroy .p4Dialogue
}

proc p4SetLut {taskname coltab} {
  global P4Widgets
  global P4NoticeBoard
  nbs put ${P4NoticeBoard}.port_$P4Widgets(PORT_NO).device_lut [file rootname $coltab]
  cgs4drInform $taskname "Setting colour table to $coltab in port $P4Widgets(PORT_NO)"
  $taskname obey lut "port=$P4Widgets(PORT_NO)" -inform "cgs4drInform $taskname %V"
}
