wm geometry . +200+100

if [tixMwm ismwmrunning .] {
    tixMwm decoration . -border 0 -menu 0 -minimize 0 -resizeh 0 -title 0 \
	-maximize 0
}

set tk_strictMotif 1
frame .f -bd 3 -bg lightgray -relief raised
button .b -bd 1 -highlightthickness 0 -bg gray80 -fg #a04040 -relief sunken \
    -font -*-helvetica-bold-r-*-*-18-*-*-*-*-*-*-* -anchor s

.b config -text "Advantages of Tix"
. config -bg gray40
pack .f -padx 1 -pady 1
pack .b -in .f -padx 0 -pady 0

