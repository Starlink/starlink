#! /usr/local/bin/tixwish

proc readfile {w} {
    global color

    set file [open /usr/lib/X11/rgb.txt RDONLY]
    while {[eof $file] == 0} {
	set line [gets $file]

	set name [lrange $line 3 end]
	set color($name,r) [lindex $line 0]
	set color($name,g) [lindex $line 1]
	set color($name,b) [lindex $line 2]
	$w insert end $name
    }
    close $file

    bind $w <ButtonRelease-1> "+setcolor $w %y"
}

proc setcolor {w y} {
    global color
    set name [$w get [$w nearest $y]]

    .f config -bg $name
    .g config -bg $name
}

proc start {} {
    frame .f -relief raised -bd 2 -width 100 -height 10
    frame .g -relief sunken -bd 2 -width 100 -height 10
    tixScrolledListBox .b
    readfile [.b subwidget listbox]

    pack .f .g .b -side left -expand yes -fill both -padx 3 -pady 3
    wm minsize . 0 0
}

start
