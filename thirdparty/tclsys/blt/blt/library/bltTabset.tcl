set bltTabset(activate) yes

bind Tabset <B2-Motion> {
    %W scan dragto %x %y
}

bind Tabset <ButtonPress-2> {
    set bltTabset(cursor) [%W cget -cursor]
    set bltTabset(activate) no
    %W configure -cursor hand1
    %W scan mark %x %y
}

bind Tabset <ButtonRelease-2> {
    %W configure -cursor $bltTabset(cursor)
    set bltTabset(activate) yes
    %W activate @%x,%y
}

bind Tabset <KeyPress-Up> {
    %W see tabUp
    %W focus tabUp
}

bind Tabset <KeyPress-Down> { 
    %W see tabDown
    %W focus tabDown
}

bind Tabset <KeyPress-Right> {
    %W see tabRight
    %W focus tabRight
}

bind Tabset <KeyPress-Left> {
    %W see tabLeft
    %W focus tabLeft
}

bind Tabset <KeyPress-space> {
    %W invoke tabFocus
}

bind Tabset <KeyPress-Return> {
    %W invoke tabFocus
}

bind Tabset <KeyPress> {
    if { [string match {[A-Za-z0-9]*} "%A"] } {
	blt::FindMatchingTab %W %A
    }
}

proc blt::FindMatchingTab { widget key } {
    set key [string tolower $key]
    set index [$widget index tabFocus]
    set numTabs [$widget size]
    for { set i 0 } { $i < $numTabs } { incr i } {
	if { [incr index] >= $numTabs } {
	    set index 0
	}
	set label [string tolower [$widget tab cget $index -text]]
	if { [string index $label 0] == $key } {
	    break
	}
    }
    $widget focus $index
    $widget see tabFocus
}

proc blt::SelectTab { widget } {
    $widget see tabSelect
    set w [$widget tab tearoff tabSelect]
    if { ($w != "") && ($w != "$widget") } {
	raise [winfo toplevel $w]
    }
    $widget invoke tabSelect
}

proc blt::MendTearoff { widget tab } {
    set top $widget.[$widget index $tab]
    if { [winfo exists $top] } {
	wm withdraw $top
	update
	$widget tab tearoff $tab $widget
	destroy $top
    }
}

proc blt::MakeTearoff { widget x y } {
    set tab [$widget get tabSelect]
    set top $widget.[$widget index $tab]
    toplevel $top
    $widget tab tearoff $tab $top.tearoff
    table $top $top.tearoff -fill both
    incr x 10 ; incr y 10
    wm geometry $top +$x+$y
    wm title $top "[wm title .]: [$widget tab cget $tab -text]"
    wm protocol $top WM_DELETE_WINDOW [list blt::MendTearoff $widget $tab]
    bind $top.tearoff <Destroy> "destroy $top"
}

proc blt::Tearoff { widget x y  } {
    set w [$widget tab tearoff tabSelect]
    if { $w == "$widget" } {
	blt::MakeTearoff $widget $x $y
    } else {
	blt::MendTearoff $widget [$widget get tabSelect]
    }
}


proc blt::InitTabBindings { widget } {
    $widget bind all <Enter> { 
	if { $bltTabset(activate) } {
	    %W activate tabSelect 
        }
    }
    $widget bind all <Leave> { 
        %W activate "" 
    }
    $widget bind all <ButtonRelease-1> { blt::SelectTab %W }
    $widget bind all <ButtonRelease-3> { blt::Tearoff %W %X %Y }
    $widget bind all <Control-ButtonRelease-1> { blt::Tearoff %W %X %Y }
}

