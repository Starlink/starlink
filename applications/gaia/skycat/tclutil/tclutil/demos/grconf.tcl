#!../blt_wish -f
#
# BLT Graph Configure Tool
# 
# Note: this file was taken from the BLT-1.7 distribution and modified
# slightly (Allan Brighton)
# "@(#) $Id: grconf.tcl,v 1.2 2005/02/02 01:43:02 brighton Exp $"

if [file exists ../library] {
    set blt_library ../library
}
set graph {}
set interp {}
set class {}
set table(dummy) {}
set setProc(dummy) {}
set getProc(dummy) {}
set row 0
set column 0

wm title . "Graph Configure Tool"

# get X defaults (allan)
# the environment variable $RTD_LIBRARY should point to the directory
# containing the Tcl library files
if {[info exists env(RTD_LIBRARY)]} {
    set rtd_library $env(RTD_LIBRARY)
} else {
    set rtd_library ../library
}
option readfile $rtd_library/../demos/Xdefaults
option add *highlightThickness 0
option add *scale.relief sunken
frame .tmp
. config -bg [.tmp cget -bg]

option add *Radiobutton.font *New*Century*Schoolbook-Medium*-R-*-12-*
option add *Radiobutton.relief flat
#option add *Entry.font *Courier-Medium-R-*-12-*
option add *Label.font *Helvetica-Bold-R-*-12-*
option add *title.font *Helvetica-Bold-R-*-14-*
option add *Entry.font *Courier-Medium-R-*-12-*
option add *Scale.length 120

#
#  Controls for various options
# 

proc GetOptions { options arrayVar defaultsVar } {
    upvar $arrayVar array
    upvar $defaultsVar defaults

    foreach item $options {
	if {[llength $item] < 4} {
	    continue
	}
	set name [lindex $item 0] 
	set value [lindex $item 4]
	set array($name) $value
    }
    set names [lsort [array names array]]
    set defaults {}
    foreach i $names {
	lappend defaults $i $array($i)
    }
}

proc AnchorControl { name option value } {
    global row column table setProc 

    label $table($name).label$option -text $option 
    frame $table($name).frame$option 
    set frame $table($name).frame$option 
    set variable $name$option

    set count 0
    foreach i { nw n ne w center e sw s se } {
	radiobutton $frame.rbutton_$i -variable $variable -value $i -text $i \
	    -command "$setProc($name) $option \$\{$variable\}"
	blt_table $frame $frame.rbutton_$i $count/3,$count%3 -anchor w
	incr count
    }
    blt_table $table($name) \
	$table($name).label$option $row,$column -anchor w \
	$table($name).frame$option $row,$column+1 -cspan 2 -fill x

    global $variable
    set $variable $value
    incr row 
}

proc AnchorReset { name option value } {
    global setProc 

    set variable $name$option
    global $variable
    set $variable $value
    $setProc($name) $option $value
}

proc ReliefControl { name option value } {
    global row column table setProc

    label $table($name).label$option -text $option 
    frame $table($name).frame$option 
    set frame $table($name).frame$option 
    set variable $name$option

    set count 0
    foreach i { sunken groove flat ridge raised } {
	radiobutton $frame.rbutton_$i -variable $variable -value $i -text $i \
	    -command "$setProc($name) $option \$\{$variable\}"
	blt_table $frame $frame.rbutton_$i $count/2,$count%2 -anchor w
	incr count
    }
    blt_table $table($name) \
	$table($name).label$option $row,$column -anchor w \
	$table($name).frame$option $row,$column+1 -cspan 2 -fill x
    
    global $variable
    set $variable $value
    incr row 
}

proc ReliefReset { name option value } {
    global setProc
    set variable $name$option
    global $variable
    set $variable $value
    $setProc($name) $option $value
}


proc SymbolControl { name option value } {
    global row column table setProc

    label $table($name).label$option -text $option 
    frame $table($name).frame$option 
    set frame $table($name).frame$option 
    set variable $name$option

    set count 0
    foreach i { Line Circle Cross Diamond Plus Square Scross Splus } {
	radiobutton $frame.rbutton_$i -variable $variable -value $i -text $i \
	    -command "$setProc($name) $option \$\{$variable\}"
	blt_table $frame $frame.rbutton_$i $count/3,$count%3 -anchor w
	incr count
    }
    blt_table $table($name) \
	$table($name).label$option $row,$column -anchor w \
	$table($name).frame$option $row,$column+1 -cspan 2 -fill x
    
    global $variable
    set $variable $value
    incr row 
}

proc SymbolReset { name option value } {
    global setProc
    set variable $name$option
    global $variable
    set $variable $value
    $setProc($name) $option $value
}

proc BooleanControl { name option value } {
    global row column table setProc

    label $table($name).label$option -text $option 
    frame $table($name).frame$option 
    set frame $table($name).frame$option 
    set variable $name$option

    radiobutton $frame.rbutton_true -text "true" -variable $variable \
	-command "$setProc($name) $option \$\{$variable\}" \
	-value 1 
    radiobutton $frame.rbutton_false  -text "false" -variable $variable \
	-command "$setProc($name) $option \$\{$variable\}" \
	-value 0
    blt_table $frame \
	$frame.rbutton_true  0,0 -anchor w \
	$frame.rbutton_false 0,1 -anchor w

    blt_table $table($name) \
	$table($name).label$option $row,$column -anchor w \
	$table($name).frame$option $row,$column+1 -cspan 2 -fill x

    global $variable
    set $variable $value
    incr row 
}

proc BooleanReset { name option value } {
    global setProc
    set variable $name$option
    global $variable
    set $variable $value
    $setProc($name) $option $value
}

proc SetInt { name option value } {
    global table setProc

    $setProc($name) $option $value
    $table($name).frame$option.scale set $value
    $table($name).frame$option.entry delete 0 end
    $table($name).frame$option.entry insert 0 $value
}

proc IntControl { name option from to value } {
    global row column table 

    label $table($name).label$option -text $option 
    frame $table($name).frame$option
    set frame $table($name).frame$option
    entry $frame.entry -relief sunken -width 6
    scale $frame.scale -from $from -to $to \
	-orient horizontal  -bg bisque1 -showvalue 0 
    blt_table $frame \
	$frame.entry 0,0 -fill x \
	$frame.scale 0,1  -fill x
    $frame.scale set $value
    $frame.scale configure -command "SetInt $name $option "
    $frame.entry insert 0 $value
    bind $frame.entry <Return> "SetInt $name $option \[%W get\]"
    blt_table $table($name) \
	$table($name).label$option $row,$column -anchor w \
	$table($name).frame$option $row,$column+1 -cspan 2 -fill x 
    incr row 
}

proc IntReset { name option value } {
    global setProc

    SetInt $name $option $value
    $setProc($name) $option $value
}

proc SetFloatFromEntry { name option scale value } {
    global table setProc

    $setProc($name) $option $value
    $table($name).frame$option.scale set [expr round($value/$scale)]
    $table($name).frame$option.entry delete 0 end
    $table($name).frame$option.entry insert 0 $value
}

proc SetFloatFromScale { name option scale value } {
    global table setProc

    set x [expr $scale*$value]
    $setProc($name) $option $x 
    $table($name).frame$option.scale set $value
    $table($name).frame$option.entry delete 0 end
    $table($name).frame$option.entry insert 0 $x
}

proc FloatControl { name option from to scale value } {
    global row column table setProc

    label $table($name).label$option -text $option 
    frame $table($name).frame$option
    set frame $table($name).frame$option
    entry $frame.entry -relief sunken -width 6
    scale $frame.scale -from $from -to $to \
	-orient horizontal  -bg bisque1 -showvalue 0  \
	-command "SetFloatFromScale $name $option $scale"
    blt_table $frame \
	$frame.entry 0,0 -fill x \
	$frame.scale 0,1  -fill x
    $frame.scale set [expr round($value/$scale)]
    $frame.entry insert 0 $value
    bind $frame.entry <Return> \
	"SetFloatFromEntry $name $option $scale \[%W get\]"
    blt_table $table($name) \
	$table($name).label$option $row,$column -anchor w \
	$table($name).frame$option $row,$column+1 -cspan 2 -fill x 
    incr row 
}

proc FloatReset { name option scale value } {
    global setProc
    
    SetFloatFromEntry $name $option $scale $value 
    $setProc($name) $option $value
}


proc RGBInit {} {
    global RGB

    set RGB(dummy) 0
    if ![catch { exec showrgb } colorList] {
	set colorList [split $colorList \n]
	foreach i $colorList {
	    set parts [split $i \t]
	    set name [lindex $parts 2]
	    set RGB($name) [lindex $parts 0]
	}
    }
}

proc RGBSetColorFromScale { name option color intensity } {
    global rgb-$name$option setProc table

    set rgb-$name${option}($color) $intensity
    set color [format #%02x%02x%02x [set rgb-$name${option}(red)] \
	       [set rgb-$name${option}(green)] [set rgb-$name${option}(blue)]]
    $table($name).frame$option.entry delete 0 end
    $table($name).frame$option.entry insert 0 $color
    $setProc($name) $option $color
}

proc RGBInitControls { name option color } {
    global rgb-$name$option table RGB

    set frame $table($name).frame$option
    $frame.entry delete 0 end
    $frame.entry insert 0 $color
    if { [string match #* $color] } {
	set rgb-${name}${option}(red) 0x[string range $color 1 2]
	set rgb-${name}${option}(green) 0x[string range $color 3 4]
	set rgb-${name}${option}(blue) 0x[string range $color 5 6]
    } elseif { [info exists RGB($color)] } {
	set info $RGB($color)
	set rgb-${name}${option}(red) [lindex $info 0]
	set rgb-${name}${option}(green) [lindex $info 1]
	set rgb-${name}${option}(blue) [lindex $info 2]
    } else {
	set rgb-${name}${option}(red) 0
	set rgb-${name}${option}(green) 0
	set rgb-${name}${option}(blue) 0
	return
    }
    foreach i { red green blue } {
	$frame.scale_$i set [expr int([set rgb-${name}${option}($i)])]
    }
}

proc RGBSetColorFromEntry { name option color } {
    global setProc table 

    RGBInitControls $name $option $color
    $setProc($name) $option $color
}

proc RGBColorControl { name option value } {
    global row column table 
    
    label $table($name).label$option -text $option 
    frame $table($name).frame$option 
    set frame $table($name).frame$option 
    
    entry $frame.entry -relief sunken -width 8
    scale $frame.scale_red -from 0 -to 255 -orient horizontal -showvalue 0 \
	-background #ff8888 -bd 0 -bg bisque1 -activebackground red \
	-command "RGBSetColorFromScale $name $option red"
    scale $frame.scale_green -from 0 -to 255 -orient horizontal -showvalue 0 \
	-background #43cd80 -bd 0 -bg bisque1 -activebackground green \
	-command "RGBSetColorFromScale $name $option green"
    scale $frame.scale_blue -from 0 -to 255 -orient horizontal -showvalue 0 \
	-background #7ec0ee -bd 0 -bg bisque1 -activebackground blue \
	-command "RGBSetColorFromScale $name $option blue"
    blt_table $frame \
	$frame.entry 0,0 -rspan 3 \
	$frame.scale_red 0,1 -fill both  \
	$frame.scale_green 1,1 -fill both \
	$frame.scale_blue 2,1 -fill both 

    bind $frame.entry <Return> \
	"RGBSetColorFromEntry $name $option \[%W get\]"
    RGBInitControls $name $option $value
    blt_table $table($name) \
	$table($name).label$option $row,$column -anchor w \
	$table($name).frame$option $row,$column+1 -cspan 2 -fill x
    incr row 
}

proc RGBColorReset { name option value } {
    RGBSetColorFromEntry $name $option $value
}

proc EntryControl { name option value } {
    global row column table setProc

    label $table($name).label$option -text $option 
    entry $table($name).entry$option -relief sunken
    bind $table($name).entry$option <Return> \
	"$setProc($name) $option \[%W get\]"
    $table($name).entry$option insert 0 $value

    blt_table $table($name) \
	$table($name).label$option $row,$column -anchor w \
	$table($name).entry$option $row,$column+1 -cspan 2 -fill x
    incr row 
}

proc EntryReset { name option value } {
    global table setProc

    $table($name).entry$option delete 0 end
    $table($name).entry$option insert 0 $value
    $setProc($name) $option $value
}

proc FontControl { name option value } {
    global row column table setProc

    label $table($name).label$option -text $option 
    frame $table($name).frame$option 
    set frame $table($name).frame$option 

    entry $frame.entry -relief sunken
    frame $frame.palette -geom 15x15 -relief raised -bd 2
    blt_table $frame \
	$frame.entry 0,0 -fill x \
	$frame.palette 0,1 -padx 2  -reqwidth 15 -fill y

    blt_table column $frame configure 1 -resize none
    $frame.entry insert 0 $value
    bind $frame.entry <Return> "$setProc($name) $option \[%W get\]"

    blt_table $table($name) \
	$table($name).label$option $row,$column -anchor w \
	$table($name).frame$option $row,$column+1 -cspan 2 -fill x
    incr row 
}


proc FontReset { name option value } {
    global table setProc

    $table($name).frame$option.entry delete 0 end
    $table($name).frame$option.entry insert 0 $value
    $setProc($name) $option $value
}


proc TraceReset { name option value } {
    global setProc
    set variable $name$option
    global $variable
    set $variable $value
    $setProc($name) $option $value
}

proc TraceControl { name option value } {
    global row column table setProc

    label $table($name).label$option -text $option 
    frame $table($name).frame$option 
    set frame $table($name).frame$option 
    set variable $name$option

    radiobutton $frame.rbutton_dec -text "decreasing" -variable $variable \
	-command "$setProc($name) $option \$\{$variable\}" \
	-value "decreasing"
    radiobutton $frame.rbutton_both -text "both" -variable $variable \
	-command "$setProc($name) $option \$\{$variable\}" \
	-value "both"
    radiobutton $frame.rbutton_inc -text "increasing" -variable $variable \
	-command "$setProc($name) $option \$\{$variable\}" \
	-value "increasing" 
    blt_table $frame \
	$frame.rbutton_both 0,0 -anchor w \
	$frame.rbutton_dec 0,1 -anchor w \
	$frame.rbutton_inc 0,2 -anchor w 

    blt_table $table($name) \
	$table($name).label$option $row,$column -anchor w \
	$table($name).frame$option $row,$column+1 -cspan 2 -fill x

    global $variable
    set $variable $value
    incr row 
}


proc MapXAxisControl { name option value } {
    global row column table setProc

    label $table($name).label$option -text $option 
    frame $table($name).frame$option 
    set frame $table($name).frame$option 
    set variable $name$option

    radiobutton $frame.rbutton_x1 -text "x" -variable $variable \
	-command "$setProc($name) $option \$\{$variable\}" \
	-value "x" 
    radiobutton $frame.rbutton_x2 -text "x2" -variable $variable \
	-command "$setProc($name) $option \$\{$variable\}" \
	-value "x2"
    radiobutton $frame.rbutton_both -text "both" -variable $variable \
	-command "$setProc($name) $option \$\{$variable\}" \
	-value "both"
    blt_table $frame \
	$frame.rbutton_x1 0,0 -anchor w \
	$frame.rbutton_x2 0,1 -anchor w \
	$frame.rbutton_both 0,2 -anchor w

    blt_table $table($name) \
	$table($name).label$option $row,$column -anchor w \
	$table($name).frame$option $row,$column+1 -cspan 2 -fill x

    global $variable
    set $variable $value
    incr row 
}

proc MapXAxisReset { name option value } {
    global setProc
    set variable $name$option
    global $variable
    set $variable $value
    $setProc($name) $option $value
}

proc MapYAxisControl { name option value } {
    global row column table setProc

    label $table($name).label$option -text $option 
    frame $table($name).frame$option 
    set frame $table($name).frame$option 
    set variable $name$option

    radiobutton $frame.rbutton_y1 -text "y" -variable $variable \
	-command "$setProc($name) $option \$\{$variable\}" \
	-value "y" 
    radiobutton $frame.rbutton_y2 -text "y2" -variable $variable \
	-command "$setProc($name) $option \$\{$variable\}" \
	-value "y2"
    radiobutton $frame.rbutton_both -text "both" -variable $variable \
	-command "$setProc($name) $option \$\{$variable\}" \
	-value "both"
    blt_table $frame \
	$frame.rbutton_y1 0,0 -anchor w \
	$frame.rbutton_y2 0,1 -anchor w \
	$frame.rbutton_both 0,2 -anchor w

    blt_table $table($name) \
	$table($name).label$option $row,$column -anchor w \
	$table($name).frame$option $row,$column+1 -cspan 2 -fill x

    global $variable
    set $variable $value
    incr row 
}

proc MapYAxisReset { name option value } {
    global setProc
    set variable $name$option
    global $variable
    set $variable $value
    $setProc($name) $option $value
}

#
#
# Component configuration routines
#
#

proc GraphConfigure { interpName graphName } {
    global graph interp class RGB

    if ![info exists RGB] {
	RGBInit
    }
    set graph $graphName
    set interp $interpName
    if { $interp == "" } {
	set interp [winfo name .]
    }
    if [catch [list send $interp winfo class $graph] class ] {
	puts stderr "\"send\" disabled: can't communicate with \"$interp\""
	puts stderr $class
	exit 1
    }
}

proc ResetGraph {} {
    global graphOptions graphDefaults graph interp class

    RGBColorReset graph -background $graphOptions(-background)
    IntReset graph -borderwidth $graphOptions(-borderwidth)
    IntReset graph -bottommargin $graphOptions(-bottommargin)
    BooleanReset graph -bufferelements $graphOptions(-bufferelements)
    EntryReset graph -cursor $graphOptions(-cursor)
    EntryReset graph -font $graphOptions(-font)
    RGBColorReset graph -foreground $graphOptions(-foreground)
    IntReset graph -halo $graphOptions(-halo)
    IntReset graph -height $graphOptions(-height)
    BooleanReset graph -invertxy $graphOptions(-invertxy)
    IntReset graph -leftmargin $graphOptions(-leftmargin)
    RGBColorReset graph -plotbackground $graphOptions(-plotbackground)
    IntReset graph -plotborderwidth $graphOptions(-plotborderwidth)
    ReliefReset graph -plotrelief $graphOptions(-plotrelief)
    ReliefReset graph -relief $graphOptions(-relief)
    IntReset graph -rightmargin $graphOptions(-rightmargin)
    EntryReset graph -title $graphOptions(-title)
    IntReset graph -topmargin $graphOptions(-topmargin)
    IntReset graph -width $graphOptions(-width)
    send $interp $graph configure $graphDefaults
}


proc SetGraph { option value } {
    global graph interp
    send $interp [list $graph configure $option $value]
}

proc GetGraph {} {
    global graph interp
    send $interp [list $graph configure]
}


proc ConfigureGeneral {} {
    global graphOptions graphDefaults class
    global table row column setProc getProc 
    
    set name graph
    if { [info command .$name] == ".$name" } {
	destroy .$name
	return
    }
    set row 2
    set column 1
    set setProc($name) SetGraph
    set getProc($name) GetGraph

    GetOptions [$getProc($name)] graphOptions graphDefaults
    set table($name) .$name
    toplevel $table($name)
    wm title .$name "General Configuration"
    
    frame $table($name).frame -relief groove -bd 2
    label $table($name).title -text "General Configuration Options"
    RGBColorControl graph -background $graphOptions(-background)
    IntControl graph -borderwidth 0 10 $graphOptions(-borderwidth)
    IntControl graph -bottommargin 0 100 $graphOptions(-bottommargin)
    BooleanControl graph -bufferelements $graphOptions(-bufferelements)
    EntryControl graph -cursor $graphOptions(-cursor)
    EntryControl graph -font $graphOptions(-font)
    RGBColorControl graph -foreground $graphOptions(-foreground)
    IntControl graph -halo 0 100 $graphOptions(-halo)
    IntControl graph -height 150 800 $graphOptions(-height)
    set row 2
    set column 5
    BooleanControl graph -invertxy $graphOptions(-invertxy)
    IntControl graph -leftmargin 0 100 $graphOptions(-leftmargin)
    RGBColorControl graph -plotbackground $graphOptions(-plotbackground)
    IntControl graph -plotborderwidth 0 10 $graphOptions(-plotborderwidth)
    ReliefControl graph -plotrelief $graphOptions(-plotrelief)
    ReliefControl graph -relief $graphOptions(-relief)
    IntControl graph -rightmargin 0 100 $graphOptions(-rightmargin)
    EntryControl graph -title $graphOptions(-title)
    IntControl graph -topmargin 0 100 $graphOptions(-topmargin)
    IntControl graph -width 150 800 $graphOptions(-width)

    button $table($name).button_done -text "done" \
	-command "destroy .$name; set varGeneral 0"
    button $table($name).button_reset -text "reset" \
	-command "ResetGraph"
    incr row
    if { $row & 1 } {
	incr row
    }
    blt_table $table($name) \
	$table($name).frame 0,0 -cspan 24 -rspan $row -fill both \
	-padx 4 -pady 4 \
	$table($name).title 		1,0 -cspan 24 \
	$table($name).button_reset 	$row,$column -reqwidth .7i -anchor w \
	$table($name).button_done 	$row,$column+1 -reqwidth .7i -anchor e
    blt_table column $table($name) configure { 1 5 } -padx 5
    blt_table row $table($name) configure all -pady 5
    blt_table column $table($name) configure { 0 4 9 } -width 0.25i
    blt_table row $table($name) configure { 0 $row-1 } -height 0.125i
    blt_table row $table($name) configure { $row } -height {}
}


proc ResetAxis { axis } {
    global axisOptions axisDefaults graph interp

    RGBColorReset axis -color $axisOptions(-color)
    BooleanReset axis -descending $axisOptions(-descending)
    EntryReset axis -font $axisOptions(-font)
    IntReset axis -linewidth $axisOptions(-linewidth)
    BooleanReset axis -logscale $axisOptions(-logscale)
    BooleanReset axis -loose $axisOptions(-loose)
    BooleanReset axis -mapped $axisOptions(-mapped)
    EntryReset axis -max $axisOptions(-max)
    EntryReset axis -min $axisOptions(-min)
    FloatReset axis -rotate 1.0 $axisOptions(-rotate)
    BooleanReset axis -showticks $axisOptions(-showticks)
    FloatReset axis -stepsize 1.0 $axisOptions(-stepsize)
    IntReset axis -subticks $axisOptions(-subticks)
    IntReset axis -ticklength $axisOptions(-ticklength)
    EntryReset axis -title $axisOptions(-title)
    send $interp $graph $axis configure $axisDefaults
}

proc GetAxis {} {
    global axis graph interp
    send $interp [list $graph $axis configure]
}

proc SetAxisOptions { axis } {
    global getProc axisOptions axisDefaults 
    GetOptions [GetAxis] axisOptions axisDefaults
    ResetAxis $axis
}


proc SetAxis { option value } {
    global axis graph interp
    send $interp [list $graph $axis configure $option $value]
}

proc ConfigureAxis {} {
    global axisOptions axisDefaults 
    global table row column axis setProc getProc

    set name axis
    if { [info command .$name] == ".$name" } {
	destroy .$name
	return
    }
    set row 3
    set column 1
    set axis xaxis
    set setProc($name) SetAxis
    set getProc($name) GetAxis

    GetOptions [$getProc($name)] axisOptions axisDefaults
    set table($name) .$name
    toplevel $table($name)
    wm title .$name "Axis Configuration"

    frame $table($name).frame -relief groove -bd 2
    label $table($name).title -text "Axis Configuration Options"
    
    frame $table($name).axis 
    radiobutton $table($name).axis.x -text "x" -variable axis \
	-value "xaxis" \
	-command "SetAxisOptions xaxis" 

    radiobutton $table($name).axis.y -text "y" -variable axis \
	-value "yaxis" \
	-command "SetAxisOptions yaxis" 

    radiobutton $table($name).axis.x2 -text "x2" -variable axis \
	-value "x2axis" \
	-command "SetAxisOptions x2axis" 

    radiobutton $table($name).axis.y2 -text "y2" -variable axis \
	-value "y2axis" \
	-command "SetAxisOptions y2axis" 

    pack append $table($name).axis \
	$table($name).axis.x { left expand fill } \
	$table($name).axis.y { left expand fill } \
	$table($name).axis.x2 { left expand fill } \
	$table($name).axis.y2 { left expand fill } 

    RGBColorControl axis -color $axisOptions(-color)
    BooleanControl axis -descending $axisOptions(-descending)
    EntryControl axis -font $axisOptions(-font)
    IntControl axis -linewidth 0 10 $axisOptions(-linewidth)
    BooleanControl axis -logscale $axisOptions(-logscale)
    BooleanControl axis -loose $axisOptions(-loose)
    BooleanControl axis -mapped $axisOptions(-mapped)
    EntryControl axis -max $axisOptions(-max)
    EntryControl axis -min $axisOptions(-min)
    FloatControl axis -rotate 0 360 1.0 $axisOptions(-rotate)
    BooleanControl axis -showticks $axisOptions(-showticks)
    FloatControl axis -stepsize 0 100 1.0 $axisOptions(-stepsize)
    IntControl axis -subticks 0 10 $axisOptions(-subticks)
    IntControl axis -ticklength -20 20 $axisOptions(-ticklength)
    EntryControl axis -title $axisOptions(-title)
    
    button $table($name).button_done -text "done" \
	-command "destroy .$name; set varAxis 0"
    button $table($name).button_reset -text "reset" \
	-command "ResetAxis $axis"
    incr row
    blt_table $table($name) \
	$table($name).frame 0,0 -cspan 14 -rspan $row -fill both \
	-padx 4 -pady 4 \
	$table($name).title 		1,0 -cspan 7 \
	$table($name).axis		2,1 -fill x -cspan 3  \
	$table($name).button_reset 	$row,2 -reqwidth .7i -anchor w \
	$table($name).button_done 	$row,3 -reqwidth .7i -anchor e
    blt_table column $table($name) configure 1 -padx 5
    blt_table row $table($name) configure all -pady 5
    blt_table column $table($name) configure { 0 4 } -width 0.25i
    blt_table row $table($name) configure { 0 $row-1 } -height 0.125i
    blt_table row $table($name) configure { $row } -height {}
}


proc ResetLegend {} {
    global legendOptions legendDefaults graph interp

    RGBColorReset legend -activebackground $legendOptions(-activebackground)
    IntReset legend -activeborderwidth $legendOptions(-activeborderwidth)
    RGBColorReset legend -activeforeground $legendOptions(-activeforeground)
    ReliefReset legend -activerelief $legendOptions(-activerelief)
    AnchorReset legend -anchor $legendOptions(-anchor)
    RGBColorReset legend -background $legendOptions(-background)
    IntReset legend -borderwidth $legendOptions(-borderwidth)
    EntryReset legend -font $legendOptions(-font)
    RGBColorReset legend -foreground $legendOptions(-foreground)
    IntReset legend -ipadx $legendOptions(-ipadx)
    IntReset legend -ipady $legendOptions(-ipady)
    BooleanReset legend -mapped $legendOptions(-mapped)
    IntReset legend -padx $legendOptions(-padx)
    IntReset legend -pady $legendOptions(-pady)
    EntryReset legend -position $legendOptions(-position)
    ReliefReset legend -relief $legendOptions(-relief)
    send $interp $graph legend configure $legendDefaults
}


proc SetLegend { option value } {
    global graph interp
    send $interp [list $graph legend configure $option $value]
}

proc GetLegend {} {
    global graph interp
    send $interp [list $graph legend configure]
}

proc ConfigureLegend {} {
    global legendOptions legendDefaults
    global table row column setProc getProc

    set name legend
    if { [info command .$name] == ".$name" } {
	destroy .$name
	return
    }
    set row 2
    set column 1
    set setProc($name) SetLegend
    set getProc($name) GetLegend

    GetOptions [$getProc($name)] legendOptions legendDefaults
    set table($name) .$name
    toplevel $table($name)
    wm title .$name "Legend Configuration"

    frame $table($name).frame -relief groove -bd 2
    label $table($name).title -text "Legend Configuration Options"

    RGBColorControl legend -activebackground $legendOptions(-activebackground)
    IntControl legend -activeborderwidth 0 10 \
	$legendOptions(-activeborderwidth)
    RGBColorControl legend -activeforeground $legendOptions(-activeforeground)
    ReliefControl legend -activerelief $legendOptions(-activerelief)
    AnchorControl legend -anchor $legendOptions(-anchor)
    RGBColorControl legend -background $legendOptions(-background)
    IntControl legend -borderwidth 0 10 $legendOptions(-borderwidth)
    EntryControl legend -font $legendOptions(-font)
    RGBColorControl legend -foreground $legendOptions(-foreground)
    IntControl legend -ipadx 0 10 $legendOptions(-ipadx)
    IntControl legend -ipady 0 10 $legendOptions(-ipady)
    BooleanControl legend -mapped $legendOptions(-mapped)
    IntControl legend -padx 0 10 $legendOptions(-padx)
    IntControl legend -pady 0 10 $legendOptions(-pady)
    EntryControl legend -position $legendOptions(-position)
    ReliefControl legend -relief $legendOptions(-relief)

    button $table($name).button_done -text "done" \
	-command "destroy .$name; set varLegend 0"
    button $table($name).button_reset -text "reset" \
	-command "ResetLegend"
    incr row
    blt_table $table($name) \
	$table($name).frame 0,0 -cspan 5 -rspan $row -fill both \
		-padx 4 -pady 4 \
	$table($name).title 		1,0 -cspan 4 \
	$table($name).button_reset 	$row,2 -reqwidth .7i -anchor w \
	$table($name).button_done 	$row,3 -reqwidth .7i -anchor e
    blt_table column $table($name) configure 1 -padx 5
    blt_table row $table($name) configure all -pady 5
    blt_table column $table($name) configure { 0 4 } -width 0.25i
    blt_table column $table($name) configure { 0 4 } -width 0.25i
    blt_table row $table($name) configure { 0 $row-1 } -height 0.125i
    blt_table row $table($name) configure { $row } -height {}
}

proc ResetCrosshairs {} {
    global xhairsOptions xhairsDefaults graph interp

    RGBColorReset crosshairs -color $xhairsOptions(-color)
    IntReset crosshairs -dashes $xhairsOptions(-dashes)
    IntReset crosshairs -linewidth $xhairsOptions(-linewidth)
    BooleanReset crosshairs -mapped $xhairsOptions(-mapped)
    EntryReset crosshairs -position $xhairsOptions(-position)
    send $interp $graph crosshairs configure $xhairsDefaults
}

proc SetCrosshairs { option value } {
    global graph interp
    send $interp [list $graph crosshairs configure $option $value]
}

proc GetCrosshairs {} {
    global graph interp
    send $interp [list $graph crosshairs configure]
}

proc ConfigureCrosshairs {} {
    global xhairsOptions xhairsDefaults 
    global table row column setProc getProc

    if { [info command .crosshairs] == ".crosshairs" } {
	destroy .crosshairs
	return
    }
    set name crosshairs
    set row 2
    set column 1
    set setProc($name) SetCrosshairs
    set getProc($name) GetCrosshairs

    GetOptions [$getProc($name)] xhairsOptions xhairsDefaults
    set table($name) .crosshairs
    toplevel $table($name)
    wm title .$name "Crosshairs Configuration"

    frame $table($name).frame -relief groove -bd 2
    label $table($name).title -text "Crosshairs Configuration Options"

    RGBColorControl crosshairs -color $xhairsOptions(-color)
    IntControl crosshairs -dashes 0 10 $xhairsOptions(-dashes)
    IntControl crosshairs -linewidth 0 10 $xhairsOptions(-linewidth)
    BooleanControl crosshairs -mapped $xhairsOptions(-mapped)
    EntryControl crosshairs -position $xhairsOptions(-position)

    button $table($name).button_done -text "done" \
	-command "destroy .$name; set varCrosshairs 0"
    button $table($name).button_reset -text "reset" \
	-command "ResetCrosshairs"
    incr row
    blt_table $table($name) \
	$table($name).frame 0,0 -cspan 7 -rspan $row -fill both \
	-padx 4 -pady 4 \
	$table($name).title 		1,0 -cspan 7 \
	$table($name).button_reset 	$row,2 -reqwidth .7i -anchor w \
	$table($name).button_done	$row,3 -reqwidth .7i -anchor e
    blt_table column $table($name) configure 1 -padx 5
    blt_table row $table($name) configure all -pady 5
    blt_table column $table($name) configure { 0 4 } -width 0.25i
    blt_table row $table($name) configure { 0 $row-1 } -height 0.125i
}


proc ResetPostScript {} {
    global psOptions psDefaults graph interp

    EntryReset postscript -colormap $psOptions(-colormap)
    EntryReset postscript -colormode $psOptions(-colormode)
    EntryReset postscript -fontmap $psOptions(-fontmap)
    BooleanReset postscript -landscape $psOptions(-landscape)
    AnchorReset postscript -pageanchor $psOptions(-pageanchor)
    IntReset postscript -pageheight $psOptions(-pageheight)
    IntReset postscript -pagewidth $psOptions(-pagewidth)
    IntReset postscript -pagex $psOptions(-pagex)
    IntReset postscript -pagey $psOptions(-pagey)
    send $interp $graph psconfigure $psDefaults
}

proc SetPostScript { option value } {
    global graph interp
    send $interp [list $graph psconfigure $option $value]
}

proc GetPostScript {} {
    global graph interp
    send $interp [list $graph psconfigure]
}

proc ConfigurePostScript {} {
    global psOptions psDefaults 
    global table row column setProc getProc

    if { [info command .postscript] == ".postscript" } {
	destroy .postscript
	return
    }
    set name postscript
    set row 2
    set column 1
    set setProc($name) SetPostScript
    set getProc($name) GetPostScript

    GetOptions [$getProc($name)] psOptions psDefaults
    set table($name) .postscript
    toplevel $table($name)
    wm title .$name "PostScript Configuration"
    
    frame $table($name).frame -relief groove -bd 2
    label $table($name).title -text "PostScript Configuration Options"

    EntryControl postscript -colormap $psOptions(-colormap)
    EntryControl postscript -colormode $psOptions(-colormode)
    EntryControl postscript -fontmap $psOptions(-fontmap)
    BooleanControl postscript -landscape $psOptions(-landscape)
    AnchorControl postscript -pageanchor $psOptions(-pageanchor)
    IntControl postscript -pageheight 0 900 $psOptions(-pageheight)
    IntControl postscript -pagewidth 0 900 $psOptions(-pagewidth)
    IntControl postscript -pagex 0 900 $psOptions(-pagex)
    IntControl postscript -pagey 0 900 $psOptions(-pagey)

    button $table($name).button_done -text "done" \
	-command "destroy .$name; set varPostScript 0"
    button $table($name).button_reset -text "reset" \
	-command "ResetPostScript"
    incr row
    blt_table $table($name) \
	$table($name).frame 0,0 -cspan 7 -rspan $row -fill both \
	-padx 4 -pady 4 \
	$table($name).title 		1,0 -cspan 7 \
	$table($name).button_reset 	$row,2 -reqwidth .7i -anchor w \
	$table($name).button_done	$row,3 -reqwidth .7i -anchor e
    blt_table column $table($name) configure 1 -padx 5
    blt_table row $table($name) configure all -pady 5
    blt_table column $table($name) configure { 0 4 } -width 0.25i
    blt_table row $table($name) configure { 0 $row-1 } -height 0.125i
}


proc ResetElement {} {
    global elemOptions elemDefaults element graph interp class

    RGBColorReset element -activebackground $elemOptions(-activebackground)
    RGBColorReset element -activeforeground $elemOptions(-activeforeground)
    RGBColorReset element -background $elemOptions(-background)
    EntryReset element -data $elemOptions(-data)
    RGBColorReset element -foreground $elemOptions(-foreground)
    EntryReset element -label $elemOptions(-label)
    MapXAxisReset element -mapx $elemOptions(-mapx)
    MapYAxisReset element -mapy $elemOptions(-mapy)
    EntryReset element -xdata $elemOptions(-xdata)
    EntryReset element -ydata $elemOptions(-ydata)

    IntReset element -borderwidth $elemOptions(-borderwidth)
    if { $class == "Blt_graph" } {
	IntReset element -activelinewidth $elemOptions(-activelinewidth)
	IntReset element -dashes $elemOptions(-dashes)
	IntReset element -linewidth $elemOptions(-linewidth)
	TraceReset element -trace  $elemOptions(-trace)
	SymbolReset element -symbol $elemOptions(-symbol)
	FloatReset element -scale 0.01 $elemOptions(-scale)
    } else {
	ReliefReset element -relief $elemOptions(-relief)
	BooleanReset element -stacked $elemOptions(-stacked)
	EntryReset element -stipple $elemOptions(-stipple)
    } 
    send $interp $graph element configure $element $elemDefaults
}

proc SetElement { option value } {
    global graph element interp

    send $interp [list $graph element configure $element $option $value]
}

proc GetElement {} {
    global graph element interp

    send $interp [list $graph element configure $element]
}

proc GetElementNames {} {
    global graph element interp

    send $interp [list $graph element names]
}

proc SetElemOptions {} {
    global elemOptions elemDefaults graph element 

    GetOptions [GetElement] elemOptions elemDefaults
    ResetElement 
}

proc ConfigureElement {} {
    global elemOptions elemDefaults class
    global table graph row column element setProc getProc 
        
    set elements [lsort [GetElementNames]]
    if { [llength $elements] < 1 } {
	return
    }
    set element [lindex $elements 0]
    GetOptions [GetElement] elemOptions elemDefaults

    set name element
    if { [info command .$name] == ".$name" } {
	destroy .$name
	return
    }
    set setProc($name) SetElement
    set getProc($name) GetElement

    set table($name) .$name
    toplevel $table($name)
    wm title .$name "Element Configuration"

    frame $table($name).frame -relief groove -bd 2
    label $table($name).title -text "Element Configuration Options"

    set count 0
    label $table($name).name -text "Elements:"
    frame $table($name).elements 
    set frame $table($name).elements
    foreach i $elements {
	radiobutton $frame.rbutton_$i -variable element -value $i -text $i \
	    -command "SetElemOptions"
	blt_table $frame $frame.rbutton_$i $count/2,$count%2 -anchor w
	incr count
    }

    set row 3
    set column 1
    RGBColorControl element -activebackground $elemOptions(-activebackground)
    RGBColorControl element -activeforeground $elemOptions(-activeforeground)
    if { $class == "Blt_graph" } {
	IntControl element -activelinewidth 0 10 $elemOptions(-activelinewidth)
	RGBColorControl element -background $elemOptions(-background)
	IntControl element -borderwidth 0 10 $elemOptions(-borderwidth)
	IntControl element -dashes 0 10 $elemOptions(-dashes)
	EntryControl element -data $elemOptions(-data)
	RGBColorControl element -foreground $elemOptions(-foreground)
	EntryControl element -label $elemOptions(-label)
	IntControl element -linewidth 0 10 $elemOptions(-linewidth)
	MapXAxisControl element -mapx $elemOptions(-mapx)
	MapYAxisControl element -mapy $elemOptions(-mapy)
	TraceControl element -trace  $elemOptions(-trace)
	SymbolControl element -symbol $elemOptions(-symbol)
	FloatControl element -scale 0 300 0.01 $elemOptions(-scale)
	EntryControl element -xdata $elemOptions(-xdata)
	EntryControl element -ydata $elemOptions(-ydata)
    } else {
	RGBColorControl element -background $elemOptions(-background)
	IntControl element -borderwidth 0 10 $elemOptions(-borderwidth)
	EntryControl element -data $elemOptions(-data)
	RGBColorControl element -foreground $elemOptions(-foreground)
	EntryControl element -label $elemOptions(-label)
	MapXAxisControl element -mapx $elemOptions(-mapx)
	MapYAxisControl element -mapy $elemOptions(-mapy)
	ReliefControl element -relief $elemOptions(-relief)
	BooleanControl element -stacked $elemOptions(-stacked)
	EntryControl element -stipple $elemOptions(-stipple)
	EntryControl element -xdata $elemOptions(-xdata)
	EntryControl element -ydata $elemOptions(-ydata)
    }
    button $table($name).button_done -text "done" \
	-command "destroy .$name; set varElement 0"
    button $table($name).button_reset -text "reset" \
	-command "ResetElement"
    incr row
    blt_table $table($name) \
	$table($name).frame 0,0 -cspan 14 -rspan $row -fill both -padx 4 \
	-pady 4 \
	$table($name).title 		1,0 -cspan 14 \
	$table($name).name		2,1 -anchor e \
	$table($name).elements 	2,2 -cspan 2 -fill x \
	$table($name).button_reset 	$row,$column+1 -reqwidth .7i -anchor w \
	$table($name).button_done 	$row,$column+2 -reqwidth .7i -anchor e
    blt_table column $table($name) configure { 1 5 } -padx 5
    blt_table row $table($name) configure all -pady 5
    blt_table column $table($name) configure { 0 4 } -width 0.25i
    blt_table row $table($name) configure { 0 $row-1 } -height 0.125i
    blt_table row $table($name) configure { $row } -height {}
}

if { $argc != 2 } {
    error "usage: grconf interp window"
}
set interps [lsort [winfo interps]]

set row 2
set column 0

foreach i { General Element Axis Legend Crosshairs PostScript } {
    checkbutton .cbutton_$i \
	-text $i \
	-command "Configure$i" \
	-relief flat \
	-variable var$i
    blt_table . \
	.cbutton_$i $row,$column -anchor w  -cspan 2 
    incr row
}
label .title -text "Graph Configure Utility"
message .msg -text "...configuring \"[lindex $argv 1]\"" -aspect 5000
label .logo -bitmap BLT
button .quit -text "quit" -command "destroy ."
blt_table . \
    .title 0,0 -fill both -cspan 2 \
    .msg   1,0 -cspan 2 -fill both  \
    .logo  $row,$column \
    .quit  $row,$column+1 -reqwidth .75i


GraphConfigure [lindex $argv 0] [lindex $argv 1]

#debug 50

