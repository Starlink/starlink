# MenuEntry.tcl - itcl widget for displaying a menubutton and an entry
#
# "@(#) $Id: MenuEntry.tcl,v 1.1.1.1 1996/02/26 13:10:13 abrighto Exp $"


itcl_class MenuEntry {
    inherit FrameWidget

    #  create new LabelEntry
    constructor {config} {
	FrameWidget::constructor

	set menu_ $this.mb.m
	pack [menubutton $this.mb \
		  -menu $menu_ \
		  -indicatoron $indicatoron] \
	    -side left
	menu $menu_
	set default_bg_ [$this.mb cget -background]

	pack [entry $this.entry -relief $relief] \
	    -side left -expand $expand -fill $fill -padx 1m -ipadx 1m

	set initialized_ 1

	#  Explicitly handle config's that may have been ignored earlier
	foreach attr $config {
	    config -$attr [set $attr]
	}

	# trace this radiobutton var to change menubutton label
	global $this.var
	trace variable $this.var w "$this update_menubutton"
    }

    
    # destructor

    destructor {
	global $this.var
	catch {unset $this.var}
    }


    #  Get the value in the entry

    method get {} {
	return [set entryValue [$this.entry get]]
    }


    # add an item to the menu.
    # The args may be the options: 
    #     -label <label for menuitem and menubutton when chosen>
    #     -bitmap <bitmap for menuitem and menubutton when chosen>
    #     -command <cmd to execute when item is selected>
    #     -bg <color of menu item and button when chosen>
    #    -font <font of menu item and button when chosen>

    method add {args} {
	global $this.var
	
	# value of radiobutton variable is the command to configure the menubutton
	set mbValue "$this.mb config"

	# command to add menu item
	set cmd "$menu_ add radiobutton"

	set n [llength $args]
	for {set i 0} {$i < $n} {incr i 2} {
	    lassign [lrange $args $i end] opt arg
	    switch -exact [string range $opt 1 end] {
		label {
		    lappend mbValue -text $arg
		    lappend cmd -label $arg
		    set id $arg
		}
		bitmap {
		    lappend mbValue -bitmap $arg
		    lappend cmd -bitmap $arg
		    set id $arg
		}
		command {
		    lappend cmd -command $arg
		}
		bg -
		background {
		    if {"$arg" == ""} {
			set mb_color $default_bg_
		    } else {
			set mb_color $arg
		    }
		    lappend mbValue -background $mb_color -activebackground $mb_color
		    lappend cmd -background $arg -activebackground $arg
		}
		font {
		    lappend mbValue -font $arg
		    lappend cmd -font $arg
		}
	    }
	}
	
	lappend cmd -value $mbValue -variable $this.var
	eval $cmd
	
	# save label or bitmap name for later reference
	set values_($id) $mbValue

	# set default value to first item
	if {"[::set $this.var]" == ""} {
	    ::set $this.var $mbValue
	}
    }

    
    # update the label on the menubutton

    method update_menubutton {args} {
	global $this.var
	if {"[::set $this.var]" != ""} {
	    eval [::set $this.var]
	}
    }


    #  called for for return or keypress in entry, calls action proc with new value

    method _action_proc {cmd} {
	lappend cmd [$this.entry get]
	eval $cmd
    }

    
    # public data


    # set the selected menu value (referenced by label or bitmap)
    # note: this.var is the trace variable and values_ holds the
    # valid values.
    public mbValue {} {
	if {[info exists values_($mbValue)]} {
	    global $this.var
	    ::set $this.var $values_($mbValue)
	}
    }

    # the width of the menu
    public mbWidth {} {
	if {$initialized_} {
	    $this.mb config -width $mbWidth
	}
    }

    # the width of the entry
    public entryWidth {} {
	if {$initialized_} {
	    $this.entry config -width $entryWidth
	}
    }

    # set the font for  displaying the menu
    public mbFont {} {
	if {$initialized_} {
	    $this.mb config -font $mbFont
	}
    }
    
    # set the font for displaying the entry
    public entryFont {} {
	if {$initialized_} {
	    $this.entry config -font $entryFont
	}
    }

    # the action for <Return> in the entry, called with the new value
    public action {} {
	if {$initialized_} {
	    bind $this.entry <Return> [list $this _action_proc $action]
	}
    }

    # menu relief
    public mbRelief {flat} {
	if {$initialized_} {
	    $this.mb config -relief $mbRelief
	}
    }
    # entry relief
    public entryRelief {flat} {
	if {$initialized_} {
	    $this.entry config -relief $entryRelief
	}
    }

    # set pack arg for expand on entry
    public expand 1

    # set pack arg for fill on entry
    public fill x

    # flag: if true, display the indicator in the menubutton
    public indicatoron 1
    
    # set the menubutton anchor (the entry follows)
    public anchor {} {
	if {$initialized_} {
	    $this.mb config -anchor $anchor
	}
    }
    
    # set the state to normal/disabled 
    public state {normal} {
	if {$initialized_} {
	    $this.mb config -state $state
	    $this.entry config -state $state
	    if {"$state" == "normal"} {
		$this.entry config -fg black
	    } else {
		$this.entry config -fg grey
	    }
	}
    }


    # -- protected variables --

    # menu widget
    protected menu_

    # default background color
    protected default_bg_

    # array(label or bitmap name) of values for selecting a radiobutton
    protected values_

    # set to 1 in constructor
    protected initialized_ 0
}


