# E.S.O. - VLT project/ ESO Archive
# "@(#) $Id$"
#
# TopLevelWidget.tcl - Itk base class for popup windows
#
# See the man page for a complete description.
#
# who             when       what
# --------------  ---------  ----------------------------------------
# Allan Brighton  01 Jun 94  Created
# P.W. Draper     04 Jul 96. Added -takefocus to help text.
#                 15 Nov 96. Added -help configuration option.
#                 19 Nov 96. Stopped window deletion until none left.
#                 10 Dec 96. Added configure_menubutton.
#                 20 Jan 98. Fixed busy focus -lastfor so that it does
#                            restore the focus, not just get the
#                            window name.  

# The TopLevelWidget itcl class is a subclass of itk::Toplevel and thus
# inherits all of the features described in Toplevel(n). In addition, a
# number of useful methods are defined, for use by the derived classes.

class util::TopLevelWidget {
    inherit itk::Toplevel

    # Create a toplevel widget with the same name as this object.

    constructor {args} {
	set class [$this info class]
	set w_ $itk_component(hull)
	wm iconname $w_ $class
	if {"[winfo parent $w_]" != "."} {
	    wm protocol $w_ WM_DELETE_WINDOW "wm withdraw $w_"
	}
	eval itk_initialize $args

	after idle [code $this call_init]

	# reset to previous position, if there was one
	global toplevel_position_
	if {[info exists toplevel_position_($w_)]} {
	    wm geometry $w_ $toplevel_position_($w_)
	}
	
	# eval command, if there is one
	if {"$command_" != ""} {
	    set cmd $command_
	    lappend cmd $w_
	    eval $cmd
	}
    }

    
    # destructor: remember window position for next create of same window...

    destructor {
	global toplevel_position_
	if {[winfo exists $w_]} {
	    set toplevel_position_($w_) "+[winfo x $w_]+[winfo y $w_]"
	}
    }


    # this method is called after all options have been evaluated and is meant
    # to be redefined in a derived class

    method init {} {
    }


    # call the init method and then check if there is a plugin for this class.

    method call_init {} {
	init
	check_plugin
    }


    # Start an itcl application - that is, an application with an itcl
    # TopLevel widget as its main window. 
    # This proc assumes that the command line args (global: $argv) 
    # should be passed on to the class (which is derived from this class).
    # If any errors occur, we print a usage message based on the itcl
    # info about the class.
    #
    # If default_opt is specified, args without options are treated as
    # belonging to that option (-file, for example)
    #
    # an optional "usage" message may also be specified, to be printed
    # when unknown options are found.
    #
    # If "name" is not specified, the class name in lower case is used with
    # a "." prepended.

    proc start {class {default_opt ""} {usage ""} {name ""}} {
	global ::argv ::argc

	# hide the "." window since an itcl class will replace it
	wm withdraw .

	# get the derived class name from the calling scope
	#set class [uplevel info class]
	if {"$name" == ""} {
	    set name .[info namespace tail [string tolower $class]]
	}

	set default_arg {}
	set cmd "$class $name -standalone 1" 
	for {set i 0} {$i < $argc} {incr i} {
	    set opt [lindex $argv $i]
	    if {"[string index $opt 0]" == "-" && "$opt" != "-"} {
		set arg [lindex $argv [incr i]]
	    } else {
		if {"$default_opt" == "" || "$default_arg" != ""} {
		    start_err_ $class $name $default_opt "invalid option $opt" $usage
		}
		set arg [set default_arg $opt]
		set opt $default_opt
	    }
	    lappend cmd $opt $arg
	}

	# create the object 
	if {[catch "@scope :: $cmd" msg]} {
	    start_err_ $class $name $default_opt $msg $usage
	}
	
	# keep track of main windows
	global main_windows
	set main_windows(.) 0

	# exit when all toplevel window based classes exit - don't wait for "."
	if {[winfo exists $name]} {
	    set main_windows($name) 1
	    tkwait window $name
	    unset main_windows($name)
	}
	if {[llength [array names main_windows]] == 0} {
	    exit 0
	}
    }

    
    # This proc is called by "start" above, in case of errors.
    # It attempts to print an informative message indicating
    # which option was wrong and what options are available by
    # using the itcl "info" command.
    #
    # The args are the itcl class name, widget name, default option
    # (-file, for example) and an error message.

    proc start_err_ {class name {default_opt ""} {msg ""} {usage ""}} {
	global ::argv0 ::errorInfo

	set prog [file rootname [file tail $argv0]]
	if {[string match "unknown option*" $msg]} {
	    puts stderr "$prog: $msg"
	    if {"$usage" == ""} {
		if {"$default_opt" == ""} {
		    set def ""
		} else {
		    set def "?[lrange $default_opt 1 end]? "
		}
		puts stderr "Usage: $prog ${def}?option value? ..."
	    } else {
		puts $usage
	    }
	} else {
	    puts stderr "$prog: $msg\n\nTraceback:$errorInfo\n"
	}
	
	exit 1
    }


    # Use this method to quit the application if you might want to 
    # reuse the window later

    method quit {} {
	if {$itk_option(-standalone)} {
	    destroy $w_
	} else {
	    wm withdraw $w_
	}
    }
	    
	    
    # run the given tcl command in the scope of this class
    # while displaying the (blt) busy cursor in the toplevel
    # window

    method busy {cmd} {
	global ::errorInfo ::errorCode
	if {[incr busy_count_] == 1} {
	    catch {focus .}
	    blt::busy hold $w_
	    update idletasks
	}
	
	# save any errors and report them later
	if {[set code [catch [list uplevel $cmd] msg]]} {
	    set info $errorInfo
	} 

	if {[incr busy_count_ -1] == 0} {
	    blt::busy release $w_
            catch {focus [focus -lastfor $w_]}
	}

	if {$code} {
	    uplevel [list error $msg $info $code]
	}
    }


    # run the given tcl command and print out any errors

    method test {cmd} {
	if {[catch [list uplevel $cmd] msg]} {
	    puts stderr "test: $msg"
	    error $msg
	}
    }


    # add a menubar to the main frame

    method add_menubar {} {
	itk_component add menubar {
	    frame $w_.menubar -relief raised -borderwidth 2
	} {
	}
	pack $itk_component(menubar) \
	    -side top -fill x -ipady 1m
    }


    # Add a menu button to the menubar, or reset it's menu to empty 
    # if the menubutton already exists.
    # If helptext is specified, it is used as the short help text.
    # If side is specified, the menu button is placed on the given side

    method add_menubutton {label {helptext ""} {side left}} {
	set f $itk_component(menubar)
	set name [string tolower $label]
	set mb $f.$name
	set m $mb.m

	if {[winfo exists $m]} {
	    destroy $m
	} else {
	    itk_component add $name {
		menubutton $mb -text $label -menu $m
	    } {
	    }
	    # for backward compat
	    if {"$helptext" == "right"} {
		set side right
	    }
	    pack $itk_component($name) \
		-side $side -padx 1m -ipadx 1m
	}
	menu $m
	bind $m <Motion> "+[code $this menu_motion $m %y]"
	
	if {"$helptext" != ""} {
	    add_short_help $mb $helptext
	}
	
	return $m
    }

    
    # return the path name of the menubutton for the given menubutton label

    method get_menubutton {label} {
	set f $itk_component(menubar)
	set name [string tolower $label]
	return $f.$name
    }


    # return the path name of the menu for the given menubutton label

    method get_menu {label} {
	return [get_menubutton $label].m
    }


    # called for motion events in menubar menus for short help use

    method menu_motion {m y} {
	if {[catch {$short_help_win_ short_help $menu_item_help_($m,[$m entrycget @$y -label])}]} {
	    short_help {}
	}
    }

    # configure something of the named menubutton.
    method configure_menubutton {label args} {
       set name [string tolower $label]
       if { [info exists itk_component($name)] } {
          eval $itk_component($name) configure $args
       }
    }

    # if any help has been defined then add a button to show it
    method add_help_button {file label {msg ""}} {
       if { $helpbutton_ == "" } { 
          set helpbutton_ [add_menubutton "Help" right]
       }
       $helpbutton_ add command -label "$label" \
          -command [code $this show_help $file]
       configure_menubutton Help -underline 0
       if { $msg != "" } {  
          set menu_item_help_($helpbutton_,$label) $msg


       }
       return $helpbutton_
    }

    # Center this window on the screen. 
    # this doesn't work with gridded geometry...

    method center_window {} {
	if {"[wm grid $w_ ]" != ""} {
	    wm geom $w_ +200+200
	} else {
	    wm withdraw $w_
	    update idletasks
	    set parent [winfo parent $w_]
	    set x [expr [winfo screenwidth $w_]/2 - [winfo reqwidth $w_]/2 \
		       - [winfo vrootx $parent]]
	    set y [expr [winfo screenheight $w_]/2 - [winfo reqheight $w_]/2 \
		       - [winfo vrooty $parent]]
	    wm geom $w_ +$x+$y
	}
	wm deiconify $w_
    }

	    
    # add a subwindow at the bottom of the screen for short help messages

    method make_short_help {{side bottom}} {
	itk_component add short_help {
	    text $w_.shelp \
		-borderwidth 3 \
		-height 1 \
		-width 1 \
		-wrap none \
		-relief groove \
		-font -Adobe-helvetica-medium-r-normal--14* 
	} {
	    rename -font -helpfont helpFont HelpFont
	}
	set w $itk_component(short_help)
	# Kim added this
	set short_help_win_ $this
	pack $w -side $side -anchor w -fill x -ipady 1m
	set bitmap_bg_ [$w cget -background]

	$w window create end \
	    -window [label $w.i \
			 -background $bitmap_bg_ \
			 -foreground $bitmap_fg_ \
			 -bitmap information]
	$w insert end " "
	add_short_help $w {Short help: note: \
					{bitmap b1} means press mouse button <1>, \
					{bitmap dragb1} = drag <1>, \
					{bitmap shiftdragb1} = drag shift <1>}
	$w config -state disabled
    }


    # set the text of the short help message to be displayed whenever
    # the mouse enters the widget w (see short_help below)
    # If "win" is specified, it is used to display the help text
    # rather than this window (it should also be a TopLevelWidget).

    method add_short_help {w msg} {
	bind $w <Enter> "+[code $short_help_win_ short_help $msg %m]"
	bind $w <Leave> "+[code $short_help_win_ short_help {} %m]"
    } 

    
    # set the text of the short help message to be displayed whenever
    # the mouse enters the menu item with the given label (see short_help below)

    method add_menu_short_help {menu label msg} {
	set menu_item_help_($menu,$label) $msg
    } 

    
    # add a menu item of the given type to the given menu and arrange to have 
    # the given short help message displayed when the mouse is over the item.
    # The extra args are passed to the "$menu add" command.

    method add_menuitem {menu type label msg args} {
	eval [concat [list $menu add $type -label $label] $args]
	set menu_item_help_($menu,$label) $msg

	# if -accelerator was specified, add key binding for command
	set n [llength $args]
	set key {}
	set cmd {}
	for {set i 0} {$i < $n} {incr i 2} {
	    set opt [lindex $args $i]
	    set arg [lindex $args [expr $i+1]]
	    if {"$opt" == "-accelerator"} {
		set key $arg
	    } elseif {"$opt" == "-command"} {
		set cmd $arg
	    }
	}
	if {"$key" != ""} {
	    bind $w_ <$key> [list $menu invoke $label]
	}
    }
    

    # set the text of the short help message (display now)
    # Note: embedded bitmaps can be specified as follows:
    # 
    # short_help "some text {bitmap mybitmap} other text ..."
    #
    # The optional mf arg is the "%m" value for the Enter/Leave event.
    # I don't see why it is needed:
    # pbiereic says: no display after button-1 press (makes double-press impossible)
    # XXX need to check why this should be (maybe Peter has different bindings...).

    method short_help {msg {mf ""}} {
	if {[info exists itk_component(short_help)]} {
	    if {"$mf" == "NotifyGrab" || "$mf" == "NotifyUngrab"} {
		return
	    }
	    set w $itk_component(short_help)
	    $w config -state normal
	    $w delete 1.2 end
	    set n 0
	    foreach i $msg {
		if {"[lindex $i 0]" == "bitmap" && [llength $i] == 2} {
		    $w window create end \
			-window [label $w.l[incr n] \
				     -background $bitmap_bg_ \
				     -foreground $bitmap_fg_ \
				     -padx 1m \
				     -bitmap [lindex $i 1]]
		} else {
		    $w insert end "$i "
		}
	    }
	    $w config -state disabled
	}
    } 

    
    # specify a command to be evaluated for each TopLevelWidget

    proc set_command {cmd} {
	set command_ $cmd
    }
    

    # Check for the existance of a plug-in for this class.  For any
    # TopLevelWodget class Foo, a plug-in Tcl source file may be
    # specified by the environment variable $FOO_PLUGIN. The file
    # should define a Tcl proc named "Foo_plugin".
    #
    # If the environment variable is defined, source the file. In any
    # case, if the plug-in proc is defined, call it, passing it the
    # name of this instance.

    method check_plugin {} {
	global ::env ::auto_path
	set name [info namespace tail [$this info class]]
	set procname ${name}_plugin
	
	if {! [llength [info procs $procname]]} {
	    set var [string toupper $procname]
	    if {[info exists env($var)]} {
		set file $env($var)
		if {[file exists $file]} {
		    lappend auto_path [file dirname $file]
		    source $file
		}
	    } 
	}
	
	uplevel "#0" TopLevelWidget::call_plugin $procname $this
    }


    # call the given proc and pass it the given instance name.
    # $file is the file that is supposed to have contained $procname.

    proc call_plugin {procname inst} {
	if {[llength [info procs $procname]]} {
	    $procname $inst
	} 
    }


    # Return a list of top level windows that are children of $w, not
    # including $this widget.

    method list_windows {{w .}} {
	set list {}
	foreach c [winfo children $w] {
	    if {"$c" != "$w_" && "[winfo manager $c]" == "wm"} {
		lappend list $c
		foreach gc [list_windows $c] {
		    lappend list $gc
		}
	    }
	}
	return $list
    }


    # Toggle the visibility of all popup windows (except this one).
    # The trace variable name is passed as an argument here. if 1, 
    # hide the windows, otherwise restore them again.

    method hide_windows {variable} {
	global ::$variable
	if {[set $variable]} {
	    set windows [list_windows]
	    set popup_windows_ {}
	    foreach w  $windows {
		if {[winfo ismapped $w]} {
		    lappend popup_windows_ $w
		    wm withdraw $w
		}
	    }
	} else {
	    foreach w $popup_windows_ {
		if {[winfo exists $w]} {
		    wm deiconify $w
		}
	    }
	}
    }

    # show the help file associated with this window.
    public method show_help {file} {
       if { $file != {} } {
          if { ! [winfo exists $helpwindow_] } {
	     set helpwindow_ [StarHelp \#auto -file $file]
          }
	  $helpwindow_ display
       }
    }



    # -- public variables --

    # true if running as separate process
    itk_option define -standalone standalone Standalone {0}

    # if true, center the application window on startup
    itk_option define -center center Center {1} {
	if {$itk_option(-center)} {
	    global toplevel_position_
	    if {! [info exists toplevel_position_($w_)]} {
		wm withdraw $w_
		after 0 [code $this center_window]
	    }
	}
    }

    # if true, withdraw the application window on startup
    itk_option define -withdraw withdraw Withdraw {0} {
	if {$itk_option(-withdraw)} {
	    wm withdraw $w_
	}
    }

    # if true and this is a child of another TopLevel widget,
    # make the toplevel window transient - so that it will
    # open and close with its parent
    itk_option define -transient transient Transient {1} {
	set w [winfo toplevel [winfo parent $w_]]
	if {"$w" != "." && $itk_option(-transient)} {
	    wm transient $w_ $w
	} 
    }

    # optionally specify a different TopLevelWidget to display short help messages
    itk_option define -shorthelpwin shortHelpWin ShortHelpWin {} {
	set short_help_win_ $itk_option(-shorthelpwin)
	if {"$short_help_win_" == ""} {
	    set short_help_win_ $this
	}
    }


    # -- private and protected variables --

    # bitmap colors for short help area
    protected variable bitmap_bg_ white
    protected variable bitmap_fg_ gray30
    
    # shorter name for $itk_component(hull)
    protected variable w_

    # array(menu,menu-label) of help text for menu items
    protected variable menu_item_help_

    # optional TopLevelWidget to display short help text (default: $this)
    protected variable short_help_win_ {}

    # count used for busy cursor
    private variable busy_count_ {0}

    # name of help button.
    protected variable helpbutton_ ""

    # help window
    protected variable helpwindow_ {}


    # saved list of popup window names for "hide_windows" method
    protected variable popup_windows_ {}

    # -- common variables (once per class) --

    # optional tcl command to eval for each TopLevelWidget created
    common command_ {}
}

