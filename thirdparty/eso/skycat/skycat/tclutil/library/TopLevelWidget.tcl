# E.S.O. - VLT project/ ESO Archive
# "@(#) $Id: TopLevelWidget.tcl,v 1.26 1998/11/26 22:39:09 abrighto Exp $"
#
# TopLevelWidget.tcl - Itk base class for popup windows
#
# See the man page for a complete description.
#
# who             when       what
# --------------  ---------  ----------------------------------------
# Allan Brighton  01 Jun 94  Created
#
# P.W. Draper     04 Jul 96  Added -takefocus to help text.
#                 10 Dec 96  Added configure_menubutton.
#                 20 Jan 98. Fixed busy focus -lastfor so
#                            that it does restore the focus,
#                            not just get the window name. 
#                 10 Mar 99  Added fix to split up args in
#                            setup_menuitem. This makes the
#                            accelerator code work.

itk::usual TopLevelWidget {}

# The TopLevelWidget itcl class is a subclass of itk::Toplevel and thus
# inherits all of the features described in Toplevel(n). In addition, a
# number of useful methods are defined, for use by the derived classes.

itcl::class util::TopLevelWidget {
    inherit itk::Toplevel

    # Create a toplevel widget with the same name as this object.

    constructor {args} {
	set class_ [utilNamespaceTail [$this info class]]
	set w_ $itk_component(hull)
	wm iconname $w_ $class_

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
	catch {set toplevel_position_($w_) "+[winfo x $w_]+[winfo y $w_]"}
    }


    # this method is called after all options have been evaluated and is meant
    # to be redefined in a derived class

    protected method init {} {
    }


    # call the init method and then check if there is a plugin for this class.

    protected method call_init {} {
	init
	check_plugins $class_
	call_plugin_procs $class_
    }


    # Start an itcl application - that is, an application with an itcl
    # TopLevel widget as its main window. 
    # This proc assumes that the command line args (global: $argv) 
    # should be passed on to the class (which is derived from this class).
    # If any errors occur, we print a usage message based on the given
    # usage argument, or if that is empty, a default message.
    #
    # If default_opt is specified, args without options are treated as
    # belonging to that option (-file, for example)
    #
    # an optional "usage" message may also be specified, to be printed
    # when unknown options are found.
    #
    # If "name" is not specified, the class name in lower case is used with
    # a "." prepended and the clone number appended (1, 2, 3, ...).

    public proc start {class {default_opt ""} {usage ""} {name ""}} {
	global ::argv ::argc ::mainclass ::tcl_version

	# hide the "." window since an itcl class will replace it
	wm withdraw .

	# Check for an application plugin and allow it to define a subclass
	# of $class and set the global var $mainclass to indicate it.
	set mainclass $class
	check_plugins $class

	# if the name of the main class changed, we have to rearrange the
	# plugin info...
	if {"$class" != "$mainclass"} {
	    if {[info exists plugin_procs_($class)]} {
		set plugin_procs_($mainclass) $plugin_procs_($class)
	    }
	}

	# get the derived class name from the calling scope
	set number [incr clone_cnt_]
	if {"$name" == ""} {
	    set name ".[utilNamespaceTail [string tolower $mainclass]]$number"
	}

	# handle default arguments, such as "$file" instead of "-file file"
	set default_arg {}
	set cmd "$mainclass $name -standalone 1 -center 0 -number $number" 
	for {set i 0} {$i < $argc} {incr i} {
	    set opt [lindex $argv $i]
	    if {"[string index $opt 0]" == "-" && "$opt" != "-"} {
		set arg [lindex $argv [incr i]]
	    } else {
		if {"$default_opt" == "" || "$default_arg" != ""} {
		    start_err_ $mainclass $name $default_opt "invalid option $opt" $usage
		}
		set arg [set default_arg $opt]
		set opt $default_opt
	    }
	    lappend cmd $opt $arg
	}

	# create the object 
	if {$tcl_version >= 8} {
	    set status [catch $cmd msg]
	} else {
	    set status [catch "@scope :: $cmd" msg]
	}
	if {$status} {
	    start_err_ $mainclass $name $default_opt $msg $usage
	}
	
	# keep track of main windows
	set main_windows_(.) 0

	# exit when all toplevel window based classes exit - don't wait for "."
	if {[winfo exists $name]} {
	    set main_windows_($name) 1
	    tkwait window $name
	    unset main_windows_($name)
	}
	if {[llength [array names main_windows_]] == 1} {
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

    private proc start_err_ {class name {default_opt ""} {msg ""} {usage ""}} {
	global ::argv0 ::errorInfo

	set prog [file rootname [file tail $argv0]]
	if {[string match "unknown option*" $msg]} {
	    if {! [string match "unknown option \"--help\"" $msg]} {
		puts stderr "$prog: $msg"
	    }
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

    public method quit {} {
	if {$itk_option(-standalone)} {
	    destroy $w_
	} else {
	    wm withdraw $w_
	}
    }
	    
	    
    # run the given tcl command in the scope of this class
    # while displaying the (blt) busy cursor in the toplevel
    # window

    public method busy {cmd} {
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

    public method test {cmd} {
	if {[catch [list uplevel $cmd] msg]} {
	    puts stderr "test: $msg"
	    error $msg
	}
    }


    # add a menubar to the main frame

    public method add_menubar {} {
	# Optional menubar added to top of window.
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
    # If side is specified, the menu button is placed on the given side.
    # The return value if the name of the menubutton's menu.

    public method add_menubutton {label {helptext ""} {side left}} {
	set f $itk_component(menubar)
	set name [string tolower $label]
	set mb $f.$name
	set m $mb.m

	if {[winfo exists $m]} {
	    destroy $m
	} else {
	    # Menubutton component. $name is the same as the menubutton label,
	    # but in lower case.
	    itk_component add $name {
		menubutton $mb \
		    -text $label \
		    -menu $m \
		    -underline 0
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

    public method get_menubutton {label} {
	set f $itk_component(menubar)
	set name [string tolower $label]
	return $f.$name
    }


    # return the path name of the menu for the given menubutton label

    public method get_menu {label} {
	return [get_menubutton $label].m
    }


    # configure something of the named menubutton.

    public method configure_menubutton {label args} {
	set w [get_menubutton $label]
	if {[winfo exists $w]} {
	    eval $w configure $args
	}
    }


    # if any help has been defined then add a button to show it

    public method add_help_button {file label {msg ""}} {
	if { $help_button_menu_ == "" } { 
	    set help_button_menu_ [add_menubutton "Help" right]
	}
	$help_button_menu_ add command -label "$label" \
	    -command [code $this show_help $file]
	configure_menubutton Help -underline 0
	if { $msg != "" } {  
	    set menu_item_help_($help_button_menu_,$label) $msg
	}
	return $help_button_menu_
    }


    # show the help file associated with this window.

    public method show_help {file} {
	if { $file != {} } {
	    if { ! [winfo exists $help_window_] } {
		set help_window_ [util::HelpWin \#auto -file $file]
	    }
	    $help_window_ display
	}
    }


    # called for motion events in menubar menus for short help use

    protected method menu_motion {m y} {
	if {[catch {$short_help_win_ short_help $menu_item_help_($m,[$m entrycget @$y -label])}]} {
	    short_help {}
	}
    }


    # Center this window on the screen. 
    # this doesn't work with gridded geometry...

    public method center_window {} {
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

    public method make_short_help {{side bottom}} {
	# Optional short help window at bottom.
	itk_component add short_help {
	    text $w_.shelp \
		-borderwidth 3 \
		-height 1 \
		-width 1 \
		-spacing1 3 \
		-wrap none \
                -takefocus 0 \
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

    public method add_short_help {w msg} {
	if {[winfo exists $w]} {
	    bind $w <Enter> "+[code $short_help_win_ short_help $msg %m]"
	    bind $w <Leave> "+[code $short_help_win_ short_help {} %m]"
	}
    } 

    
    # set the text of the short help message to be displayed whenever
    # the mouse enters the menu item with the given label (see short_help below)

    public method add_menu_short_help {menu label msg} {
	set menu_item_help_($menu,$label) $msg
    } 

    
    # add a menu item of the given type to the given menu and arrange to have 
    # the given short help message displayed when the mouse is over the item.
    # The extra args are passed to the "$menu add" command.

    public method add_menuitem {menu type label msg args} {
	eval [concat [list $menu add $type -label $label] $args]
	setup_menuitem $menu $label $msg $args
    }
    

    # Insert a menu item of the given type in the given menu before the given index 
    # and arrange to have the given short help message displayed when the mouse is 
    # over the item. The extra args are passed to the "$menu insert" command.

    public method insert_menuitem {menu index type label msg args} {
	eval [concat [list $menu insert $index $type -label $label] $args]
	setup_menuitem $menu $label $msg $args
    }

    
    # Local method to set up the menu accelerator, help text, and command bindings.

    protected method setup_menuitem {menu label msg args} {
	set menu_item_help_($menu,$label) $msg

        # PWD: modification here. Need to split up $args, so use largs.
	# if -accelerator was specified, add key binding for command
        eval set largs $args
	set n [llength $largs]
	set key {}
	for {set i 0} {$i < $n} {incr i 2} {
	    set opt [lindex $largs $i]
	    set arg [lindex $largs [expr $i+1]]
	    if {"$opt" == "-accelerator"} {
		set key $arg
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

    protected method short_help {msg {mf ""}} {
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

    public proc set_command {cmd} {
	set command_ $cmd
    }
    

    # Check for the existance of any plugins for the given class.  For
    # any TopLevelWidget class Foo, a plug-in Tcl source file may be
    # specified by the environment variable $FOO_PLUGIN, which may
    # contain a colon separated list of full path names to plugin
    # files or directories. Each plugin file may define a Tcl proc
    # named "Foo_plugin". There may be multiple such procs, so they
    # are renamed to Foo_plugin1, Foo_plugin2, etc., after the file is
    # sourced. The plugin proc is called once after each instance of
    # the given class is created, with the name of the instance as an 
    # argument.
    #
    # Plugin files for an application class (a class that is started
    # with the util::TopLevelWidget::start proc) are sourced before the
    # first class instance is created, so that the plugin can derive a
    # subclass and set the global var "mainclass" to indicate that it
    # be used instead. If a plugin proc is defined, it is called after
    # the instance is created.
    #
    # If the plugin directory is specified, the filename is assumed to
    # be <classname>_plugin.tcl or <dir>/<classname>_plugin.tcl, for
    # example Foo_plugin.tcl or mydir/Foo_plugin.tcl.
    #
    # Rather than defining an environment variable for each class, it
    # is also possible to define one environment variable for the
    # application, "<appname>_plugin", to a certain file or a
    # directory containing multiple plugins or subdirectories with
    # plugins. In this way, installing a plugin involves only copying
    # the file or directory to the proper place. The application name
    # is defined by the Tk command [winfo name .].
    #
    # This proc only sources the plugin files, but does not call the
    # plugin procs. That is done by the "call_plugin_procs" method.

    protected proc check_plugins {classname} {
	global ::env ::auto_path
	set appname [winfo name .]
	set classname [utilNamespaceTail $classname]
	set procname ${classname}_plugin
	
	if {! [info exists checked_plugins_($classname)]} {
	    set checked_plugins_($classname) 1
	    set plugin_list {}
	    set var [string toupper $procname]
	    if {[info exists env($var)]} {
		set plugin_list $env($var)
	    } else {
		set var "[string toupper $appname]_PLUGIN"
		if {[info exists env($var)]} {
		    set plugin_list $env($var)
		}
	    }
	    set n 0
	    foreach i [split $plugin_list :] {
		if {[file isdir $i]} {
		    set files [glob -nocomplain $i/$procname.tcl $i/*/$procname.tcl]
		} else {
		    set files $i
		}
		foreach file $files {
		    if {[file exists $file]} {
			lappend auto_path [file dirname $file]
			if {! [info exists sourced_plugin_file_($file)]} {
			    set sourced_plugin_file_($file) 1
			    uplevel "#0" source $file
			}
			if {[llength [uplevel "#0" info procs $procname]] == 0} {
			    continue
			}
			# rename the plugin proc to allow more than one of them
			uplevel "#0" rename $procname $procname[incr n]
			lappend plugin_procs_($classname) $procname$n
		    }
		}
	    }
	} 
    }


    # Call each of the plugin procs for the given class, as found by the 
    # check_plugins proc above, passing the the name of this instance
    # as an argument. $classname should be the name of this class.

    protected method call_plugin_procs {classname} {
	if {[info exists plugin_procs_($classname)]} {
	    foreach procname $plugin_procs_($classname) {
		call_plugin_proc $procname $this
	    }
	}
    }


    # call the given proc and pass it the given instance name.
    # $file is the file that is supposed to have contained $procname.

    protected proc call_plugin_proc {procname inst} {
	if {[llength [uplevel "#0" info procs $procname]]} {
	    uplevel "#0" $procname $inst
	} 
    }


    # Return a list of top level windows that are children of $w.

    public method list_windows {{w .}} {
	set list {}
	foreach c [winfo children $w] {
	    if {"[winfo manager $c]" == "wm"} {
		lappend list $c
	    }
	    foreach gc [list_windows $c] {
		lappend list $gc
	    }
	}
	return $list
    }


    # Toggle the visibility of all popup windows (except this one).
    # The trace variable name is passed as an argument here. if 1, 
    # hide the windows, otherwise restore them again.

    public method hide_windows {variable} {
	global ::$variable
	if {[set $variable]} {
	    set windows [list_windows]
	    set popup_windows_ {}
	    foreach w  $windows {
		if {"$w" != "$w_" && [winfo ismapped $w]} {
		    lappend popup_windows_ $w
		    catch {wm withdraw $w}
		}
	    }
	} else {
	    foreach w $popup_windows_ {
		if {[winfo exists $w]} {
		    catch {wm deiconify $w}
		}
	    }
	}
    }
  

    # -- public variables --

    # true if running as separate process
    itk_option define -standalone standalone Standalone {0}

    # Set an optional unique instance or clone number for this window.
    # If this is not set, it is taken from the main window name,
    # which should be ".<appname><number>".
    itk_option define -number number Number {} {
	if {"$itk_option(-number)" == ""} {
	    set appname [string tolower [winfo name .]]
	    if {! [regsub "(\.$appname)(\[0-9\]+)\.(.*)" $w_ {\2} itk_option(-number)]} {
		set itk_option(-number) 1
	    }
	}
    }

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

    # name of this (derived) class
    protected variable class_

    # array(menu,menu-label) of help text for menu items
    protected variable menu_item_help_

    # optional TopLevelWidget to display short help text (default: $this)
    protected variable short_help_win_ {}

    # name of help button.
    protected variable help_button_menu_ ""

    # help window
    protected variable help_window_ {}

    # count used for busy cursor
    private variable busy_count_ {0}

    # saved list of popup window names for "hide_windows" method
    protected variable popup_windows_ {}

    # -- common variables (once per class) --

    # optional tcl command to eval for each TopLevelWidget created
    protected common command_ {}

    # array(name) of main, application level top level windows
    protected common main_windows_

    # array(classname) of list of plugin procs to call for each instance
    protected common plugin_procs_

    # array(classname) of bool: true if we checked for plugins already for the class
    protected common checked_plugins_

    # array(filename) of bool: true if we sourced the plugin file
    protected common sourced_plugin_file_

    # clone number of this instance (for main windows)
    protected common clone_cnt_ 0
}

