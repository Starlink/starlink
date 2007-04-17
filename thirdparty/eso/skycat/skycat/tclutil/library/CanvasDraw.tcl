# E.S.O. - VLT project/ ESO Archive
# "@(#) $Id: CanvasDraw.tcl,v 1.1.1.1 2006/01/12 16:40:45 abrighto Exp $"
#
# CanvasDraw.tcl - Add interactive drawing capabilities to a Tk canvas.
#
# usage: CanvasDraw .draw -canvas $canvas ?options...?
#
# See the man page CanvasDraw(n) for a complete description.
#
# who             when       what
# --------------  ---------  ----------------------------------------
# Allan Brighton  01 Jun 95  Created
#                 23 Jun 96  constrain (clip) interactive drawing with -bbox option
#                 20 Mar 98  Use init method (easier to subclass)
#                            Added -clipping option to control clipping of graphics
#                            (default is the same as before).

itk::usual CanvasDraw {}

# CanvasDraw is an [incrTcl] widget class that an application
# can use to add interactive drawing capabilities to any Tk
# canvas.  Since it is a subclass of TopLevelWidget, it creates
# its own toplevel window containing a drawing mode selection
# section and an option section for setting colors, fonts, line
# thickness, arrow types, etc. The window can be hidden on
# startup by specifying the "-withdraw 1" option inherited from
# TopLevelWidget.

itcl::class util::CanvasDraw {
    inherit util::TopLevelWidget

    # create an instance of this class

    constructor {args} {
	eval itk_initialize $args
    }

    
    # This method is called after the constructor have completed.
    protected method init {} {
	if {!$itk_option(-withtoolbox)} {
	    return
	}

	wm title $w_ "Line Graphics ($itk_option(-number))"
	wm iconname $w_ Graphics

	# make the toolbox
	make_drawmode_frame
	make_options_frame

	# create the canvas item menu
	make_object_menu

	$canvas_ config -cursor [set prev_cursor_ $itk_option(-defaultcursor)]
	
	# add button frame and buttons

	# Button frame at bottom of window
	itk_component add bot {
	    frame $w_.bot -borderwidth 2 -relief groove
	}
	pack $itk_component(bot) -side bottom -fill x

	# Close button
	itk_component add close {
	    button $itk_component(bot).close -text Close \
		-command "wm withdraw $w_"
	}
	# Clear button
	itk_component add clear {
	    button $itk_component(bot).clear -text Clear \
		-command [code $this clear]
	}
	# Delete button
	itk_component add delete {
	    button $itk_component(bot).delete -text Delete \
		-command [code $this delete_selected_objects]
	}
	pack $itk_component(close) $itk_component(clear) $itk_component(delete) \
	    -side left -padx 2m -pady 2m -expand 1

	add_short_help $itk_component(close) "Close this window"
	add_short_help $itk_component(clear) "Clear all line graphics"
	add_short_help $itk_component(delete) "Delete selected line graphic objects"

	# short help for selection grips
	if {$itk_option(-withrotate)} {
	    set msg "Selection grips: {bitmap dragb1} = resize object, \
                                  Control {bitmap dragb3} = rotate object "
	} else {
	    set msg "Selection grips: {bitmap dragb1} = resize object"
	}
	$canvas_ bind grip <Enter> "+[code $short_help_win_ short_help $msg]"
	$canvas_ bind grip <Leave> "+[code $short_help_win_ short_help {} ]"
    }

    
    # destructor: clean up
    
    destructor {
	catch {clear}
	catch {destroy $object_menu_}
    }
    
    
    # create the canvas item menu.

    protected method make_object_menu {} {
	# create menu in global namespace to avoid problems with tk menu utils
	set cmd [list menu $canvas_.objmenu -tearoff 0]
	set m [set object_menu_ [uplevel "#0" $cmd]]
	$m add command -label "Delete Selected Items" \
	    -command [code $this delete_selected_objects]
    }

    
    # post the object menu at the mouse cursor position.
    # If an item is selected, post the menu with the items enabled.
    
    protected method post_object_menu {x y} {
	# disable items if no selection...
	if {! [lempty [selected_items]]} {
	    tk_popup $object_menu_ $x $y
	}
    }


    # delete the selected canvas items

    public method delete_selected_objects {} {
	foreach id [selected_items] {
	    delete_object $id
	}
    }

    # delete the given canvas item

    public method delete_object {id} {
	deselect_object $id
	$canvas_ delete $id
	if {[info exists notify_($id)]} {
	    set cmd $notify_($id)
	    unset notify_($id)
	    eval "$cmd delete"
	}
	if {[info exists notify_update_($id)]} {
	    unset notify_update_($id)
	}
    }


    # delete all canvas items

    public method clear {} {
	deselect_objects
	$canvas_ delete $w_.objects
	$canvas_ delete objects
    }


    # create the menu items in the given menu

    public method add_menuitems {m} {
	# save menu name for enable/disable
	set menu_ $m

	# add the menus
	foreach i {Mode Width Arrow ArrowShape Fill Outline Stipple Font Smooth} {
	    $m add cascade -label $i -menu [menu $m.[string tolower $i]]
	}
	
	# drawing mode menu
	foreach i $drawing_modes_ {
	    $m.mode add radiobutton \
		-variable $w_.mode -value $i \
		-bitmap $i \
		-command [code $this set_drawing_mode $i]
	}
	after idle [code $this set_drawing_mode anyselect]

	# line width menu
	foreach i {1 2 3 4} {
	    $m.width add radiobutton \
		-variable $w_.width \
		-value $i \
		-bitmap width$i \
		-command [code $this set_linewidth $i]
	}

	# arrow  menu
	foreach i {none first both last} {
	    $m.arrow add radiobutton \
		-variable $w_.arrow \
		-bitmap arrow$i \
		-value $i \
		-command [code $this set_arrowtype $i]
	}

	# arrow shape  menu (bitmap names corr. to arrow shape lists)
	foreach i $arrowshapes_ {
	    $m.arrowshape add radiobutton \
		-variable $w_.arrowshape \
		-value $i \
		-bitmap ar[join $i _] \
		-command [list $this set_arrowshape $i]
	}

	# fill  menu
	$m.fill add radiobutton \
	    -variable $w_.fill -value "" \
	    -label None \
	    -command [code $this set_fillcolor {} ]
	foreach i $itk_option(-colors) {
	    $m.fill add radiobutton \
		-variable $w_.fill -value $i \
		-command [code $this set_fillcolor $i] \
		-background $i
	}

	# outline  menu
	foreach i $itk_option(-colors) {
	    $m.outline add radiobutton \
		-variable $w_.outline -value $i \
		-command [code $this set_outlinecolor $i] \
		-background $i
	}
	$m.outline add radiobutton \
	    -variable $w_.outline -value "" \
	    -label None \
	    -command [code $this set_outlinecolor {} ]

	# Stipple  menu
	for {set i 0} {$i < 16} {incr i} {
	    set bitmap pat$i
	    $m.stipple add radiobutton \
		-variable $w_.stipple -value pat$i \
		-bitmap $bitmap \
		-command [code $this set_stipplepattern $i]
	}

	# Font  menu
	foreach i $itk_option(-fonts) {
	    if {[catch {
		$m.font add radiobutton \
		    -variable $w_.font -value $i \
		    -label {abc} \
		    -command [code $this set_textfont $i] \
		    -font $i
	    } msg]} {
		puts "warning: $msg"
	    }
	}
	
	# Smooth  menu
	set bool 0
	foreach i {rectangle smooth} {
	    $m.smooth add radiobutton \
		-bitmap $i \
		-value $bool \
		-variable $w_.smooth \
		-command [code $this set_smooth $bool]
	    incr bool
	}
	set $w_.smooth 0

	# add items for deleting objects
	$m add separator
	$m add command -label "Clear" -command [code $this clear]
	$m add command -label "Delete" -command [code $this delete_selected_objects]

	# add short help texts for menu items
	set w $short_help_win_
	$w add_menu_short_help $m Mode {Select the drawing mode}
	$w add_menu_short_help $m Width {Set the line width for drawing}
	$w add_menu_short_help $m Arrow {Select the arrow mode for lines}
	$w add_menu_short_help $m ArrowShape {Select the arrow shape for lines}
	$w add_menu_short_help $m Fill {Select the fill color for drawing}
	$w add_menu_short_help $m Outline {Select the outline color for drawing}
	$w add_menu_short_help $m Stipple {Select the stipple pattern for filling objects}
	$w add_menu_short_help $m Font {Select the font to use for labels}
	$w add_menu_short_help $m Clear {Delete graphic objects}
	$w add_menu_short_help $m Delete {Delete selected graphic objects}
	$w add_menu_short_help $m Smooth {Set the smooth option for drawing polygons}
    }



    # enable or disable the drawing items in the menu 
    # (enable is state is "normal", disable if "disabled".

    public method set_menu_state {state} {
	if {"$menu_" != ""} {
	    for {set i 0} {$i < 16} {incr i} {
		catch {$menu_ entryconfigure $i -state $state}
	    }
	}
    }


    # create the frame for selecting draw types  (line, oval, ...)
    # in the given frame

    protected method make_drawmode_frame {} {
	# Frame containing drawing mode buttons.
	itk_component add drawmodes {
	    frame $w_.drawmodes -borderwidth 6
	}
	pack $itk_component(drawmodes) -side top -fill both
	
	set row 0
	set col 0
	set maxcol 3

	foreach drawing_mode $drawing_modes_ {
	    # Drawing mode buttons 
	    # (anyselect, region, line, rectangle, oval, polyline, polygon, text)
	    itk_component add $drawing_mode {
		set b [button $itk_component(drawmodes).$drawing_mode \
			   -bitmap $drawing_mode \
			   -borderwidth 3 \
			   -command [code $this set_drawing_mode $drawing_mode]]
	    }
	    add_short_help $b "set the drawing mode to $drawing_mode"
	    blt::table $itk_component(drawmodes) $b $row,$col \
		-fill x -ipadx 1m -ipady 1m
	    if {$col < $maxcol} {
		incr col
	    } else {
		set col 0
		incr row
	    }
	}
	set_drawing_mode anyselect
    }


    # create the draw options frame (line width, color, etc...)

    protected method make_options_frame {} {
	global ::$w_.width ::$w_.arrow ::$w_.arrowshape ::$w_.fill ::$w_.outline \
	    ::$w_.stipple ::$w_.font ::$w_.smooth

	# Frame containing option menubuttons for graphic items
	itk_component add options {
	    set f [frame $w_.options -borderwidth 6]
	}
	pack $itk_component(options) -side top -fill both

	foreach i {Width Arrow ArrowShape Fill Outline Stipple Font Smooth} {
	    set menu [string tolower $i]
	    # LabelMenu(n) items for changing options on graphic objects.
	    # (Width Arrow ArrowShape Fill Outline Stipple Font Smooth)
	    itk_component add $menu {
		util::LabelMenu $f.$menu \
		    -text "$i:" \
		    -relief raised \
		    -variable $w_.$menu \
		    -labelwidth 15
	    }
	    pack $itk_component($menu) -side top -fill both -expand 1 -ipadx 1m -ipady 1m
	}

	# line width menu
	foreach i {1 2 3 4} {
	    $f.width add \
		-bitmap width$i \
		-value $i \
		-command [code $this set_linewidth $i]
	}
	set $w_.width 1
	add_short_help $f.width "set the line width for drawing"

	# arrow  menu
	foreach i {none first both last} {
	    $f.arrow add \
		-bitmap arrow$i \
		-value $i \
		-command [code $this set_arrowtype $i]
	}
	set $w_.arrow none
	add_short_help $f.arrow "set the arrow type for drawing lines"

	# arrow shape  menu
	foreach i $arrowshapes_ {
	    $f.arrowshape add  \
	 	-bitmap ar[join $i _] \
		-value $i \
		-command [list $this set_arrowshape $i]
	}
	set $w_.arrowshape [lindex $arrowshapes_ 0]
	add_short_help $f.arrowshape \
	    "set the arrow shape for drawing (must also select type above)"

	# fill  menu
	$f.fill add \
	    -label None \
	    -command [code $this set_fillcolor {} ] \
	    -background [. cget -background]
	foreach i $itk_option(-colors) {
	    $f.fill add \
		-command [code $this set_fillcolor $i] \
		-label {    } \
		-value $i \
		-background $i
	}
	set $w_.fill None
	add_short_help $f.fill "set the fill color for drawing"

	# outline  menu
	foreach i $itk_option(-colors) {
	    $f.outline add \
		-command [code $this set_outlinecolor $i] \
		-label {    } \
		-value $i \
		-background $i
	}
	$f.outline add \
	    -label None \
	    -command [code $this set_outlinecolor {} ] \
	    -background [. cget -background]
	set $w_.outline white
	add_short_help $f.outline "set the outline color for drawing"

	# Stipple  menu
	for {set i 0} {$i < 16} {incr i} {
	    set bitmap pat$i
	    $f.stipple add \
		-bitmap $bitmap \
		-command [code $this set_stipplepattern $i]
	}
	set $w_.stipple pat0
	add_short_help $f.stipple "set the stipple pattern for drawing"

	# Font  menu
	foreach i $itk_option(-fonts) {
	    if {[catch {
		$f.font add \
		    -label {abc} \
		    -value $i \
		    -command [code $this set_textfont $i] \
		    -font $i
	    } msg]} {
		puts "warning: $msg"
	    }
	}
	set $w_.font abc
	add_short_help $f.font "set the font used for text items"

	# Smooth  menu
	set bool 0
	foreach i {rectangle smooth} {
	    $f.smooth add \
		-bitmap $i \
		-value $bool \
		-command [code $this set_smooth $bool]
	    incr bool
	}
	set $w_.smooth 0
	add_short_help $f.smooth "set the smooth option for drawing"
    }

        
    # update the display to show the options for the given canvas item

    protected method update_display_from_object {id} {
	global ::$w_.width ::$w_.arrow ::$w_.arrowshape ::$w_.fill ::$w_.outline \
	    ::$w_.stipple ::$w_.font ::$w_.smooth

	
	foreach i [$canvas_ itemconfig $id] {
	    lassign $i opt {} {} {} value
	    set var "$w_.[string range $opt 1 end]"
	    set $var $value
	}
    }


    # apply the given option to the selected canvas items

    public method config_selected {option value} {
	foreach i [selected_items] {
	    if {[info exists option_map_($i,$option)]} {
		set option $option_map_($i,$option)
	    } 
	    catch {$canvas_ itemconfig $i $option $value}
	}
    }

    
    # set the line width default and for any selected objects

    public method set_linewidth {w} {
	global ::$w_.width
	set $w_.width $w
	config -linewidth $w
	config_selected -width $w
    }

    
    # set the arrow type default and for any selected objects

    public method set_arrowtype {t} {
	global ::$w_.arrow
	config -arrowtype $t
	set $w_.arrow $t
	config_selected -arrow $t
    }


    # set the arrow shape default and for any selected objects

    public method set_arrowshape {list} {
	global ::$w_.arrowshape
	config -arrowshape $list
	set $w_.arrowshape $list
	config_selected -arrowshape $list
    }


    # set the fill color default and for any selected objects
    # note: if the stipple is invisible (see through), make it solid to
    # make sure fill is visible

    public method set_fillcolor {c} {
	global ::$w_.fill
	if {$stipple_index_ == 8} {
	    set_stipplepattern 0
	} elseif {"$c" == ""} {
	    set_stipplepattern 8
	}
	config -fillcolor [set $w_.fill $c]
	config_selected -fill $c
    }


    # set the outline color default and for any selected objects

    public method set_outlinecolor {c} {
	global ::$w_.outline
	config -outlinecolor [set $w_.outline $c]
	config_selected -outline $c
    }


    # set the stipple pattern default and for any selected objects
    # index is the pattern number (corresponding to pat$index.xbm)

    public method set_stipplepattern {index} {
	global ::$w_.stipple
	config -stipplepattern [set $w_.stipple pat$index]
	set stipple_index_ $index

	# make sure newly created lines are visible (pat8 is invisible)
	if {$index == 8} {
	    config -linestipple pat0
	} else {
	    config -linestipple $itk_option(-stipplepattern)
	}

	config_selected -stipple $itk_option(-stipplepattern)
    }


    # set the text font  default and for any selected objects

    public method set_textfont {f} {
	global ::$w_.font
	config -textfont $f
	set $w_.font $f
	config_selected -font $f
    }


    # set the smooth option for drawing 

    public method set_smooth {bool} {
	global ::$w_.smooth
	config -smooth $bool
	set $w_.smooth $bool
	config_selected -smooth $bool
    }


    # set the type of object to draw (line, oval, ...)
    # If create_cmd is specified, it is called with the coordinates
    # of the new object when its creation is completed

    public method set_drawing_mode {type {create_cmd ""}} {
	if {"$type" == "region" && $drawing_} {
	    # already drawing...
	    return
	}

	# reset selection
	foreach i $drawing_modes_ {
	    $itk_component($i) config -relief raised 
	}
	if {[info exists itk_component($type)]} {
	    $itk_component($type) config -relief sunken
	}

	# from menu's -variable option
	global ::$w_.mode
	set drawing_mode_ [set $w_.mode $type]
	set create_cmd_ $create_cmd

	set drawing_ [expr {"$type" != "objselect" && "$type" != "anyselect"} ]
	if {$drawing_} {
	    deselect_objects
	    $canvas_ config -cursor $itk_option(-drawcursor)
	} else {
	    reset_cursor
	}

	# reset canvas bindings based on drawing_mode
	bind $canvas_ <Motion> {}
	switch -exact $drawing_mode_ {
	    anyselect -
	    objselect {
		bind $canvas_ <1> [code $this check_deselect]
		bind $canvas_ <ButtonRelease-1> {}
		bind $canvas_ <B1-Motion> {}
		bind $canvas_ <Shift-B1-Motion> {}
		bind $canvas_ <Double-ButtonPress-1> {}
	    }
	    region {
		$canvas_ config -cursor $itk_option(-regioncursor)
		bind $canvas_ <1> \
		    [code eval $this create_object $canvasX_ $canvasY_]
		bind $canvas_ <ButtonRelease-1> \
		    [code eval $this check_done $canvasX_ $canvasY_ update_region]
		bind $canvas_ <B1-Motion> \
		    [code eval $this update_region $canvasX_ $canvasY_ 0]
		bind $canvas_ <Shift-B1-Motion> \
		    [code eval $this update_region $canvasX_ $canvasY_ 1]
		bind $canvas_ <Double-ButtonPress-1> \
		    [code eval $this create_done $canvasX_ $canvasY_]
	    }
	    line {
		bind $canvas_ <1> \
		    [code eval $this create_object $canvasX_ $canvasY_]
		bind $canvas_ <ButtonRelease-1> \
		    [code eval $this check_done $canvasX_ $canvasY_]
		bind $canvas_ <B1-Motion> \
		    [code eval $this update_line $canvasX_ $canvasY_]
		bind $canvas_ <Shift-B1-Motion> \
		    [code eval $this update_line $canvasX_ $canvasY_ 1]
		bind $canvas_ <Double-ButtonPress-1> \
		    [code eval $this create_done $canvasX_ $canvasY_]
	    }
	    oval -
	    arc {
		bind $canvas_ <1> \
		    [code eval $this create_object $canvasX_ $canvasY_]
		bind $canvas_ <ButtonRelease-1> \
		    [code eval $this check_done $canvasX_ $canvasY_]
		bind $canvas_ <B1-Motion> \
		    [code eval $this update_object $canvasX_ $canvasY_]
		bind $canvas_ <Shift-B1-Motion> \
		    [code eval $this update_object $canvasX_ $canvasY_ 1]
		bind $canvas_ <Double-ButtonPress-1> \
		    [code eval $this create_done $canvasX_ $canvasY_]
	    }
	    rectangle {
		bind $canvas_ <1> \
		    [code eval $this create_object $canvasX_ $canvasY_]
		bind $canvas_ <ButtonRelease-1> \
		    [code eval $this check_done $canvasX_ $canvasY_ update_rectangle]
		bind $canvas_ <B1-Motion> \
		    [code eval $this update_rectangle $canvasX_ $canvasY_]
		bind $canvas_ <Shift-B1-Motion> \
		    [code eval $this update_rectangle $canvasX_ $canvasY_ 1]
		bind $canvas_ <Double-ButtonPress-1> \
		    [code eval $this create_done $canvasX_ $canvasY_]
	    }
	    polyline -
	    polygon {
		bind $canvas_ <1> \
		    [code eval $this create_object $canvasX_ $canvasY_]
		bind $canvas_ <ButtonRelease-1> \
		    [code eval $this add_poly_point $canvasX_ $canvasY_]
		bind $canvas_ <B1-Motion> \
		    [code eval $this update_poly_object $canvasX_ $canvasY_]
		bind $canvas_ <Shift-B1-Motion> \
		    [code eval $this update_poly_object $canvasX_ $canvasY_]
		bind $canvas_ <Double-ButtonPress-1> \
		    [code eval $this create_done $canvasX_ $canvasY_]
	    }
	    freehand {
		bind $canvas_ <1> \
		    [code eval $this create_object $canvasX_ $canvasY_]
		bind $canvas_ <ButtonRelease-1> \
		    [code eval $this check_done $canvasX_ $canvasY_ update_freehand]
		bind $canvas_ <B1-Motion> \
		    [code eval $this update_freehand $canvasX_ $canvasY_]
		bind $canvas_ <Shift-B1-Motion> \
		    [code eval $this update_freehand $canvasX_ $canvasY_]
		bind $canvas_ <Double-ButtonPress-1> \
		    [code eval $this create_done $canvasX_ $canvasY_]
	    }
	    text -
	    label {
		$canvas_ config -cursor $itk_option(-textcursor)
		bind $canvas_ <1> \
		    [code eval $this create_object $canvasX_ $canvasY_]
		bind $canvas_ <ButtonRelease-1> \
		    [code eval $this create_text_done $canvasX_ $canvasY_]
		bind $canvas_ <B1-Motion> {}
		bind $canvas_ <Shift-B1-Motion> {}
		bind $canvas_ <Double-ButtonPress-1> {}
	    }
	}
    }

    
    # clip the given point to keep it over the drawing area

    public method clip {px py} {
	if {$itk_option(-clipping)} {
	    upvar $px x
	    upvar $py y
	    if {$x < $x0_} {
		set x $x0_
	    } elseif {$x > $x1_} {
		set x $x1_
	    }
	    if {$y < $y0_} {
		set y $y0_
	    } elseif {$y > $y1_} {
		set y $y1_
	    }
	}
    }


    # mark (remember) the current position

    protected method mark {x y} {
	clip x y
	set startx_ $x
	set starty_ $y
	set xoffset_ 0
	set yoffset_ 0
    }


    # set up the canvas object bindings. target_id defaults to id.
    # If target_id is specified, it will receive the events instead of id.

    public method add_object_bindings {id {target_id ""}} {
	if {"$target_id" == ""} {
	    set target_id $id
	}

	# mark pos
	$canvas_ bind $id <1> "+[code eval $this mark $canvasX_ $canvasY_]"
	$canvas_ bind $id <Control-Button-1> "+[code eval $this mark $canvasX_ $canvasY_]"
	$canvas_ bind $id <Shift-Button-1> "+[code eval $this mark $canvasX_ $canvasY_]"

	# select
	$canvas_ bind $id <ButtonRelease-1> \
	    "+[code eval $this select_only_object $target_id $canvasX_ $canvasY_]"
	$canvas_ bind $id <Control-ButtonRelease-1> \
	    "+[code eval $this toggle_select_object $target_id $canvasX_ $canvasY_]"
	$canvas_ bind $id <Shift-ButtonRelease-1> \
	    "+[code eval $this toggle_select_object $target_id $canvasX_ $canvasY_]"

	# move
	$canvas_ bind $id <B1-Motion> \
	    "+[code eval $this move_object $target_id $canvasX_ $canvasY_]"
	$canvas_ bind $id <Shift-B1-Motion> \
	    "+[code eval $this move_object $target_id $canvasX_ $canvasY_]"
	$canvas_ bind $id <Control-B1-Motion> \
	    "+[code eval $this move_object $target_id $canvasX_ $canvasY_]"
	$canvas_ bind $id <ButtonRelease-1> "+[code $this end_move $target_id]"
	$canvas_ bind $id <Control-ButtonRelease-1> "+[code $this end_move $target_id]"
	$canvas_ bind $id <Shift-ButtonRelease-1> "+[code $this end_move $target_id]"

	# cursor
	$canvas_ bind $id <Any-Enter> "+[code $this set_cursor tcross]"
	$canvas_ bind $id <Any-Leave> "+[code $this reset_cursor]"

	# menu
	if {$itk_option(-show_object_menu)} {
	    $canvas_ bind $id <3> "+[code $this post_object_menu %X %Y]"
	}
    }


    # Finish creation of the current object and call the "create" command, if one
    # was specified, otherwise, for region items, call the region command, if
    # defined.

    public method create_done {x y} {
	set resizing_ 0
	reset_cursor
	add_object_bindings $obj_id_
	if {"$create_cmd_" != ""} {
	    eval "$create_cmd_ $obj_id_ [$canvas_ bbox $obj_id_]"
	    set create_cmd_ ""
	} elseif {"$drawing_mode_" == "region"} {
	    set_drawing_mode anyselect
	    eval select_region [$canvas_ bbox $obj_id_]
	    delete_object $obj_id_
	    return
	}

	set_drawing_mode anyselect
	select_object $obj_id_
	update_display_from_object $obj_id_
	mark $x $y
    }


    # Finish creation of the current text object

    protected method create_text_done {x y} {
	set_drawing_mode anyselect
	reset_cursor
	add_object_bindings $obj_id_
    }

    
    # check if object creation is done: if the button has moved
    # since it was pressed, we are done, otherwise dragging continues
    # without the button pressed.
    #
    # update_method is the method to call to update the object, if needed

    protected method check_done {x y {update_method "update_object"}} {
	clip x y
	if {$x == $startx_ && $y == $starty_} {
	    bind $canvas_ <Motion> [code eval $this $update_method $canvasX_ $canvasY_]
	    bind $canvas_ <1> [code eval $this create_done $canvasX_ $canvasY_]
	} else {
	    create_done $x $y
	}
    }


    # This method is called when a region object has been created to select
    # a region of the canvas. 
    # All objects in the region are selected and if a region command option
    # was specified, the command is evaluated with the bounding box of the region.

    public method select_region {x0 y0 x1 y1} {
	foreach id [$canvas_ find enclosed $x0 $y0 $x1 $y1] {
	    # make sure its is one of our objects
	    if {[item_has_tag $id $w_.objects]} {
		select_object $id
	    }
	}

	if {"$itk_option(-regioncommand)" != ""} {
	    eval "$itk_option(-regioncommand) $x0 $y0 $x1 $y1"
	}
    }


    # add a point to the curent polygon/polyline

    protected method add_poly_point {x y} {
	clip x y
	lappend obj_coords_ $x $y
	update_poly_object $x $y
	bind $canvas_ <1> {}
	bind $canvas_ <Motion> [code eval $this update_poly_object $canvasX_ $canvasY_]
    }


    # If the mouse is not over an object, deselect all objects

    public method check_deselect {} {
	if {[llength [$canvas_ gettags current]] == 0} {
	    deselect_objects
	}
    }

    
    # if the object is selected, deselect it, otherwise select it

    protected method toggle_select_object {id x y} {
	# only if not moving mouse...
	if {$startx_ == $x && $starty_ == $y} {
	    if {[item_is_selected $id]} {
		deselect_object $id
	    } else {
		select_object $id 
		mark $x $y
	    }
	}
    }

    
    # select only the given object

    public method select_only_object {id x y} {
	# only if not moving mouse...
	if {$startx_ == $x && $starty_ == $y} {
	    deselect_objects
	    select_object $id 1
	    update_display_from_object $id
	}
	if {$drawing_} {
	    return
	}
	mark $x $y
    }


    # select the given object by drawing 8 little grips on it, or for text objects, 
    # if any==1 and the selection type is "anyselect", select the text in the object.
    # The grips have the tag "grip" and the object gets the tag "$w_.selected"

    public method select_object {id {any 0}} {
	if {$drawing_} {
	    return
	}
	
	# decide whether to select object or only text
	set type [$canvas_ type $id]
	if {$any && "$type" == "text" && "$drawing_mode_" == "anyselect"} {
	    $canvas_ focus $id
	    $canvas_ icursor $id end
	} elseif {$itk_option(-show_selection_grips)} {
	    draw_selection_grips $id $type
	}
	$canvas_ addtag $w_.selected withtag $id
    }


    # Draw the selection grips for a simple line.
    # Lines get one grip at each end.

    protected method draw_line_selection_grips {id} {
	foreach side {e w} {
	    set sel_id [$canvas_ create rectangle 0 0 \
			    $itk_option(-gripwidth) $itk_option(-gripwidth) \
			    -tags [list grip grip.$id grip.$id.$side] \
			    -fill $itk_option(-gripfill) -outline $itk_option(-gripoutline)]

	    $canvas_ bind $sel_id <Any-Enter> \
		[list $canvas_ config -cursor $itk_option(-linecursor)]

	    $canvas_ bind $sel_id <Any-Leave> [code $this reset_cursor]

	    $canvas_ bind $sel_id <1> \
		[code eval $this mark $canvasX_ $canvasY_]

	    $canvas_ bind $sel_id <Shift-1> \
		[code eval $this mark $canvasX_ $canvasY_]

	    $canvas_ bind $sel_id <ButtonRelease-1> \
		[code $this end_resize_line $id]

	    $canvas_ bind $sel_id <Shift-ButtonRelease-1> \
		[code $this end_resize_line $id]

	    $canvas_ bind $sel_id <B1-Motion> \
		[code eval $this resize_line $id $canvasX_ $canvasY_ $side 0]

	    $canvas_ bind $sel_id <Shift-B1-Motion> \
		[code eval $this resize_line $id $canvasX_ $canvasY_ $side 1]

	    if {$itk_option(-withrotate)} {
		$canvas_ bind $sel_id <Control-Enter> \
		    [list $canvas_ config -cursor $rotate_cursor_]
		$canvas_ bind $sel_id <Control-3> \
		    [code eval $this start_rotate $id $canvasX_ $canvasY_]
		$canvas_ bind $sel_id <Control-B3-Motion> \
		    [code eval $this update_rotate $id $canvasX_ $canvasY_]
		$canvas_ bind $sel_id <Control-ButtonRelease-3> \
		    [code $this end_rotate $id]
	    }
	}
	adjust_line_selection $id
    }

    
    # set the cursor for the canvas window and save the previous cursor
    # for later restoration
    
    public method set_cursor {cursor} {
	if {! $resizing_} {
	    set prev_cursor_ [$canvas_ cget -cursor]
	    $canvas_ config -cursor $cursor
	}
    }

    # set the cursor for the canvas window back to the previous one, unless
    # we are in the middle of a resize operation
    
    public method reset_cursor {} {
	if {! $resizing_} {
	    $canvas_ config -cursor $prev_cursor_
	    set prev_cursor_ $itk_option(-defaultcursor)
	}
    }


    # Draw the object selection grips (small boxes similar to those
    # used by FrameMaker and used to move and resize the object).
    # Lines (see above) get one at each end, 
    # other objects (here) get one at each corner and side of the object's 
    # bounding box. If type is "polygon" or "line", rotation bindings are
    # also enabled (with <Control-...>).

    protected method draw_selection_grips {id {type ""}} {
	# make sure we are using the canvas id
	set id [lindex [$canvas_ find withtag $id] 0]

	if {"$type" == "line" && [llength [$canvas_ coords $id]] == 4} {
	    draw_line_selection_grips $id
	    return
	}
	foreach side {n e s w ne nw se sw} {
	    set sel_id [$canvas_ create rectangle 0 0 \
			    $itk_option(-gripwidth) $itk_option(-gripwidth) \
			    -tags [list grip grip.$id grip.$id.$side] \
			    -fill $itk_option(-gripfill) -outline $itk_option(-gripoutline)]

	    $canvas_ bind $sel_id <Any-Enter> \
		[list $canvas_ config \
		     -cursor "$cursors_($side) white black" ]

	    $canvas_ bind $sel_id <Any-Leave> [code $this reset_cursor]

	    $canvas_ bind $sel_id <1> \
		[code eval $this start_resize $id $canvasX_ $canvasY_ $side]

	    $canvas_ bind $sel_id <Shift-1> \
		[code eval $this start_resize $id $canvasX_ $canvasY_ $side]

	    $canvas_ bind $sel_id <ButtonRelease-1> \
		[code $this end_resize $id]

	    $canvas_ bind $sel_id <Shift-ButtonRelease-1> \
		[code $this end_resize $id]

	    $canvas_ bind $sel_id <B1-Motion> \
		[code eval $this update_resize $id $canvasX_ $canvasY_ $side 0]

	    $canvas_ bind $sel_id <Shift-B1-Motion> \
		[code eval $this update_resize $id $canvasX_ $canvasY_ $side 1]

	    # Add support for rotation for polygons and lines.
	    # Tk doesn't support it directly, but we can do it manually for 
	    # polygons and (poly)lines. We don't want to allow rotation for
	    # "region" items, for now. Note that we could do the selection
	    # a lot better if the rotation angle was a Tk canvas item option
	    # (i.e.: the coords would stay the same, and the angle would change).
	    if {$itk_option(-withrotate) \
		    && ("$type" == "polygon" || "$type" == "line") \
		    && ![info exists regions_($id)]} {
		$canvas_ bind $sel_id <Control-Enter> \
		    [list $canvas_ config -cursor $rotate_cursor_]
		$canvas_ bind $sel_id <Control-3> \
		    [code eval $this start_rotate $id $canvasX_ $canvasY_]
		$canvas_ bind $sel_id <Control-B3-Motion> \
		    [code eval $this update_rotate $id $canvasX_ $canvasY_]
		$canvas_ bind $sel_id <Control-ButtonRelease-3> \
		    [code $this end_rotate $id]
	    }
	}
	adjust_object_selection $id
    }


    # deselect the given object

    public method deselect_object {id} {
	$canvas_ delete grip.$id
	$canvas_ dtag $id $w_.selected
	$canvas_ focus {}
    }


    # deselect all canvas objects

    public method deselect_objects {} {
	$canvas_ delete grip
	$canvas_ dtag all $w_.selected
	$canvas_ focus {}
    }

    
    # adjust the selection handles for the given object to fit the new size/pos.

    public method adjust_object_selection {id} {
	if {"[$canvas_ type $id]" == "line" && [llength [$canvas_ coords $id]] == 4} {
	    adjust_line_selection $id
	    return
	}
	lassign [$canvas_ bbox $id] x0 y0 x1 y1
	if {"$x0" == ""} {
	    deselect_object $id
	    return
	}
	set xm [expr {$x0+($x1-$x0)/2}]
	set ym [expr {$y0+($y1-$y0)/2}]
	
	set w [expr {$itk_option(-gripwidth)/2}]
	foreach i [list \
		       "nw $x0 $y0" \
		       "n $xm $y0" \
		       "ne $x1 $y0" \
		       "e $x1 $ym" \
		       "se $x1 $y1" \
		       "s $xm $y1" \
		       "sw $x0 $y1" \
		       "w $x0 $ym" ] {
	    lassign $i side x y 
	    $canvas_ coords grip.$id.$side \
		[expr {$x-$w}] [expr {$y-$w}] \
		[expr {$x+$w}] [expr {$y+$w}]
	}
    }


    # adjust the selection handles for the given simple line

    public method adjust_line_selection {id} {
	lassign [$canvas_ coords $id] x0 y0 x1 y1
	set xm [expr {$x0+($x1-$x0)/2}]
	set ym [expr {$y0+($y1-$y0)/2}]
	set w [expr {$itk_option(-gripwidth)/2}]
	foreach i [list \
		       "e $x0 $y0" \
		       "w $x1 $y1" ] {
	    lassign $i side x y 
	    $canvas_ coords grip.$id.$side \
		[expr {$x-$w}] [expr {$y-$w}] \
		[expr {$x+$w}] [expr {$y+$w}]
	}
    }


    # resize the given line

    protected method resize_line {id x y side constrain} {
	clip x y
	set resizing_ 1
	lassign [$canvas_ coords $id] x0 y0 x1 y1
	if {$constrain} {
	    if {abs($x1-$x0)<abs($y1-$y0)} {
		if {"$side" == "e"} {
		    set x1 $x
		} else {
		    set x $x0
		}
	    } else {
		if {"$side" == "e"} {
		    set y1 $y
		} else {
		    set y $y0
		}
	    }
	}
	if {"$side" == "e"} {
	    $canvas_ coords $id $x $y $x1 $y1
	} else {
	    $canvas_ coords $id $x0 $y0 $x $y
	}
	if {[info exists notify_update_($id)]} {
	    eval "$notify_update_($id) resize"
	}
    }

        
    # stop resizing the selected line

    protected method end_resize_line {id} {
	if {[info exists notify_($id)]} {
	    eval "$notify_($id) resize"
	}
	adjust_line_selection $id
	set resizing_ 0
	reset_cursor
    }

        

        
    # stop resizing the selected object

    protected method end_resize {id} {
	if {[info exists notify_($id)]} {
	    eval "$notify_($id) resize"
	}
	adjust_object_selection $id
	set resizing_ 0
	reset_cursor
    }

        
    # start resizing the selected object
    # (19.9.96: added changes from pbiereic)

    protected method start_resize {id x y side} {
        clip x y
        set endx_ $x
        set endy_ $y
        set resizing_ 1
        lassign [$canvas_ bbox $id] x0 y0 x1 y1
        switch -exact $side {
            nw {
                set startx_ $x1
                set starty_ $y1
                set dragx_ 1
                set dragy_ 1
                set posx_ 2
                set posy_ 3
            }
            n  {
                set startx_ [expr {$x0+($x1-$x0)/2.}]
                set starty_ $y1
                set dragx_ 0
                set dragy_ 1
                set posx_ -1
                set posy_ 3
            }
            ne {
                set startx_ $x0
                set starty_ $y1
                set dragx_ 1
                set dragy_ 1
                set posx_ 0
                set posy_ 3
            }
            e  {
                set startx_ $x0
                set starty_ [expr {$y0+($y1-$y0)/2.}]
                set dragx_ 1
                set dragy_ 0
                set posx_ 0
                set posy_ -1
            }
            se {
                set startx_ $x0
                set starty_ $y0
                set dragx_ 1
                set dragy_ 1
                set posx_ 0
                set posy_ 1
            }
            s  {
                set startx_ [expr {$x0+($x1-$x0)/2.}]
                set starty_ $y0
                set dragx_ 0
                set dragy_ 1
                set posx_ -1
                set posy_ 1
            }
            sw {
                set startx_ $x1
                set starty_ $y0
                set dragx_ 1
                set dragy_ 1
                set posx_ 2
                set posy_ 1
            }
            w  {
                set startx_ $x1
                set starty_ [expr {$y0+($y1-$y0)/2.}]
                set dragx_ 1
                set dragy_ 0
                set posx_ 2
                set posy_ -1
            }
        }
        if {$posx_ == 0} {
            set posx2_ 2
        } else {
            set posx2_ 0
        }
        if {$posy_ == 1} {
            set posy2_ 3
        } else {
            set posy2_ 1
        }
    }
        

    # update the resizing of the selected object
    # (updated with changes from pbiereic, 19.9.96)

    protected method update_resize {id x y side constrain} {
        clip x y
        set dx [set dy 1]
        if {$constrain && $dragx_ && $dragy_} {
            if {abs($startx_-$x) > abs($starty_-$y)} {
                set x [expr {$startx_+($y-$starty_)}]
            } else {
                set y [expr {$starty_+($x-$startx_)}]
            }
        }

        if {$dragx_} {
            set d [expr {$endx_-$startx_}]
            if {$d != 0} {
                set dx [expr {($x-$startx_)/$d}]
            }
        } 
        if {$dragy_} {
            set d [expr {$endy_-$starty_}]
            if {$d != 0} {
                set dy [expr {($y-$starty_)/$d}]
            }
        }
        if {$dx < 0 || $dy < 0} {
            return
        }
        
        if {$dx != 0 && $dy != 0} {
            $canvas_ scale $id $startx_ $starty_ $dx $dy
        }
        # move back to exact position (bbox only returns integer coords)
        # set exact end position to avoid cummulative errors
        set pos [$canvas_ bbox $id]
        if {$posx_ == -1} {
            set dx 0
            set endx_ $x
        } else {
            set dx [expr {$startx_ - [lindex $pos $posx_]}]
            set endx_ [lindex $pos $posx2_]
        }
        if {$posy_ == -1} {
            set dy 0
            set endy_ $y
        } else {
            set dy [expr {$starty_ - [lindex $pos $posy_]}]
            set endy_ [lindex $pos $posy2_]
        }
        $canvas_ move $id $dx $dy
        if {[info exists notify_update_($id)]} {
            eval "$notify_update_($id) resize"
        }
    }

    
    # create a new object in the canvas
    
    public method create_object {x y} {
	mark $x $y
	set obj_coords_ "$x $y"
	set obj_id_ [create_$drawing_mode_ $x $y]
	if {"$drawing_mode_" != "region"} {
	    $canvas_ addtag objects withtag $obj_id_
	    $canvas_ addtag $w_.objects withtag $obj_id_
	}
	set resizing_ 1
    }

    
    # return 1 if the given item has the given tag

    public method item_has_tag {item tag} {
	return [expr {[lsearch -exact [$canvas_ gettags $item] $tag] != -1}]
    }


    # return 1 if the given item is currently selected

    public method item_is_selected {item} {
	return [item_has_tag $item $w_.selected]
    }


    # return a list of all of the currently selected items

    public method selected_items {} {
	return [$canvas_ find withtag $w_.selected]
    }


    # move the given object

    protected method move_object {id x y} {
	if {$drawing_} {
	    return
	}
	if {![item_is_selected $id]} {
	    deselect_objects
	    select_object $id
	    mark $x $y
	}
	set dx [expr {$x-($startx_+$xoffset_)}]
	set dy [expr {$y-($starty_+$yoffset_)}]
	set xoffset_ [expr {$x-$startx_}]
	set yoffset_ [expr {$y-$starty_}]
	
	if {$itk_option(-clipping)} {
	    # don't allow a move outside of drawing area
	    lassign [$canvas_ bbox $w_.selected] x0 y0 x1 y1
	    set dx0 [expr {$x0_-$x0}]
	    set dy0 [expr {$y0_-$y0}]
	    set dx1 [expr {$x1_-$x1}]
	    set dy1 [expr {$y1_-$y1}]
	    if {$dx < $dx0} {
		set dx $dx0
	    } elseif {$dx > $dx1} {
		set dx $dx1
	    }
	    if {$dy < $dy0} {
		set dy $dy0
	    } elseif {$dy > $dy1} {
		set dy $dy1
	    }
	}

	if {$dx || $dy} {
	    foreach i "$w_.selected grip" {
		$canvas_ move $i $dx $dy
	    }
	    foreach i [selected_items] {
		set moved_($i) 1
		if {[info exists notify_update_($i)]} {
		    eval "$notify_update_($i) move"
		}
	    }
	    update idletasks
	}
    }

    
    # this method is called when a move operation is done

    protected method end_move {id} {
	foreach i [selected_items] {
	    if {[info exists moved_($i)]} {
		if {[info exists notify_($i)]} {
		    eval "$notify_($i) move"
		}	
		unset moved_($i)
	    }
	}
    }


    # create and return a new rectangle object (actually create a
    # polygon object in the shape of a rectangle, since it is more
    # flexible and can be rotated).

    protected method create_rectangle {x y} {
	clip x y
	if {$itk_option(-withrotate)} {
	    return [$canvas_ create polygon $x $y $x $y $x $y $x $y \
			-width $itk_option(-linewidth) \
			-fill $itk_option(-fillcolor) \
			-outline $itk_option(-outlinecolor) \
			-smooth $itk_option(-smooth) \
			-stipple $itk_option(-stipplepattern)]
	} else {
	    return [$canvas_ create rect $x $y $x $y \
			-width $itk_option(-linewidth) \
			-fill $itk_option(-fillcolor) \
			-outline $itk_option(-outlinecolor) \
			-stipple $itk_option(-stipplepattern)]
	}
    }


    # create and return a new region (dashed rectangle marking a region
    # of the canvas)

    protected method create_region {x y} {
	clip x y
	set id [$canvas_ create line $x $y $x $y $x $y $x $y \
			-width 2 \
			-fill $itk_option(-outlinecolor) \
			-stipple pat8]
	set regions_($id) 1
	return $id
    }


    # create  and return a new oval object

    protected method create_oval {x y} {
	clip x y
	return [$canvas_ create oval $x $y $x $y \
		    -width $itk_option(-linewidth) \
		    -fill $itk_option(-fillcolor) \
		    -outline $itk_option(-outlinecolor) \
		    -stipple $itk_option(-stipplepattern)]
    }


    # create  and return a new arc object

    protected method create_arc {x y} {
	clip x y
	return [$canvas_ create arc $x $y $x $y \
		    -width $itk_option(-linewidth) \
		    -fill $itk_option(-fillcolor) \
		    -outline $itk_option(-outlinecolor) \
		    -stipple $itk_option(-stipplepattern)]
    }


    # return a color for a line that will be visible

    protected method get_line_color {} {
	if {"$itk_option(-fillcolor)" == ""} {
	    return $itk_option(-outlinecolor)
	} else {
	    return $itk_option(-fillcolor)
	}
    }


    # create  and return a new line object

    protected method create_line {x y} {
	clip x y
	return [$canvas_ create line $x $y $x $y \
		    -width $itk_option(-linewidth) \
		    -fill [get_line_color] \
		    -stipple $itk_option(-linestipple) \
		    -arrow $itk_option(-arrowtype) \
		    -smooth $itk_option(-smooth) \
		    -arrowshape $itk_option(-arrowshape)]
    }


    # create  and return a new freehand object

    protected method create_freehand {x y} {
	clip x y
	return [$canvas_ create line $x $y $x $y \
		    -width $itk_option(-linewidth) \
		    -fill [get_line_color] \
		    -arrow $itk_option(-arrowtype) \
		    -stipple $itk_option(-linestipple) \
		    -arrowshape $itk_option(-arrowshape)]
    }


    # create  and return a new polyline object

    protected method create_polyline {x y} {
	clip x y
	return [create_line $x $y]
    }


    # create  and return a new polygon object

    protected method create_polygon {x y} {
	clip x y
	set obj_coords_ "$x $y $x $y $x $y"
	return [$canvas_ create polygon $x $y $x $y $x $y  \
		    -fill $itk_option(-fillcolor) \
		    -outline $itk_option(-outlinecolor) \
		    -width $itk_option(-linewidth) \
		    -smooth $itk_option(-smooth) \
		    -stipple $itk_option(-stipplepattern)]
    }


    # create  and return a new text object

   protected  method create_text {x y} {
	clip x y
	set id [$canvas_ create text $x $y  \
		    -fill [get_line_color] \
		    -font $itk_option(-textfont) \
		    -anchor w \
		    -stipple $itk_option(-linestipple)]
	

	# set up text bindings
	ct_add_bindings $canvas_ $id
	focus $canvas_
	$canvas_ focus $id
	$canvas_ icursor $id 0
	$canvas_ addtag $w_.selected withtag $id
	return $id
    }


    # create and return a new label (entry) object

    protected method create_label {x y} {
	clip x y
	set entry [entry $canvas_.lab[incr tag_count_] \
		       -font $itk_option(-textfont) \
		       -borderwidth 0 -relief flat \
		       -width 0]
	set id [$canvas_ create window $x $y  \
		    -window $entry \
		    -anchor w]
	focus $entry
	bind $entry <Control-Button-1> "+focus $canvas_; 
                         $this select_only_object $id $canvasX_ $canvasY_"
	bind $entry <1> "+focus $entry"
	$canvas_ addtag $w_.selected withtag $id
	return $id 
    }

    
    # resize the rectangle for a label to fit the text
    
    protected method resize_label {text rect tag} {
	eval "$canvas_ coords $rect [$canvas_ bbox $text]"
    }


    # update the current object in the canvas with the new x, y end points
    
    protected method update_object {x y {constrain 0}} {
	clip x y
	if {$constrain} {
	    if {abs($startx_-$x)>abs($starty_-$y)} {
		set x [expr {$startx_+($y-$starty_)}]
	    } else {
		set y [expr {$starty_+($x-$startx_)}]
	    }
	}
	$canvas_ coords $obj_id_ $startx_ $starty_ $x $y
    }


    # update the current line object in the canvas with the new x, y end points
    
    protected method update_line {x y {constrain 0}} {
	clip x y
	if {$constrain} {
	    if {abs($startx_-$x)<abs($starty_-$y)} {
		set x $startx_
	    } else {
		set y $starty_
	    }
	}
	$canvas_ coords $obj_id_ $startx_ $starty_ $x $y
    }


    # update the current polygon/polyline object in the canvas with the 
    # new x, y end points
    
    protected method update_poly_object {x y} {
	clip x y
	eval $canvas_ coords $obj_id_ $obj_coords_ $x $y
    }


    # update the current rectangle (or polygon, so we can rotate it)
    # object in the canvas with the new x, y end points
    
    protected method update_rectangle {x y {constrain 0}} {
	clip x y
	if {$constrain} {
	    if {abs($startx_-$x)>abs($starty_-$y)} {
		set x [expr {$startx_+($y-$starty_)}]
	    } else {
		set y [expr {$starty_+($x-$startx_)}]
	    }
	}
	if {$itk_option(-withrotate)} {
	    $canvas_ coords $obj_id_ $startx_ $starty_ $x $starty_ $x $y $startx_ $y
	} else {
	    $canvas_ coords $obj_id_ $startx_ $starty_ $x $y
	}
    }


    # update the current region object (dashed rectangle used for marking
    # a region on the canvas)
    
    protected method update_region {x y {constrain 0}} {
	clip x y
	if {$constrain} {
	    if {abs($startx_-$x)>abs($starty_-$y)} {
		set x [expr {$startx_+($y-$starty_)}]
	    } else {
		set y [expr {$starty_+($x-$startx_)}]
	    }
	}
	$canvas_ coords $obj_id_ \
	    $startx_ $starty_ \
	    $x $starty_ \
	    $x $y \
	    $startx_ $y \
	    $startx_ $starty_
    }


    # update the current  freehand object in the canvas with the new x, y end points
    
    protected method update_freehand {x y} {
	clip x y
	lappend obj_coords_ $x $y
	eval $canvas_ coords $obj_id_ $obj_coords_ $x $y
    }

    
    # arrange to have cmd evaluated whenever the given object is moved
    # or resized. id is the canvas id of the object, If update_flag 
    # is 1, $cmd is also evaluated while the item is being moved or
    # resized, otherwise only when the operation is done.
    
    public method add_notify_cmd {id cmd {update_flag 0}} {
	set notify_($id) $cmd
	if {$update_flag} {
	    set notify_update_($id) $cmd
	}
    }

    
    # remove the notify command for the given canvas id

    public method remove_notify_cmd {id} {
	if {[info exists notify_($id)]} {
	    unset notify_($id)
	}
	if {[info exists notify_update_($id)]} {
	    unset notify_update_($id)
	}
    }

    
    # Exchange the X and Y coords for the given item (not really rotate...)

    public method rotate {item} {
	set ids [$canvas_ find withtag $item]
	foreach id $ids {
	    catch {
		set coords [$canvas_ coords $id]
		set n [llength $coords]
		set ncoords {}
		for {set i 0} {$i < $n} {incr i 2} {
		    lappend ncoords [lindex $coords [expr {$i+1}]] [lindex $coords $i]
		}
		eval $canvas_ coords $id $ncoords
	    }
	}
    }


    # flip the named item(s) on the X axis by subtracting the x
    # coordinates from $maxx

    public method flipx {item maxx} {
	set ids [$canvas_ find withtag $item]
	foreach id $ids {
	    catch {
		set coords [$canvas_ coords $id]
		set n [llength $coords]
		set ncoords {}
		for {set i 0} {$i < $n} {incr i 2} {
		    lappend ncoords [expr {$maxx-[lindex $coords $i]}] \
			[lindex $coords [expr {$i+1}]] 
		}
		eval $canvas_ coords $id $ncoords
	    }
	}
    }


    # flip the named item(s) on the Y axis by subtracting the y
    # coordinates from $maxy

    public method flipy {item maxy} {
	set ids [$canvas_ find withtag $item]
	foreach id $ids {
	    catch {
		set coords [$canvas_ coords $id]
		set n [llength $coords]
		set ncoords {}
		for {set i 0} {$i < $n} {incr i 2} {
		    lappend ncoords [lindex $coords $i] [expr {$maxy-[lindex $coords [expr {$i+1}]]}] 
		}
		eval $canvas_ coords $id $ncoords
	    }
	}
    }

       
    # start rotating the selected object

    protected method start_rotate {id x y} {
	mark $x $y
	set rotate_angle_ ""

	# get center of object
	lassign [$canvas_ bbox $w_.selected] x0 y0 x1 y1
	set rotate_cx_ [expr {($x1+$x0)/2.}]
	set rotate_cy_ [expr {($y1+$y0)/2.}]
    }


    # rotate the given point about the given center point by the given angle 
    # (in radians) and return a list {x y} as the result.

     public method rotate_point {x y cx cy angle} {
	 set x [expr {$x-$cx}]
	 set y [expr {$y-$cy}]
	set tmp $x
	 set cosa [expr {cos($angle)}]
	 set sina [expr {sin($angle)}]
	 set x [expr {$x*$cosa+$y*$sina+$cx}]
	 set y [expr {-$tmp*$sina+$y*$cosa+$cy}]
	return "$x $y"
    }
        
    
    # update the rotating of the selected object

    protected method update_rotate {id x y} {
        clip x y
	set dx [expr {$rotate_cx_-double($x+0.5)}]
	set dy [expr {$rotate_cy_-double($y+0.5)}]
	set a [expr {atan2($dx,$dy)}]
	if {"$rotate_angle_" == ""} {
	    set rotate_angle_ $a
	    return
	}
	set angle [expr {$a-$rotate_angle_}]
	set rotate_angle_ $a
	set coords [$canvas_ coords $id]
	set cmd [list $canvas_ coords $id]
	set n [llength $coords]
	for {set i 0} {$i<$n} {incr i 2} {
	    set x [lindex $coords $i]
	    set y [lindex $coords [expr {$i+1}]]
	    append cmd " [rotate_point $x $y $rotate_cx_ $rotate_cy_ $angle]"
	}
	eval $cmd
    }


    # stop rotating the selected object

    protected method end_rotate {id} {
	if {[info exists notify_($id)]} {
	    eval "$notify_($id) rotate"
	}
	adjust_object_selection $id
	reset_cursor
    }
        
    
    # -- options --

    # if true (default) create the GUI interface (toolbox), otherwise don't
    itk_option define -withtoolbox withToolbox WithToolbox {1}

    # if true (default) use polygons for rectangles to support rotation in Tcl
    # (Tk doesn't support rotating, but polygons can be rotated in Tcl code).
    # Subclasses that define rotation by adding new canvas objects can set this
    # option to 0.
    itk_option define -withrotate withRotate WithRotate {1}

    # canvas window
    itk_option define -canvas canvas Canvas {} {
	set canvas_ $itk_option(-canvas)
	# string used in bindings to translate %x and %y to canvas coords
	set canvasX_ "\[$canvas_ canvasx %x\]"
	set canvasY_ "\[$canvas_ canvasy %y\]"
    }

    # specify bounding box of interactive drawing area as a list {x0 y0 x1 y1}
    itk_option define -bbox bbox Bbox {} {
	if {[llength $itk_option(-bbox)] == 4} {
	    lassign $itk_option(-bbox) x0_ y0_ x1_ y1_
	    # leave room for the selection grips, line width, etc
	    set w $itk_option(-gripwidth)
	    set x0_ [expr {$x0_-$w}]
	    set y0_ [expr {$y0_-$w}]
	    set x1_ [expr {$x1_+$w}]
	    set y1_ [expr {$y1_+$w}]
	}
    }

    # flag: if true, keep graphics within the specified bounding box (-bbox option)
    itk_option define -clipping clipping Clipping 1
 
    # list of colors for menu
    itk_option define -colors colors Colors {
	white 
	grey90 grey80 grey70 grey60 grey50 grey40 grey30 grey20 grey10         
	black 
	red green blue cyan magenta yellow
    }
    
    # default color for outlines
    itk_option define -outlinecolor outlineColor OutlineColor {grey90} 

    # default text item font
    itk_option define -textfont textFont TextFont {-*-courier-bold-r-*-*-*-120-*-*-*-*-*-*}

    # default list of fonts for font menu
    itk_option define -fonts fonts Fonts {
	-*-courier-medium-r-*-*-*-120-*-*-*-*-*-*
	-*-courier-medium-o-*-*-*-120-*-*-*-*-*-*
	-*-courier-bold-r-*-*-*-120-*-*-*-*-*-*
	-*-courier-medium-r-*-*-*-140-*-*-*-*-*-*
	-*-courier-medium-o-*-*-*-140-*-*-*-*-*-*
	-*-courier-bold-r-*-*-*-140-*-*-*-*-*-*
	-*-courier-medium-r-*-*-*-180-*-*-*-*-*-*
	-*-courier-medium-o-*-*-*-180-*-*-*-*-*-*
	-*-courier-bold-r-*-*-*-180-*-*-*-*-*-*
	-*-courier-medium-r-*-*-*-240-*-*-*-*-*-*
	-*-courier-medium-o-*-*-*-240-*-*-*-*-*-*
	-*-courier-bold-r-*-*-*-240-*-*-*-*-*-*
    }

    # default value for smooth option
    itk_option define -smooth smooth Smooth 0 


    # default line width for drawing
    itk_option define -linewidth lineWidth LineWidth 1

    # default arrow type for lines
    itk_option define -arrowtype arrowType ArrowType none 

    # default arrow shape for lines
    itk_option define -arrowshape arrowShape ArrowShape {8 10 3}

    # default fill color for drawing
    itk_option define -fillcolor fillColor FillColor {white}

    # default stipple pattern for drawing
    itk_option define -stipplepattern stipplePattern StipplePattern "pat7"

    # default stipple for lines
    itk_option define -linestipple lineStipple LineStipple "pat0"

    # -- standard cursor names taken from X11/cursorfont.h --

    # default cursor when not drawing
    itk_option define -defaultcursor defaultCursor DefaultCursor {}

    # cursor when drawing graphics
    itk_option define -drawcursor drawCursor DrawCursor {tcross white black}

    # cursor when creating region items (dashed box marking a region)
    itk_option define -regioncursor regionCursor RegionCursor {leftbutton white black}

    # Tcl command to evaluate whenever a "region" object is created, if no
    # specific "create" command was specified.
    itk_option define -regioncommand regionCommand RegionCommand {}

    # cursor when creating text items
    itk_option define -textcursor textCursor TextCursor {left_side}
    
    # cursor displayed over simple lines for resizing
    itk_option define -linecursor lineCursor LineCursor {draft_small white black}

    # size of selection grips
    itk_option define -gripwidth gripWidth GripWidth 5

    # fill color of selection grips
    itk_option define -gripfill gripFill GripFill white

    # outline color of selection grips
    itk_option define -gripoutline gripOutline GripOutline black

    # flag: if true, display selection grips when an item is selected
    itk_option define -show_selection_grips show_selection_grips Show_selection_grips 1

    # flag: if true, display menus over graphic objects when selected with <3>
    itk_option define -show_object_menu show_object_menu Show_object_menu 1

  
    # -- protected member variables --

    # list of drawing types 
    protected variable drawing_modes_ {
	anyselect region
	line rectangle oval
	polyline polygon
	text
    }

    # available arrow shapes for canvas lines 
    # (corresponds to bitmaps named ar$1_$2_$3)
    protected variable arrowshapes_ {
	{8 10 3}
	{10 13 3}
	{12 11 3}
	{12 12 3}
	{6 6 4}
	{8 12 9}
	{8 8 6}
    } 

    # current draw type
    protected variable drawing_mode_ {anyselect}

    # starting x coord of object being drawn
    protected variable startx_ 0

    # starting y coord of object being drawn
    protected variable starty_ 0

    # ending x coord of object being drawn
    protected variable endx_ 0

    # ending y coord of object being drawn
    protected variable endy_ 0

    # x offset for move operations
    protected variable xoffset_ 0

    # y offsets for move operations
    protected variable yoffset_ 0

    # flag: true if resizing on X axis
    protected variable dragx_ 0

    # flag: true if resizing on Y axis
    protected variable dragy_ 0

    # flag, true if drawing currently
    protected variable drawing_ 0

    # list of coords for current object being created
    protected variable obj_coords_ {}

    # canvas id of the current canvas item
    protected variable obj_id_

    # strings used in bindings to translate %x to canvas coords
    protected variable canvasX_ 

    # strings used in bindings to translate %y to canvas coords
    protected variable canvasY_ 
    
    # counter used for compound objects to generate canvas tags
    protected variable tag_count_ 0
 
    # array(canvasID,option) of option to use instead for that id
    protected variable option_map_

    # command to evaluate when the current item has been created
    protected variable create_cmd_
    
    # array(id) of tcl command to evaluate when object given by canvas id 
    # is moved or scaled
    protected variable notify_

    # same as notify_ above, but indicates command should be evaluated
    # while item is being moved or resized
    protected variable notify_update_

    # array(id) of flag: set if object with id was moved
    protected variable moved_

    # flag: true if currently resizing an item
    protected variable resizing_ 0

    # popup menu for canvas items
    protected variable object_menu_

    # saved menu (optional, from caller's menubar) with drawing commands
    # for setting the state of the menu items.
    protected variable menu_ {}
    
    # saved previous cursor for canvas
    protected variable prev_cursor_ {}

    # cursor to use in rotate mode
    protected variable rotate_cursor_ {exchange}

    # index of stipple bitmap (0 .. 15, 7 is see through, see -stipplepattern)
    protected variable stipple_index_ 7

    # canvas widget
    protected variable canvas_

    # x0 of bounding box of interactive drawing area
    protected variable x0_ 0

    # y0 of bounding box of interactive drawing area
    protected variable y0_ 0

    # x1 of bounding box of interactive drawing area
    protected variable x1_ 1000

    # y1 of bounding box of interactive drawing area
    protected variable y1_ 1000

    # x pos for resize operations
    protected variable posx_ 0

    # y pos for resize operations
    protected variable posy_ 0

    # x2 pos for resize operations
    protected variable posx2_ 0

    # y2 pos for resize operations
    protected variable posy2_ 0

    # center X coordinate of object being rotated
    protected variable rotate_cx_ 0

    # center Y coordinate of object being rotated
    protected variable rotate_cy_ 0

    # current rotation angle of object
    protected variable rotate_angle_ 0

    # array(canvasId) of region ids, used to keep these from rotating
    protected variable regions_

    # -- common variables --

    # array(direction={n,w,e,s,nw,ne,sw,se}) of cursor names for selection handles
    protected common cursors_
    set cursors_(n) top_side
    set cursors_(w) left_side
    set cursors_(s) bottom_side
    set cursors_(e) right_side
    set cursors_(nw) top_left_corner
    set cursors_(ne) top_right_corner
    set cursors_(sw) bottom_left_corner
    set cursors_(se) bottom_right_corner

    # const PI
    protected common pi_ 3.14159265358979323846

    # const PI/180.
    protected common rad_ [expr {$pi_/180.}]
}
