# E.S.O. - VLT project/ESO Archive
# @(#) $Id: SymbolConfig.tcl,v 1.11 1998/10/28 17:37:23 abrighto Exp $
#
# SymbolConfig.tcl - Widget for editing plot symbol information
#
# who         when       what
# --------   ---------   ----------------------------------------------
# A.Brighton 22 Oct 97   created


itk::usual SymbolConfig {}


# Class SymbolConfig defines a dialog window for editing the plot symbol 
# information for the AstroCat catalog window. The dialog lets you select
# which symbols to plot under given conditions, the size, color, shape
# of the symbols, specified as expressions in terms of column name variables.

itcl::class cat::SymbolConfig {
    inherit util::TopLevelWidget


    # constructor

    constructor {args} {
	eval itk_initialize $args
    }

    
    # called after options have been evaluated

    protected method init {} {
	wm title $w_ "Plot Symbols for $catalog ($itk_option(-number))"
	wm iconname $w_ "Plot Symbols for $catalog"

	add_table
	add_dialog_buttons
	fill_table
	make_short_help
    }


    # add a table listing the symbols and some widgets to edit them

    protected method add_table {} {
	# table listing overview at top
	pack [set table_ \
		  [TableList $w_.table \
		       -height 3 \
		       -exportselection 0 \
		       -headings {Columns Symbol Color Ratio Angle Label Condition Size Units}]] \
	    -side top -padx 1m -pady 1m -fill both -expand 1

	bind [$w_.table component listbox] <1> +[code $this update_table]
	bind [$w_.table component listbox] <ButtonRelease-1> +[code $this select_row]

	# lower frame
	pack [set f [frame $w_.f]] \
	    -side top -padx 2m -pady 2m -ipadx 2m -ipady 2m -fill both -expand 1

	# double listbox for choosing column names
	pack [frame $f.cols] \
	    -side top -fill x -expand 1 -ipady 2m
	    
	pack \
	    [LabelWidget $f.cols.label \
		 -text "Colums:" \
		 -labelwidth $itk_option(-labelwidth) \
		 -anchor $itk_option(-anchor) \
		 -labelfont $itk_option(-labelfont)] \
	    -side left

	pack \
	    [DoubleList $f.cols.dlist \
		 -updown 0 \
		 -height 2 \
		 -width 12 \
		 -lefttitle Used \
		 -righttitle {Not Used}] \
	    -side left -padx 2m -fill both -expand 1

	set left_ [$f.cols.dlist component left]
	set right_ [$f.cols.dlist component right]

	# set default contents of right list to catalog cols
	$right_ set_contents $columns
	
	pack \
	    [set symbol_ [LabelMenu $f.symbol \
			   -text "Symbol:" \
			   -relief raised \
			   -borderwidth 3 \
			   -labelwidth $itk_option(-labelwidth) \
			   -anchor $itk_option(-anchor) \
			   -valuefont $itk_option(-valuefont) \
			   -labelfont $itk_option(-labelfont)]] \
	    [set color_ [LabelMenu $f.color \
			   -text "Color:" \
			   -relief raised \
			   -borderwidth 3 \
			   -labelwidth $itk_option(-labelwidth) \
			   -anchor $itk_option(-anchor) \
			   -valuefont $itk_option(-valuefont) \
			   -labelfont $itk_option(-labelfont)]] \
	    [set ratio_ [LabelEntry $f.ratio \
			   -text "Ratio:" \
			   -state disabled \
			    -command [code $this apply] \
			   -labelwidth $itk_option(-labelwidth) \
			   -anchor $itk_option(-anchor) \
			   -valuefont $itk_option(-valuefont) \
			   -labelfont $itk_option(-labelfont)]] \
	    [set angle_ [LabelEntry $f.angle \
			   -text "Angle:" \
			   -state disabled \
			   -command [code $this apply] \
			   -labelwidth $itk_option(-labelwidth) \
			   -anchor $itk_option(-anchor) \
			   -valuefont $itk_option(-valuefont) \
			   -labelfont $itk_option(-labelfont)]] \
	    [set label_ [LabelEntry $f.label \
			   -text "Label:" \
			   -labelwidth $itk_option(-labelwidth) \
			   -command [code $this apply] \
			   -anchor $itk_option(-anchor) \
			   -valuefont $itk_option(-valuefont) \
			   -labelfont $itk_option(-labelfont)]] \
	    [set cond_  [LabelEntry $f.cond \
			   -text "Condition:" \
			   -labelwidth $itk_option(-labelwidth) \
			   -command [code $this apply] \
			   -anchor $itk_option(-anchor) \
			   -valuefont $itk_option(-valuefont) \
			   -labelfont $itk_option(-labelfont)]] \
	    [set size_ [LabelEntry $f.size \
			   -text "Size:" \
			   -labelwidth $itk_option(-labelwidth) \
			   -command [code $this apply] \
			   -anchor $itk_option(-anchor) \
			   -valuefont $itk_option(-valuefont) \
			   -labelfont $itk_option(-labelfont)]] \
	    [set units_ [LabelMenu $f.units \
			   -text "Units:" \
			   -relief raised \
			   -borderwidth 3 \
			   -labelwidth $itk_option(-labelwidth) \
			   -anchor $itk_option(-anchor) \
			   -valuefont $itk_option(-valuefont) \
			   -labelfont $itk_option(-labelfont)]] \
	    -side top -fill x -expand 1 -ipady 2m

	# fill the overview table
	if {[catch {set symbols [$astrocat symbol]} msg]} {
	    error_dialog $msg
	    quit
	}
	# add menu choices
	foreach i {circle square cross triangle diamond plus compass ellipse line arrow} {
	    $symbol_ add \
		-bitmap symb_$i \
		-value $i \
		-command [code $this set_symbol $i]
	}

	$color_ add \
	    -label "default (black&white)" \
	    -background [option get $color_ *Menu*background Background] \
	    -value {}

	foreach i $itk_option(-colors) {
	    $color_ add \
		-label {    } \
		-value $i \
		-background $i

	}
	foreach i {
	    {{default (image pixels)} {}}
	    {{WCS degrees in J2000} {deg 2000}}
	    {{WCS degrees in B1950} {deg 1950}}} {
	    $units_ add \
		-label [lindex $i 0] \
		-value [lindex $i 1]
	}
    }

    
    # called when a symbol is selcted from the menu

    protected method set_symbol {symbol} {
	if {"$symbol" == "ellipse" \
		|| "$symbol" == "compass"\
		|| "$symbol" == "plus"\
		|| "$symbol" == "line"\
		|| "$symbol" == "arrow"} {
	    set state normal
	} else {
	    set state disabled
	}
	$ratio_ config -state $state
	$angle_ config -state $state
    }

    
    # fill the table with the symbol info for the current catalog

    protected method fill_table {} {
	set info {}
	foreach i [$astrocat symbol] {
	    lassign $i cols sym expr
	    lassign $sym symbol color ratio angle label cond
	    lassign $expr size units
	    lappend info [list $cols $symbol $color $ratio $angle $label $cond $size $units]
	}
	if {[llength $info]} {
	    $table_ config -info $info
	    $table_ select_row 0
	    select_row
	} else {
	    after 1000 \
		[code info_dialog \
		     "Note: This catalog does not have any plot symbols yet. \
                      To add a symbol, fill out the form below and then select 'Add Symbol'" $w_]
	}
    }

    
    # update the table display from the entries and return an empty
    # string if all was OK

    protected method update_table {args} {
	if {[catch {set row [get_row]} msg]} {
	    error_dialog $msg
	    return ERROR
	}
	if {"$row" != "" && "$sel_" != "" && "$sel_" != "$row"} {
	    $table_ set_row $sel_ $row
	    set sel_ $row
	    incr save_needed_
	}
    }

    
    # apply the current changes

    protected method apply {args} {
	if {"[update_table]" == "ERROR"} {
	    return
	}
	save
    }


    # save the changes in the default config file

    protected method save {{ask 0}} {
	if {$save_needed_} {
	    set info {}
	    foreach i [$table_ get_contents] {
		if {[llength $i] == 9} {
		    lassign $i cols symbol color ratio angle label cond size units
		    lappend info [list $cols \
				      [list $symbol $color $ratio $angle $label $cond] \
				      [list $size $units]]
		}
	    }
	    if {[catch {$astrocat symbol [join $info :]} msg]} {
		error_dialog $msg $w_
		return
	    }

	    if {!$ask || [confirm_dialog "Do you want to save your changes in the default catalog \
                                  config file (~/.skycat/skycat.cfg)?" $w_]} {
		cat::CatalogInfo::save {} $w_ $ask
		set save_needed_ 0
	    }
	    
	    if {"$command" != ""} {
		eval $command
	    }
	}
    }

    
    # called when a table row is selected
    
    protected method select_row {} {
	set sel_ [lindex [$table_ get_selected] 0]
	lassign $sel_ cols symbol color ratio angle label cond size units

	# put current columns used in the left box and others in the right
	foreach i $cols {
	    set a($i) 1
	}
	set other {}
	foreach i $columns {
	    if {! [info exists a($i)]} {
		lappend other $i
	    }
	}
	$left_ set_contents $cols
	$right_ set_contents $other

	$symbol_ config -value $symbol
	$color_ config -value $color
	$ratio_ config -value $ratio
	$angle_ config -value $angle
	$label_ config -value $label
	$cond_ config -value $cond
	set_symbol $symbol
	$size_ config -value $size
	$units_ config -value $units

	$w_.remove config -state normal
    }

    
    # return a table row based on the current entries and menus or empty
    # if no data was enterred. Raises an error if there was something wrong.

    protected method get_row {} {
	set row {}
	
	set cols [$left_ get_contents]
	set symbol [$symbol_ get]
	set color [$color_ get]
	set ratio [$ratio_ get]
	set angle [$angle_ get]
	set label [$label_ get]
	set cond  [$cond_ get]
	set size [$size_ get]
	set units [$units_ get]

	# test the variable usage in the expressions
	foreach var $cols {
	    set $var 1
	}
	if {"$size" == ""} {
	    if {[llength $cols]} {
		error "Please specify a value for size (expr or const) in $units"
	    }
	    return
	}
	foreach var {size ratio angle cond} {
	    set v [set $var]
	    if {"$v" != ""} {
		if {[catch {expr [set $var]} msg]} {
		    error "in $var expr: $msg"
		}
	    }
	}
	
	# label may also contain col name vars, but may not be numeric
	if {[catch {subst $label} msg]} {
	    error "error in label '$label': $msg"
	}

	return [list $cols $symbol $color $ratio $angle $label $cond $size $units]
    }

    
    # add a new row to the table

    public method add_row {} {
	if {[catch {set row [get_row]} msg]} {
	    error_dialog $msg
	    return
	}
	if {"$row" != ""} {
	    # no duplicates
	    foreach i [$table_ get_contents] {
		if {"$i" == "$row"} {
		    info_dialog "To add a new symbol, change some parameters and press Add Symbol"
		    return [update_table]
		}
	    }
	    $table_ add_row $row
	    $table_ select_row end
	    select_row
	    incr save_needed_
	}
    }


    # remove the selected row from the table

    public method remove_row {} {
	$table_ remove_selected 
	select_row
	incr save_needed_
    }

    
    # apply the changes and close the window

    public method ok {} {
	apply
	quit
    }


    # add the dialog button frame

    protected method add_dialog_buttons {} {
	# dialog buttons
	pack [frame $w_.buttons -borderwidth 2 -relief raised] \
	    -side top -fill x
	pack \
	    [button $w_.ok \
		 -text "OK" \
		 -command [code $this ok]] \
	    [button $w_.apply \
		 -text "Apply" \
		 -command [code $this apply]] \
	    [button $w_.cancel \
		 -text "Cancel" \
		 -command [code $this cancel]] \
	    [button $w_.add \
		 -text "Add Symbol" \
		 -command [code $this add_row]] \
	    [button $w_.remove \
		 -text "Remove Symbol" \
		 -state disabled \
		 -command [code $this remove_row]] \
	    -side left -expand 1 -padx 6m -pady 2m -in $w_.buttons
    }

    
    # called when the cancel button is pressed. Delete the window so that
    # it will be reinitialized next time

    public method cancel {} {
	destroy $w_
    }


    # add a short help window and set the help texts
    
    protected method make_short_help {} {
	TopLevelWidget::make_short_help

	add_short_help $table_ {Table listing symbols to plot, one symbol per row, click to select}
	add_short_help $symbol_ {Symbol to use to plot stars or other objects}
	add_short_help $color_ {Color in which to draw the symbol}
	add_short_help $ratio_ {Ratio of width to height, expr using column names as variables}
	add_short_help $angle_ {Angle of rotation for symbol, expr using column names as variables}
	add_short_help $label_ {Label for symbol, using column names as variables}
	add_short_help $cond_ {Condition for displaying symbol, using column names as variables}
	add_short_help $size_ {Size of symbol, expression using column names as variables}
	add_short_help $units_ {Units in which to interpret the size of the symbol}
	add_short_help $left_ {List of column names used as variables for Size, Ratio, Angle, Label, Condition}
	add_short_help $right_ {List of unused column names}
	add_short_help $w_.ok {OK: Apply changes and close window}
	add_short_help $w_.apply {Apply the current changes}
	add_short_help $w_.cancel {Cancel changes and Close window}
	add_short_help $w_.add {Add the current entry as a new plot symbol (must be unique)}
	add_short_help $w_.remove {Remove the selected plot symbol entry}
    }


    # -- options --

    # command to eval when done
    public variable command {}

    # catalog name
    public variable catalog {none}

    # astrocat (C++ based) object
    public variable astrocat

    # list of column headings for the catalog
    public variable columns {}

    # font to use for labels
    itk_option define -labelfont labelFont LabelFont -Adobe-helvetica-bold-r-normal-*-12*

    # font to use for values
    itk_option define -valuefont valueFont ValueFont -adobe-courier-medium-r-*-*-*-120-*-*-*-*-*-*

    # set the width for  displaying labels
    itk_option define -labelwidth labelWidth LabelWidth 8

    # color list for color menu
    itk_option define -colors colors Colors {
	white black grey yellow red green blue lightblue pink 
	purple orange violet navy turquoise cyan gold tan lavender 
	bisque chartreuse
    } 

    # set the anchor for labels
    itk_option define -anchor anchor Anchor e

    
    # -- protected vars --

    # TableList widget displaying plot info
    protected variable table_
    
    # symbol shape menu
    protected variable symbol_

    # symbol color menu
    protected variable color_
    
    # symbol ratio entry
    protected variable ratio_

    # symbol angle entry
    protected variable angle_

    # symbol label entry
    protected variable label_

    # symbol condition entry
    protected variable cond_

    # symbol size entry 
    protected variable size_
    
    # symbol units entry 
    protected variable units_

    # left column selection listbox
    protected variable left_

    # right column selection listbox
    protected variable right_

    # table row last selected
    protected variable sel_ {}

    # set to true if the catalog configuration has been edited and needs to be saved
    protected variable save_needed_ 0
}
