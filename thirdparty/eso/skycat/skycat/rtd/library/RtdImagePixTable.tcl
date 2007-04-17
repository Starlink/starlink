#*******************************************************************************
# E.S.O. - VLT project
#
# "@(#) $Id: RtdImagePixTable.tcl,v 1.1.1.1 2006/01/12 16:38:24 abrighto Exp $"
#
# RtdImagePixTable.tcl - itcl widget for displaying a table of pixel values
#                        for an RtdImage widget
# 
# See man page RtdImagePixTable(n) for a complete description.
#
# 
# who             when       what
# --------------  ---------  ----------------------------------------
# Allan Brighton  01 Jun 95  Created
# Peter Biereichel 22/07/97  Added statistics
# Peter W. Draper  24/04/03  Added high-lighting for maximum and minimums

itk::usual RtdImagePixTable {}

# This widget displays a variable sized table of raw image pixel
# values from an RtdImage widget with the given pixel at the
# center.  This is meant to be bound to mouse motion events to
# display pixel values as the mouse moves across the image.

itcl::class rtd::RtdImagePixTable {
    inherit util::TopLevelWidget


    #  constructor: create a new instance of this class

    constructor {args} {
	eval itk_initialize $args

	# Tk label "Pixel Table"
	itk_component add label {
	    label $w_.label -text "Pixel Table"
	}
	pack $itk_component(label) -side top
	make_table
	make_buttons

	global ::pixtstat
	if {![info exists pixtstat]} {
	    make_statistics
	    set pixtstat 1
	} elseif {$pixtstat} {
	    make_statistics
	    set pixtstat 1
	}

	$image_ pixtab start $itk_option(-nrows) $itk_option(-ncols)
	$image_ zoom slow
    }
    
    
    # destructor 
    
    destructor {
	$image_ pixtab stop
	$image_ zoom fast
    }


    # make the table of pixel values with the X,Y coords at the
    # top and left resp.

    protected method make_table {} {
	set var $image_
	global ::$var

	# BLT table frame
	itk_component add tab {
	    set f [frame $w_.tab]
	}
	pack $f -side top -fill both -expand 1
	blt::table $f
	set ncols $itk_option(-ncols)
	set nrows $itk_option(-nrows)
        for {set col 0} {$col <= $ncols} {incr col} {
            set trow $nrows
            for {set row 0} {$row <= $nrows} {incr row} {
		blt::table $f \
		    [label $f.p$row,$col \
			 -font $itk_option(-valuefont) \
			 -borderwidth 1 \
			 -relief groove \
			 -width 6 \
			 -textvariable ${var}($row,$col)] \
		    $trow,$col -fill both
                incr trow -1
	    }
	}

	# use different font for X and Y values
	for {set row 0} {$row <= $nrows} {incr row} {
	    $f.p$row,0 config -font $itk_option(-labelfont) -borderwidth 2 -relief raised
	}
	for {set col 0} {$col <= $ncols} {incr col} {
	    $f.p0,$col config -font $itk_option(-labelfont) -borderwidth 2 -relief raised
	}
	
        # trace pixel showing maximum and minimum values
        trace variable ${var}(PIXTAB_MAXX) w [code $this update_max_pixel_]
        trace variable ${var}(PIXTAB_MAXY) w [code $this update_max_pixel_]
        trace variable ${var}(PIXTAB_MINX) w [code $this update_min_pixel_]
        trace variable ${var}(PIXTAB_MINY) w [code $this update_min_pixel_]

	# 0,0 unused
	$f.p0,0 config -relief flat -textvariable RtdPixTab(xy)
	set RtdPixTab(xy) {Y\X}
	
	# highlight center pixel and X,Y values
	set col [expr {$ncols/2+1}]
	set row [expr {$nrows/2+1}]
	$f.p$row,$col config -relief raised -foreground red
	$f.p0,$col config -relief raised -foreground red
	$f.p$row,0 config -relief raised -foreground red

        # standard background colour
        set bgcol_ [$f.p$row,$col cget -background]

	blank_values
	add_short_help $itk_component(tab) {Shows pixels around the last cursor position in the image}
    }

    # statistics on pixels
	
    protected method make_statistics {} {
	set var $image_
	global ::$var
	set ncols $itk_option(-ncols)
	set nrows $itk_option(-nrows)
	
	# Tk label "Statistics"
	itk_component add slabel {
	    label $w_.slabel -text "Statistics"
	}
	pack $itk_component(slabel) -side top -before $itk_component(buttons)

	# statistics Tk frame
	itk_component add stat {
	    set sf [frame $w_.stat -relief raised -borderwidth 1]
	}
	pack $sf -side top -fill both -expand 1 -before $itk_component(buttons)
	set col 0
	set row 1
	foreach el {Min Max Ave RMS N} {
	    set lel [string tolower $el]
	    # LabelValue(n) widgets: pixtab_Min, pixtab_Max, pixtab_Ave, 
	    # pixtab_RMS, pixtab_N
	    itk_component add pixtab_$lel {
		util::LabelValue $sf.$lel \
		    -text "$el:" \
		    -textvariable ${var}(PIXTAB_[string toupper $el]) \
		    -labelfont $itk_option(-labelfont) \
		    -valuefont $itk_option(-valuefont) \
		    -labelwidth $itk_option(-labelwidth) \
		    -valuewidth $itk_option(-valuewidth) \
		    -relief groove \
		    -anchor w}
	    blt::table $sf $itk_component(pixtab_$lel) \
		$row,$col -fill both
	    incr col
	    if {$col >= $ncols} {
		set col 0
		incr row
	    }
	}

        # Max and min match highlight colours as visual clue
        $itk_component(pixtab_max) configure \
           -foreground $itk_option(-maxhighlight)
        $itk_component(pixtab_min) configure \
           -foreground $itk_option(-minhighlight)

	add_short_help $itk_component(pixtab_min) {Min: Shows the min value of the pixel table}
	add_short_help $itk_component(pixtab_max) {Min: Shows the max value of the pixel table}
	add_short_help $itk_component(pixtab_ave) {Min: Shows the average value of the pixel table}
	add_short_help $itk_component(pixtab_rms) {Min: Shows the RMS value of the pixel table}
	add_short_help $itk_component(pixtab_n) {N: Shows the number of pixels in the pixel table}
    }

    # set the background of one of the tabel cells
    protected method set_cell_bg_colour_ {x y colour} {
       catch {
          $itk_component(tab).p${x},${y} configure -background $colour
       }
    }

    # update the cell coloured to show that it has the maximum value    
    protected method update_max_pixel_ { args } {
       set var $image_
       global ::$var
       set_cell_bg_colour_ $maxx_ $maxy_ $bgcol_
       set maxx_ [set ${var}(PIXTAB_MAXX)]
       set maxy_ [set ${var}(PIXTAB_MAXY)]
       set_cell_bg_colour_ $maxx_ $maxy_ $itk_option(-maxhighlight)
    }

    # update the cell coloured to show that it has the minimum value    
    protected method update_min_pixel_ { args } {
       set var $image_
       global ::$var
       set_cell_bg_colour_ $minx_ $miny_ $bgcol_
       set minx_ [set ${var}(PIXTAB_MINX)]
       set miny_ [set ${var}(PIXTAB_MINY)]
       set_cell_bg_colour_ $minx_ $miny_ $itk_option(-minhighlight)
    }

    # make the button frame at the bottom of the window

    protected method make_buttons {} {
	# button frame
	itk_component add buttons {
	    frame $w_.buttons -borderwidth 0 -relief flat}
	   
	pack $itk_component(buttons) -side top -fill none -expand 0
	pack \
	    [button $w_.close \
		 -text "Close" \
		 -command [code delete object $this]] \
	    -padx 2m -pady 2m -side right -in $w_.buttons

	global ::pixtstat
	pack \
	    [checkbutton $w_.statistics \
		 -text "Statistics" \
		 -variable pixtstat \
		 -anchor w \
		 -font $itk_option(-labelfont) \
		 -command [code $this statistics]] \
	    -padx 2m -pady 2m -side left -in $w_.buttons

	add_short_help $w_.close {Close: {bitmap b1} = Close this window}
	add_short_help $w_.statistics {Statistics: {bitmap b1} = Switch statistics info on/off}
    }

    # method to switch statistics window on/off

    public method statistics {} {
	global ::pixtstat
	if {$making_stat_} {
	    if {[winfo exists $itk_component(stat)]} {
		set pixtstat 1
	    } else {
		set pixtstat 0
	    }
	    return
	}
	set making_stat_ 1
	if {$pixtstat} {
	    blank_values
	    catch {make_statistics}
	    update idletasks
	} else {
	    catch {
		destroy $itk_component(slabel)
		destroy $itk_component(stat)
	    }
	}
	set making_stat_ 0
    }

    # method to blank out all pixel values

    public method blank_values {} {
	set var $image_
	global ::$var
	set ncols $itk_option(-ncols)
	set nrows $itk_option(-nrows)
	for {set col 0} {$col <= $ncols} {incr col} {
	    for {set row 0} {$row <= $nrows} {incr row} {
		set ${var}($row,$col) ""
	    }
	}
    }

    # -- public vars --

    # caller's RtdImage itcl object
    itk_option define -image image Image {} {
	# get internal image object and canvas
	set image_ [$itk_option(-image) get_image]
	set canvas_ [$itk_option(-image) get_canvas]
    }

    # number of rows/columns of pixels to display
    itk_option define -nrows nrows Nrows 3
    itk_option define -ncols ncols Ncols 3

    # fonts used
    itk_option define -labelfont labelFont LabelFont -Adobe-helvetica-bold-r-normal-*-12*
    itk_option define -valuefont valueFont ValueFont -Adobe-helvetica-medium-r-normal-*-12*

    # set the width for displaying labels and values
    itk_option define -labelwidth labelWidth LabelWidth 4
    itk_option define -valuewidth valueWidth ValueWidth 8

    # maximum and minimum highlight colours
    itk_option define -maxhighlight maxhighlight MaxHighlight lightblue
    itk_option define -minhighlight minhighlight MinHighlight lightgreen

    # -- protected vars --

    # internal rtdimage object
    protected variable image_

    # canvas for image
    protected variable canvas_

    # flag for "making statistics widget"
    protected variable making_stat_ 0

    # indices of last cells to hold the maximum and minimum colours
    protected variable maxx_ 1
    protected variable maxy_ 1
    protected variable minx_ 1
    protected variable miny_ 1

    # background colour of a cell that isn't highlighted
    protected variable bgcol_ lightgrey
}
