# E.S.O. - VLT project/ ESO Archive
# "@(#) $Id: CanvasPrint.tcl,v 1.2 2005/02/02 01:43:02 brighton Exp $"
#
# CanvasPrint.tcl - Popup dialog for printing the contents of a Tk canvas
#
# who             when       what
# --------------  ---------  ----------------------------------------
# Allan Brighton  01 Jun 94  Created

itk::usual CanvasPrint {}

# This class extends the PrintDialog class to be able to 
# print the contents of a canvas as postscript.
# Note: this class does not support printing images embedded 
# in the canvas.

itcl::class util::CanvasPrint {
    inherit util::PrintDialog

    # constructor 

    constructor {args} {
	eval itk_initialize $args
    }


    # this method is called after all options have been evaluated

    protected method init {} {
	util::PrintDialog::init

	global ::$w_.color ::$w_.rotate ::$w_.fit_to_page

	pack [frame $w_.options -borderwidth 3 -relief groove] \
	    -side top -fill x -expand 1 -in $w_.config

	pack [label $w_.options.title -text "Postscript Options"] \
	    -side top 
	
	# color options 
	pack [frame $w_.color -borderwidth 5] \
	    -side top -fill x -expand 1 -in $w_.options
	pack [radiobutton $w_.color.color -text "Color" \
	      -variable $w_.color \
	      -value color] \
        [radiobutton $w_.color.gray -text "Gray-Scale" \
	     -variable $w_.color \
	     -value gray] \
        [radiobutton $w_.color.mono -text "Black & White" \
	     -variable $w_.color \
	     -value mono] \
	-side left -fill x -expand 1
	::set $w_.color color

	# rotate options 
	pack [frame $w_.rotate -borderwidth 5] \
	    -side top -fill x -expand 1 -in $w_.options
	pack [radiobutton $w_.rotate.yes -text "Landscape" \
	      -variable $w_.rotate \
	      -value yes] \
        [radiobutton $w_.rotate.no -text "Portrait" \
	     -variable $w_.rotate \
	     -value no] \
	-side left -fill x -expand 1
	::set $w_.rotate no

	# page size options 
	pack [frame $w_.pagesize -borderwidth 5] \
	    -side top -fill x -expand 1 -in $w_.options
	pack [checkbutton $w_.pagesize.fit -text "Fit on page" \
		  -variable $w_.fit_to_page \
		  -command [code $this toggle_fit_pagesize]] \
	    -side top -anchor w
	pack \
	    [LabelEntry $w_.pagesize.width \
		 -text "Page width " \
		 -value $itk_option(-pagewidth) \
		 -valuewidth 6] \
	    [LabelEntry $w_.pagesize.height \
		 -text "Page height" \
		 -value $itk_option(-pageheight) \
		 -valuewidth 6] \
	    -side left -expand 1
	::set $w_.fit_to_page $itk_option(-fit_to_page)
    }

    
    # called when the "Fit on page" button is pressed

    protected method toggle_fit_pagesize {} {
	global ::$w_.fit_to_page
	if {[set $w_.fit_to_page]} {
	    $w_.pagesize.width config -state normal
	    $w_.pagesize.height config -state normal
	} else {
	    $w_.pagesize.width config -state disabled
	    $w_.pagesize.height config -state disabled
	}
    }


    # print the contents of the canvas to the open filedescriptor
    
    protected method print {fd} {
	global ::$w_.color ::$w_.rotate ::$w_.colormap ::$w_.fit_to_page

	set cmd [list $itk_option(-canvas) postscript \
		     -colormode [set $w_.color] \
		     -rotate [set $w_.rotate]]
	
	if {"$x0" == ""} {
	    lassign [$itk_option(-canvas) bbox all] x0 y0 x1 y1
	}

	if {$itk_option(-show_headers)} {
	    add_headers
	}
	
	lappend cmd \
	    -width [expr {$x1-$x0}] \
	    -height [expr {$y1-$y0}] \
	    -x $x0 \
	    -y $y0

	if {[set $w_.fit_to_page]} {
	    lappend cmd \
		-pagewidth [$w_.pagesize.width get] \
		-pageheight [$w_.pagesize.height get]
	}

	if {"[set $w_.color]" == "mono"} {
	    # you can add to this array, see canvas(n) man page
	    set $w_.colormap(grey) "0.0 0.0 0.0 setrgbcolor"
	    lappend cmd -colormap $w_.colormap
	} 

	# puts $cmd
	puts $fd [eval $cmd]

	if {$itk_option(-show_headers)} {
	    rm_headers
	}
    }

    
    # add header and footer labels above/below draw area by temporarily inserting
    # the text 

    protected method add_headers {} {
	set hy0 [expr {$y0-25}]
	set hy1 [expr {$y1+25}]
	
	# white background for labels
	$itk_option(-canvas) create rect $x0 $hy0 $x1 $y0 \
	    -outline white \
	    -fill white \
	    -tags print
	$itk_option(-canvas) create rect $x0 $y1 $x1 $hy1 \
	    -outline white \
	    -fill white \
	    -tags print

	set y0 [expr {$y0-5}]
	set y1 [expr {$y1+5}]

	if {"$itk_option(-top_left)" != ""} {
	    $itk_option(-canvas) create text $x0 $y0 \
		-text $itk_option(-top_left) \
		-font $itk_option(-header_font) \
		-anchor sw \
		-tags print
	}
	if {"$itk_option(-top_right)" != ""} {
	    $itk_option(-canvas) create text $x1 $y0 \
		-text $itk_option(-top_right) \
		-font $itk_option(-header_font) \
		-anchor se \
		-tags print
	}
	if {"$itk_option(-bot_left)" != ""} {
	    $itk_option(-canvas) create text $x0 $y1 \
		-text $itk_option(-bot_left) \
		-font $itk_option(-header_font) \
		-anchor nw \
		-tags print
	}
	if {"$itk_option(-bot_right)" != ""} {
	    $itk_option(-canvas) create text $x1 $y1 \
		-text $itk_option(-bot_right) \
		-font $itk_option(-header_font) \
		-anchor ne \
		-tags print
	}
	set y0 $hy0
	set y1 $hy1
    }


    # remove the headers, if any and restore the original state
    protected method rm_headers {} {
	$itk_option(-canvas) delete print
    }


    # -- options --

    # canvas widget
    itk_option define -canvas canvas Canvas {}

    # flag, if true, scale output to fit on page
    itk_option define -fit_to_page fit_to_page Fit_to_page 1

    # page width, used when fit_to_page is 1
    itk_option define -pagewidth pageWidth Pagewidth 8.268i

    # page height, used when fit_to_page is 1
    itk_option define -pageheight pageHeight Pageheight 11.693i
    
    # flag, if true, insert headers before printing
    itk_option define -show_headers show_headers Show_headers 0

    # header text to appear at top left
    itk_option define -top_left top_left Top_left {}

    # header text to appear at top right
    itk_option define -top_right top_right Top_right {}

    # header text to appear at bottom left
    itk_option define -bot_left bot_left Bot_left {}

    # header text to appear at bottom right
    itk_option define -bot_right bot_right Bot_right {}
    
    # header/footer fonts
    itk_option define -header_font header_font Header_font \
	{-*-courier-bold-r-*-*-10-100-*-*-*-*-*-*}

    # x0 coordinate of area of canvas to print
    itk_option define -x0 x0 X0 {} {set x0 $itk_option(-x0)}

    # y0 coordinate of area of canvas to print
    itk_option define -y0 y0 Y0 {} {set y0 $itk_option(-y0)}

    # x1 coordinate of area of canvas to print
    itk_option define -x1 x1 X1 {} {set x1 $itk_option(-x1)}

    # y1 coordinate of area of canvas to print
    itk_option define -y1 y1 Y1 {} {set y1 $itk_option(-y1)}

    # -- protected variables --
    
    # saved -x0 option value
    protected variable x0

    # saved -y0 option value
    protected variable y0

    # saved -x1 option value
    protected variable x1

    # saved -y1 option value
    protected variable y1
}


