#*******************************************************************************
# E.S.O. - VLT project
#
# "@(#) $Id$"
#
# CanvasPrint.tcl - popup dialog for printing an RTD image
#
# who             when       what
# --------------  ---------  ----------------------------------------
# Allan Brighton  01 Jun 95  Created
# P. Biereichel   05/08/97   Modified for xgrabsc (screen dump)
# Allan Brighton  21 Nov 97  Renamed to RtdImagePrint, since it is
#                            now rtd specific, pass image as option.
# Peter W. Draper 14 May 98  Added changes to use Canvas postscript
#                            printing with a suitably patched Tk.
#                            Now only get a footer.

itk::usual CanvasPrint {}

# CanvasPrint defines a popup dialog for printing an RTD image.
# This class extends the PrintDialog class.

class util::CanvasPrint {
    inherit util::PrintDialog


    # constructor 

    constructor {args} {
	eval itk_initialize $args
    }


    # this method is called after all options have been evaluated

    protected method init {} {
	util::PrintDialog::init

	wm title $w_ "Postscript print"

	global ::$w_.color ::$w_.rotate ::$w_.fit_to_page ::$w_.footer \
           ::$w_.whole

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
	::set $w_.color gray

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

        #  Capture whole of displayed canvas.
        pack [frame $w_.whole -borderwidth 5] \
            -side top -fill x -expand 1 -in $w_.options
        pack [checkbutton $w_.whole.yes -text "Print all image window" \
                 -variable $w_.whole \
                 -onvalue 1 -offvalue 0 \
                 -command [code $this set_whole_canvas] ] \
            -side left

	# page size options 
	pack [frame $w_.pagesize -borderwidth 5] \
		-side top -fill x -expand 1 -in $w_.options
	checkbutton $w_.pagesize.fit -text "Encapsulated Postscript" \
	    -variable $w_.fit_to_page \
	    -command [code $this toggle_fit_pagesize]
	checkbutton $w_.pagesize.footer -text "Footer text" \
	    -variable $w_.footer
	LabelEntry $w_.pagesize.width \
	     -text "Page width " \
	     -value $itk_option(-pagewidth) \
	     -valuewidth 6
	LabelEntry $w_.pagesize.height \
	     -text "Page height" \
	     -value $itk_option(-pageheight) \
	     -valuewidth 6
	blt::table $w_.pagesize \
	    $w_.pagesize.fit      1,0 -anchor w \
	    $w_.pagesize.footer   1,1 -anchor e \
	    $w_.pagesize.width    2,0 -anchor w \
	    $w_.pagesize.height   2,1 -anchor e

	::set $w_.fit_to_page $itk_option(-fit_to_page)
	::set $w_.footer $itk_option(-show_footer)

	add_short_help
    }


    # add short help texts

    protected method add_short_help {} {
	global ::env
	set tp [winfo parent $w_]

	$tp add_short_help $w_.color.color \
	    "Write output in Postscript format for color printers"
	$tp add_short_help $w_.color.gray \
	    "Write output in Postscript format for greyscale printers"
	$tp add_short_help $w_.color.mono \
	    "Write output in Postscript format in black and white"

	$tp add_short_help $w_.rotate.yes \
	    "Use landscape layout (with page width and height exchanged) for \
	     Postscript output"
	$tp add_short_help $w_.rotate.no \
	    "Use portrait layout for Postscript output"

	$tp add_short_help $w_.pagesize.width \
	    "Paper width in inches (default for A4: $itk_option(-pagewidth))"
	$tp add_short_help $w_.pagesize.height \
	    "Paper height in inches (default for A4: $itk_option(-pageheight))"

	$tp add_short_help $w_.pagesize.fit \
	    "Encapsulated Postscript for including the image into a document \
             (e.g. LaTeX/FrameMaker). No translation or scaling!"

	$tp add_short_help $w_.whole.yes \
           "Print whole of main image region (needed to keep off-image \
            graphics, e.g. grid plots)"

	$tp add_short_help $w_.pagesize.footer "Add footer to image"
    }

    
    # called when the "Fit on page" button is pressed

    protected method toggle_fit_pagesize {} {
	global ::$w_.fit_to_page
	if {![set $w_.fit_to_page]} {
	    $w_.pagesize.width config -state normal
	    $w_.pagesize.height config -state normal
	} else {
	    $w_.pagesize.width config -state disabled
	    $w_.pagesize.height config -state disabled
	}
    }


    # print the contents of the canvas to the open filedescriptor

    method print {fd} {
	global ::$w_.color ::$w_.rotate $w_.colormap ::$w_.footer
	global ::$w_.fit_to_page

	set cmd [list $itk_option(-canvas) postscript \
		     -colormode [set $w_.color] \
		     -rotate [set $w_.rotate]]

       #  Get the offsets that correct for the apparent shift of the
       #  image when zoomed.
       set xoff [expr int([$itk_option(-canvas) canvasx 0])]
       set yoff [expr int([$itk_option(-canvas) canvasy 0])]
       set xoff [max $xoff 0]
       set yoff [max $yoff 0]

       if { $itk_option(-x0) == {} } {

          # no prefered canvas section so adjust things to the size
          # of the displayed rtd image if available. If not then
          # use a bounding box that encompasses all the displayed items.

          if { $itk_option(-rtdimage) != {} && !$itk_option(-whole_canvas) } {

             #  The origin of the image is always 0,0 for a canvas
             #  print to work.
             set x0 0
             set y0 0
             set x1 [min [winfo width $itk_option(-canvas)] [$itk_option(-rtdimage) dispwidth]]
             set y1 [min [winfo height $itk_option(-canvas)] [$itk_option(-rtdimage) dispheight]]
          } else {

             #  Use whole printing surface.
             set x0 [min 0 [$itk_option(-canvas) canvasx 0]]
             set y0 [min 0 [$itk_option(-canvas) canvasy 0]]
             set x1 [expr $x0+[winfo width $itk_option(-canvas)]]
             set y1 [expr $y0+[winfo height $itk_option(-canvas)]]
          }
       }

       #  Set the background (use a filled rectangle to simulate this).
       if { $itk_option(-whole_canvas) } {
          set_background
       }

       #  Now add footer.
       if {[set $w_.footer]} {
          add_footer
       }

       #  Set the width, height and corner.
       lappend cmd \
           -width [expr $x1-$x0] \
           -height [expr $y1-$y0] \
           -x $x0 \
           -y $y0

       if {! [set $w_.fit_to_page]} {
          lappend cmd \
             -pagewidth [$w_.pagesize.width get] \
             -pageheight [$w_.pagesize.height get]
       }

       if {"[set $w_.color]" == "mono"} {
          # you can add to this array, see canvas(n) man page
          set $w_.colormap(grey) "0.0 0.0 0.0 setrgbcolor"
          lappend cmd -colormap $w_.colormap
       }

       #  Shift all canvas items so that they align to the image
       #  with its origin of 0,0.
       $itk_option(-canvas) move all [expr -1.0*$xoff] [expr -1.0*$yoff]
       $itk_option(-canvas) move $itk_option(-rtdimage) $xoff $yoff
       $itk_option(-canvas) move print $xoff $yoff

       #  Write postscript into file stream.
       puts $fd [eval $cmd]

       #  Shift everything back to original position.
       $itk_option(-canvas) move all $xoff $yoff
       $itk_option(-canvas) move $itk_option(-rtdimage) \
           [expr -1.0*$xoff] [expr -1.0*$yoff]

       #  Remove footer.
       if {[set $w_.footer]} {
          rm_footer
       }

       #  Remove background.
       if { $itk_option(-whole_canvas) } {
          remove_background
       }
    }

    # add footer labels below draw area by temporarily inserting
    # the text

    method add_footer {} {
       set hy0 [expr $y1+25]
       set hy1 [expr $y1+50]

       # white background for labels
       $itk_option(-canvas) create rect $x0 $y1 $x1 $hy1 \
          -outline white \
          -fill white \
          -tags print

       set hy0 [expr $y1+20]
       set hy1 [expr $y1+40]

       if {"$itk_option(-top_left)" != ""} {
          $itk_option(-canvas) create text $x0 $hy0 \
             -text $itk_option(-top_left) \
             -font $itk_option(-footer_font) \
             -anchor sw \
             -tags print
       }
       if {"$itk_option(-top_right)" != ""} {
          $itk_option(-canvas) create text $x1 $hy0 \
             -text $itk_option(-top_right) \
             -font $itk_option(-footer_font) \
             -anchor se \
             -tags print
       }
       if {"$itk_option(-bot_left)" != ""} {
          $itk_option(-canvas) create text $x0 $hy1 \
             -text $itk_option(-bot_left) \
             -font $itk_option(-footer_font) \
             -anchor nw \
             -tags print
       }
       if {"$itk_option(-bot_right)" != ""} {
          $itk_option(-canvas) create text $x1 $hy1 \
             -text $itk_option(-bot_right) \
             -font $itk_option(-footer_font) \
             -anchor ne \
             -tags print
       }
       set y1 [expr $y1+50]
    }


    # remove the footer, if any and restore the original state
    method rm_footer {} {
	$itk_option(-canvas) delete print
    }

    # modify capture all canvas items
    method set_whole_canvas {} {
       global ::$w_.whole
       configure -whole_canvas [set $w_.whole]
    }

    # modify show_footer
    method set_show_footer {} {
       global ::$w_.footer
       configure -show_footer [set $w_.footer]
    }

    # set/remove the background of the canvas.
    method set_background {} {
        set xlow  [$itk_option(-canvas) canvasx 0]
        set ylow  [$itk_option(-canvas) canvasy 0]
	set xhigh [$itk_option(-canvas) canvasx [winfo width $itk_option(-canvas)]]
	set yhigh [$itk_option(-canvas) canvasy [winfo height $itk_option(-canvas)]]
	set xlow [expr round($xlow-1)]
	set ylow [expr round($ylow-1)]
	set xhigh [expr round($xhigh+1)]
	set yhigh [expr round($yhigh+1)]
	$itk_option(-canvas) create rectangle $xlow $ylow $xhigh $yhigh \
		-fill [$itk_option(-canvas) cget -background] \
		-tags ${this}_back
	$itk_option(-canvas) lower ${this}_back all
    }
    method remove_background {} {
       $itk_option(-canvas) delete ${this}_back
    }

    # -- options --

    # name of rtdimage widget, set by caller
    itk_option define -rtdimage rtdimage RtdImage {}

    # name of canvas widget, set by caller
    itk_option define -canvas canvas Canvas {}

    # flag, it true whole canvas is captured, this includes any
    # graphics that extends outside the image.
    itk_option define -whole_canvas whole_canvas Whole_Canvas 0

    # flag, if true, scale output to fit on page
    itk_option define -fit_to_page fit_to_page Fit_to_page 0

    # page width, used when fit_to_page is 1
    itk_option define -pagewidth pageWidth Pagewidth 8.268i

    # page height, used when fit_to_page is 1
    itk_option define -pageheight pageHeight Pageheight 11.693i
    
    # flag, if true, insert footers before printing
    itk_option define -show_footer show_footer Show_footer 0

    # footer text to appear at top left
    itk_option define -top_left top_left Top_left {}

    # footer text to appear at top right
    itk_option define -top_right top_right Top_right {}

    # footer text to appear at bottom left
    itk_option define -bot_left bot_left Bot_left {}

    # footer text to appear at bottom right
    itk_option define -bot_right bot_right Bot_right {}
    
    # footer fonts
    itk_option define -footer_font footer_font Footer_font \
	{-*-courier-bold-r-*-*-10-100-*-*-*-*-*-*}

    # coordinates of area of canvas to print (default bbox all)
    itk_option define -x0 x0 X0 {} {set x0 $itk_option(-x0)}
    itk_option define -y0 y0 Y0 {} {set y0 $itk_option(-y0)}
    itk_option define -x1 x1 X1 {} {set x1 $itk_option(-x1)}
    itk_option define -y1 y1 Y1 {} {set y1 $itk_option(-y1)}


    # -- protected variables --
    protected variable x0 {}
    protected variable y0 {}
    protected variable x1 {}
    protected variable y1 {}
}
