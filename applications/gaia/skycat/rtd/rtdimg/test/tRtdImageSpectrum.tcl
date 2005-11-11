proc make_spectrum {line_id x0 y0 x1 y1} {
    
    RtdImageSpectrum .rtd.spectrum \
	    -x0 [expr int($x0)] \
	    -y0 [expr int($y0)] \
	    -x1 [expr int($x1)] \
	    -y1 [expr int($y1)] \
	    -image .rtd \
	    -transient 1 \
	    -line_id $line_id
}

source test.tcl

set w [RtdImage .rtd \
	   -file ../images/ngc1275.fits \
	   -scrollbars 1 \
	   -drag_scroll 1 \
           -graphics 1 \
	  ]
pack $w -fill both -expand 1
update

.rtd.draw set_drawing_mode line make_spectrum
