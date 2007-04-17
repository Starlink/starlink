source test.tcl

set w [RtdImage .rtd \
	   -file ../images/ngc1275.fits \
	   -scrollbars 1 \
	   -drag_scroll 1 \
	  ]
pack $w -fill both -expand 1
update
RtdImagePick $w.pick -target_image $w

 
