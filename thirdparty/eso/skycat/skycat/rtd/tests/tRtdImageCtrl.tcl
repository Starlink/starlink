source test.tcl

set w [RtdImageCtrl .rtd \
	   -scrollbars 1 \
	   -drag_scroll 1 \
	   -with_zoom_window 1 \
	   -with_pan_window 1 \
	   -with_colorramp 1 \
	   -zoom_view_propagate 1 \
	   -pan_width 180 \
	   -pan_height 180 \
	  ]
pack $w -fill both -expand 1

update

$w config -file ../images/ngc1275.fits

#after 2000 [code [$w component pan] config -width 200 -height 200]
#after 4000 [code [$w component zoom] config -width 200 -height 200]

#[$w component draw] center_window

#update
#puts "image name: [$w get_image], canvas name: [$w component canvas]"
#puts "image cget -file: [[$w get_image] cget -file]"
#puts "canvas config: [[$w component canvas] config]"

