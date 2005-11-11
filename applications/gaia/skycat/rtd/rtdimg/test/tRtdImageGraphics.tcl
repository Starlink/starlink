source test.tcl

set w [RtdImage .rtd \
	   -file ../images/ngc1275.fits \
	   -canvaswidth 100 -canvasheight 100 \
	   -scrollbars 1 \
	   -drag_scroll 1 \
	  ]
pack $w -fill both -expand 1

tkwait visibility $w

#puts "getting shared memory id..."
#puts "id = [[$w get_image] shm get data]"

[$w component draw] center_window

update
puts "image name: [$w get_image], canvas name: [$w component canvas]"
puts "image cget -file: [[$w get_image] cget -file]"
puts "canvas config: [[$w component canvas] config]"

