#!/bin/sh
# the next line restarts using a special startup script to setup the env \
exec ../demos/rtd.sh "$0" "$@"

source test.tcl

# for debugging: print all errors on stderr 
catch {tkerror}
rename tkerror tkerror__
proc tkerror {msg} {
    global errorInfo
    puts stderr "$errorInfo"
    tkerror__ "error: $msg"
}


set w [RtdImage .rtd \
	   -file ../images/ngc1275.gzfits \
	   -canvaswidth 100 -canvasheight 100 \
	   -scrollbars 1 \
	   -drag_scroll 1 \
	  ]
pack $w -fill both -expand 1

tkwait visibility $w

puts "getting shared memory id..."
puts "id = [[$w get_image] shm get data]"

[$w component draw] center_window

update
puts "image name: [$w get_image], canvas name: [$w component canvas]"
puts "image cget -file: [[$w get_image] cget -file]"
puts "canvas config: [[$w component canvas] config]"

