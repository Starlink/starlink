#!/bin/sh
# the next line restarts using a special startup script to setup the env \
exec ../demos/rtd.sh "$0" "$@"

# Start with: ../demos/rtd.sh tRtdImageCut.tcl in order to set BLT_LIBRARY

source test.tcl

set w [RtdImage .rtd \
	   -file ../images/ngc1275.gzfits \
	   -scrollbars 1 \
	   -drag_scroll 1 \
	  ]
pack $w -fill both -expand 1
update
RtdImageCut $w.cut -image $w
