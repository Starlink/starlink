proc p4Plot {taskname} {
#+
# Sends "obey lut" and "obey plot" to the p4 task.
#-
    global P4Widgets

# Check that there is a data set specified.
    set data [string trim [$P4Widgets(DATA) get]]
    if {$data == ""} {
        cgs4drClear $taskname
	set message "p4Plot error : You must specify a data set name!"
        cgs4drInform $taskname $message
	return
    }

# Get the root name of the notice board items
    cgs4drCursor pirate orange black
    global P4NoticeBoard
    set port $P4Widgets(PORT_NO)
    set root ${P4NoticeBoard}.port_${port}.

# Set autoscaling options
    set autoscale $P4Widgets(AUTOSCALE)
    nbs put ${root}autoscale $autoscale
    if {$autoscale == 0} {
	set high [$P4Widgets(HIGH) get]
	nbs put ${root}high $high
	set low [$P4Widgets(LOW) get]
	nbs put ${root}low $low
    }

# Set array limits.
    set plot_whole $P4Widgets(PLOT_WHOLE)
    nbs put ${root}plot_whole $plot_whole
    if {$plot_whole == 0} {
	set xstart [$P4Widgets(XSTART) get]
	nbs put ${root}xstart $xstart
	set xend [$P4Widgets(XEND) get]
	nbs put ${root}xend $xend
	set ystart [$P4Widgets(YSTART) get]
	nbs put ${root}ystart $ystart
	set yend [$P4Widgets(YEND) get]
	nbs put ${root}yend $yend
    }

# Set the plot style.
    set display_type $P4Widgets(DISPLAY_TYPE)
    nbs put ${root}display_type $display_type


# Set any style specific options.
    switch $display_type {
        IMAGE {
            set message "Plotting $data in port $port as an $display_type"
            cgs4drInform $taskname $message
        }
        SURFACE {
            set message "Plotting $data in port $port as a pseudo-$display_type"
            cgs4drInform $taskname $message
        }
	HISTOGRAM {
            set message "Plotting $data in port $port as a $display_type"
            cgs4drInform $taskname $message
	    set histogram_xstep [$P4Widgets(HISTOGRAM_XSTEP) get]
	    nbs put ${root}histogram_xstep $histogram_xstep
	    set histogram_ystep [$P4Widgets(HISTOGRAM_YSTEP) get]
	    nbs put ${root}histogram_ystep $histogram_ystep
	    set histogram_bins [$P4Widgets(HISTOGRAM_BINS) get]
	    nbs put ${root}histogram_bins $histogram_bins
	    set hist_smooth [$P4Widgets(HIST_SMOOTH) get]
	    nbs put ${root}hist_smooth $hist_smooth
	}
	CONTOUR {
            set message "Plotting $data in port $port as a $display_type map"
            cgs4drInform $taskname $message
	    set contour_levels [$P4Widgets(CONTOUR_LEVELS) get]
	    nbs put ${root}contour_levels $contour_levels
	    set contour_type $P4Widgets(CONTOUR_TYPE)
	    nbs put ${root}contour_type $contour_type
	}
	OVERGRAPH {
            set message "Plotting $data in port $port as an $display_type"
            cgs4drInform $taskname $message
	    set plot_errors $P4Widgets(PLOT_ERRORS)
	    nbs put ${root}plot_errors $plot_errors
	    set cut_direction $P4Widgets(CUT_DIRECTION)
	    nbs put ${root}cut_direction $cut_direction
	    set slice_start [$P4Widgets(SLICE_START) get]
	    nbs put ${root}slice_start $slice_start
	    set slice_end [$P4Widgets(SLICE_END) get]
	    nbs put ${root}slice_end $slice_end
	    set overcolour $P4Widgets(OVERCOLOUR)
	    nbs put ${root}overcolour $overcolour
	}
	GRAPH {
            set message "Plotting $data in port $port as a $display_type ignoring colour selection"
            cgs4drInform $taskname $message
	    set plot_errors $P4Widgets(PLOT_ERRORS)
	    nbs put ${root}plot_errors $plot_errors
	    set cut_direction $P4Widgets(CUT_DIRECTION)
	    nbs put ${root}cut_direction $cut_direction
	    set slice_start [$P4Widgets(SLICE_START) get]
	    nbs put ${root}slice_start $slice_start
	    set slice_end [$P4Widgets(SLICE_END) get]
	    nbs put ${root}slice_end $slice_end
	   #set fg_colour $P4Widgets(OVERCOLOUR)
	   #nbs put ${root}fg_colour $fg_colour
	}
    }

# Set the display plane.
    set display_plane $P4Widgets(DISPLAY_PLANE)
    nbs put ${root}display_plane $display_plane

# Get the data set name and plot it.
    if {[$taskname path] != 0} {
      cgs4drCursor watch red white
      $taskname obey lut "port=$port" -inform "cgs4drInform $taskname %V"
      $taskname obey display "data=$data port=$port" -inform "cgs4drInform $taskname %V"
    } else {
      cgs4drClear $taskname
      set message "No path to the plotting task - reload software?!"
      cgs4drInform $taskname $message
    }
    cgs4drCursor arrow green black
}
