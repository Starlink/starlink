proc p4Plot {taskname} {
#+
# Sends "obey lut" and "obey plot" to the p4 task.
#-
    global P4Widgets
    global P4NoticeBoard

# Get the nbsroot name of the notice board items and save present values
    cgs4drCursor pirate orange black
    set port $P4Widgets(PORT_NO)
    set nbsroot ${P4NoticeBoard}.port_${port}
    if {$P4Widgets(RESETPLOT) == 1} {p4GetNbs $nbsroot}

# Check that there is a data set specified.
    set data [string trim [$P4Widgets(DATA) get]]
    if {$data=="" || [string tolower $data]==".p4.frame.df.entry"} {
        cgs4drClear $taskname
        cgs4drInform $taskname "p4Plot error : You must specify a data set name!"
        if {$P4Widgets(RESETPLOT) == 1} {p4PutNbs $nbsroot}
        cgs4drCursor arrow green black
	return
    } else {
      nbs put ${nbsroot}.display_data [format "%80s" " "]
      nbs put ${nbsroot}.display_data $data
    }

# Set autoscaling options
    nbs put ${nbsroot}.autoscale $P4Widgets(AUTOSCALE)
    if {$P4Widgets(AUTOSCALE) == 0} {
      nbs put ${nbsroot}.high [$P4Widgets(HIGH) get]
      nbs put ${nbsroot}.low [$P4Widgets(LOW) get]
    }

# Set array limits.
    nbs put ${nbsroot}.plot_whole $P4Widgets(PLOT_WHOLE)
    if {$P4Widgets(PLOT_WHOLE) == 0} {
      nbs put ${nbsroot}.xstart [$P4Widgets(XSTART) get]
      nbs put ${nbsroot}.xend [$P4Widgets(XEND) get]
      nbs put ${nbsroot}.ystart [$P4Widgets(YSTART) get]
      nbs put ${nbsroot}.yend [$P4Widgets(YEND) get]
    }

# Set the plot style.
    nbs put ${nbsroot}.display_type [format "%80s" " "]
    nbs put ${nbsroot}.display_type [string toupper $P4Widgets(DISPLAY_TYPE)]

# Set any style specific options.
    switch [string toupper $P4Widgets(DISPLAY_TYPE)] {
        IMAGE {
          set message "Plotting $data in port $port as an $P4Widgets(DISPLAY_TYPE)"
        }
        SURFACE {
          set message "Plotting $data in port $port as a pseudo-$P4Widgets(DISPLAY_TYPE)"
        }
	HISTOGRAM {
          set message "Plotting $data in port $port as a $P4Widgets(DISPLAY_TYPE)"
	  nbs put ${nbsroot}.histogram_xstep [$P4Widgets(HISTOGRAM_XSTEP) get]
	  nbs put ${nbsroot}.histogram_ystep [$P4Widgets(HISTOGRAM_YSTEP) get]
	  nbs put ${nbsroot}.histogram_bins [$P4Widgets(HISTOGRAM_BINS) get]
	  nbs put ${nbsroot}.hist_smooth [$P4Widgets(HIST_SMOOTH) get]
	}
	CONTOUR {
          set message "Plotting $data in port $port as a $P4Widgets(DISPLAY_TYPE) map"
	  nbs put ${nbsroot}.contour_levels [$P4Widgets(CONTOUR_LEVELS) get]
          nbs put ${nbsroot}.contour_type [format "%80s" " "]
	  nbs put ${nbsroot}.contour_type $P4Widgets(CONTOUR_TYPE)
	}
	OVERGRAPH {
          set message "Plotting $data in port $port as an $P4Widgets(DISPLAY_TYPE)"
	  nbs put ${nbsroot}.plot_errors $P4Widgets(PLOT_ERRORS)
          nbs put ${nbsroot}.cut_direction [format "%80s" " "]
	  nbs put ${nbsroot}.cut_direction $P4Widgets(CUT_DIRECTION)
	  nbs put ${nbsroot}.slice_start [$P4Widgets(SLICE_START) get]
	  nbs put ${nbsroot}.slice_end [$P4Widgets(SLICE_END) get]
          nbs put ${nbsroot}.overcolour [format "%80s" " "]
	  nbs put ${nbsroot}.overcolour $P4Widgets(OVERCOLOUR)
	}
	GRAPH {
          set message "Plotting $data in port $port as a $P4Widgets(DISPLAY_TYPE) ignoring colour selection"
	  nbs put ${nbsroot}.plot_errors $P4Widgets(PLOT_ERRORS)
          nbs put ${nbsroot}.cut_direction [format "%80s" " "]
	  nbs put ${nbsroot}.cut_direction $P4Widgets(CUT_DIRECTION)
	  nbs put ${nbsroot}.slice_start [$P4Widgets(SLICE_START) get]
	  nbs put ${nbsroot}.slice_end [$P4Widgets(SLICE_END) get]
         #nbs put ${nbsroot}.overcolour [format "%80s" " "]
	 #nbs put ${nbsroot}.overcolour $P4Widgets(OVERCOLOUR)
	 #nbs put ${nbsroot}.fg_colour $P4Widgets(OVERCOLOUR)
	}
    }
    cgs4drInform $taskname $message

# Set the display plane and data.
    nbs put ${nbsroot}.display_plane [format "%80s" " "]
    nbs put ${nbsroot}.display_plane $P4Widgets(DISPLAY_PLANE)

# Get the data set name and plot it.
    if {[$taskname path] != 0} {
      cgs4drCursor watch red white
      set displut -1
      $taskname obey lut "port=$port" -inform "cgs4drInform $taskname %V" -endmsg {set displut 1}
      tkwait variable displut
      set dispstat -1
      $taskname obey display "data=$data port=$port" -inform "cgs4drInform $taskname %V" -endmsg {set dispstat 1}
      tkwait variable dispstat
    } else {
      cgs4drClear $taskname
      cgs4drInform $taskname "p4Plot error : No path to the plotting task - reload software?!"
    }

# Restore the previous nbs items and exit script
    if {[nbs get ${nbsroot}.plot_ok] == 1} {
      nbs put ${nbsroot}.last_type [format "%80s" " "]
      nbs put ${nbsroot}.last_type $P4Widgets(DISPLAY_TYPE)
    } else {
      nbs put ${nbsroot}.last_type [format "%80s" " "]
      nbs put ${nbsroot}.last_type UNKNOWN
    }
    if {$P4Widgets(RESETPLOT) == 1} {p4PutNbs $nbsroot}
    cgs4drCursor arrow green black
}
