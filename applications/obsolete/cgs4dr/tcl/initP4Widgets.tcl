proc initP4Widgets {} {
#+
# Initialises the widgets in the p4 interface by loading them with the
# values of the appropriate notice board items for the currently selected
# port.
#
# String items are trimed to remove trailing spaces.
#-
    global P4Widgets

# Get the root name of the notice board items
    global P4NoticeBoard
    set port $P4Widgets(PORT_NO)
    set root ${P4NoticeBoard}.port_${port}.

    #$P4Widgets(DATA) delete 0 end
    #$P4Widgets(DATA) insert 0 [string trim [nbs get ${root}display_data]]
    set P4Widgets(DISPLAY_PLANE) [string trim [nbs get ${root}display_plane]]
    if {[nbs get ${root}plot_whole]} {
      set P4Widgets(PLOT_WHOLE) 1
    } else {
      set P4Widgets(PLOT_WHOLE) 0
    }
    if {[nbs get ${root}autoscale]} {
      set P4Widgets(AUTOSCALE) 1
    } else {
      set P4Widgets(AUTOSCALE) 0
    }
    set P4Widgets(DISPLAY_TYPE) [string trim [nbs get ${root}display_type]]
    $P4Widgets(XSTART) delete 0 end
    $P4Widgets(XSTART) insert 0 [nbs get ${root}xstart]
    $P4Widgets(YSTART) delete 0 end
    $P4Widgets(YSTART) insert 0 [nbs get ${root}ystart]
    $P4Widgets(XEND) delete 0 end
    $P4Widgets(XEND) insert 0 [nbs get ${root}xend]
    $P4Widgets(YEND) delete 0 end
    $P4Widgets(YEND) insert 0 [nbs get ${root}yend]
    $P4Widgets(HIGH) delete 0 end
    $P4Widgets(HIGH) insert 0 [nbs get ${root}high]
    $P4Widgets(LOW) delete 0 end
    $P4Widgets(LOW) insert 0 [nbs get ${root}low]
    $P4Widgets(HISTOGRAM_XSTEP) delete 0 end
    $P4Widgets(HISTOGRAM_XSTEP) insert 0 [nbs get ${root}histogram_xstep]
    $P4Widgets(HISTOGRAM_YSTEP) delete 0 end
    $P4Widgets(HISTOGRAM_YSTEP) insert 0 [nbs get ${root}histogram_ystep]
    $P4Widgets(HISTOGRAM_BINS) delete 0 end
    $P4Widgets(HISTOGRAM_BINS) insert 0 [nbs get ${root}histogram_bins]
    $P4Widgets(HIST_SMOOTH) delete 0 end
    $P4Widgets(HIST_SMOOTH) insert 0 [nbs get ${root}hist_smooth]
    set P4Widgets(CONTOUR_TYPE) [string trim [string toupper [nbs get ${root}contour_type]]]
    $P4Widgets(CONTOUR_LEVELS) delete 0 end
    $P4Widgets(CONTOUR_LEVELS) insert 0 [nbs get ${root}contour_levels]
    set P4Widgets(PLOT_ERRORS) [string trim [nbs get ${root}plot_errors]]
    set P4Widgets(CUT_DIRECTION) [string trim [nbs get ${root}cut_direction]]
    $P4Widgets(SLICE_START) delete 0 end
    $P4Widgets(SLICE_START) insert 0 [nbs get ${root}slice_start]
    $P4Widgets(SLICE_END) delete 0 end
    $P4Widgets(SLICE_END) insert 0 [nbs get ${root}slice_end]
    set P4Widgets(OVERCOLOUR) [string trim [nbs get ${root}overcolour]]
    set P4Widgets(BGCOLOUR) [string trim [nbs get ${root}bg_colour]]
    set P4Widgets(FGCOLOUR) [string trim [nbs get ${root}fg_colour]]
    set P4Widgets(LUT) [string trim [nbs get ${root}device_lut]]
}
