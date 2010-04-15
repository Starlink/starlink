proc p4SetNbs {nbsroot} {
#+
# Sets all relevent NBS items
#-
    global P4Widgets
    global P4NoticeBoard

#  Put character nbs items
    nbs put ${nbsroot}.display_data  [string trim [$P4Widgets(DATA) get]]
    nbs put ${nbsroot}.title         [string trim [$P4Widgets(TITLE) get]]
    nbs put ${nbsroot}.device_name   [string trim [$P4Widgets(DEVICE) get]]
    nbs put ${nbsroot}.device_xopt   [string trim [$P4Widgets(XOPT) get]]
    nbs put ${nbsroot}.device_yopt   [string trim [$P4Widgets(YOPT) get]]
    nbs put ${nbsroot}.device_lut    [string trim $P4Widgets(LUT)]
    nbs put ${nbsroot}.display_type  [string trim $P4Widgets(DISPLAY_TYPE)]
    nbs put ${nbsroot}.display_plane [string trim $P4Widgets(DISPLAY_PLANE)]
    nbs put ${nbsroot}.contour_type  [string trim $P4Widgets(CONTOUR_TYPE)]
    nbs put ${nbsroot}.overcolour    [string trim $P4Widgets(OVERCOLOUR)]
    nbs put ${nbsroot}.colour_style  [string trim $P4Widgets(CSTYLE)]
    nbs put ${nbsroot}.fg_colour     [string trim $P4Widgets(FGCOLOUR)]
    nbs put ${nbsroot}.bg_colour     [string trim $P4Widgets(BGCOLOUR)]
    nbs put ${nbsroot}.cut_direction [string trim $P4Widgets(CUT_DIRECTION)]
    nbs put ${nbsroot}.last_type     [string trim $P4Widgets(DISPLAY_TYPE)]

#  Put logical nbs items
    nbs put ${nbsroot}.plot_axes      $P4Widgets(PLOT_AXES)
    nbs put ${nbsroot}.plot_errors    $P4Widgets(PLOT_ERRORS)
    nbs put ${nbsroot}.plot_whole     $P4Widgets(PLOT_WHOLE)
    nbs put ${nbsroot}.pre_erase_plot $P4Widgets(PRE_ERASE_PLOT)
    nbs put ${nbsroot}.autoscale      $P4Widgets(AUTOSCALE)
    nbs put ${nbsroot}.port_ok        1
    nbs put ${nbsroot}.plot_ok        1

#  Put integer nbs items
    nbs put ${nbsroot}.contour_levels  [string trim [$P4Widgets(CONTOUR_LEVELS) get]]
    nbs put ${nbsroot}.histogram_bins  [string trim [$P4Widgets(HISTOGRAM_BINS) get]]
    nbs put ${nbsroot}.histogram_xstep [string trim [$P4Widgets(HISTOGRAM_XSTEP) get]]
    nbs put ${nbsroot}.histogram_ystep [string trim [$P4Widgets(HISTOGRAM_YSTEP) get]]
    nbs put ${nbsroot}.hist_smooth     [string trim [$P4Widgets(HIST_SMOOTH) get]]
    nbs put ${nbsroot}.istart          [string trim [$P4Widgets(ISTART) get]]
    nbs put ${nbsroot}.iend            [string trim [$P4Widgets(IEND) get]]
    nbs put ${nbsroot}.jstart          [string trim [$P4Widgets(JSTART) get]]
    nbs put ${nbsroot}.jend            [string trim [$P4Widgets(JEND) get]]

#  Put real nbs items
    nbs put ${nbsroot}.xstart      [string trim [$P4Widgets(XSTART) get]]
    nbs put ${nbsroot}.xend        [string trim [$P4Widgets(XEND) get]]
    nbs put ${nbsroot}.ystart      [string trim [$P4Widgets(YSTART) get]]
    nbs put ${nbsroot}.yend        [string trim [$P4Widgets(YEND) get]]
    nbs put ${nbsroot}.high        [string trim [$P4Widgets(HIGH) get]]
    nbs put ${nbsroot}.low         [string trim [$P4Widgets(LOW) get]]
    nbs put ${nbsroot}.slice_start [string trim [$P4Widgets(SLICE_START) get]]
    nbs put ${nbsroot}.slice_end   [string trim [$P4Widgets(SLICE_END) get]]
    nbs put ${nbsroot}.char_height [string trim [$P4Widgets(CHAR_HEIGHT) get]]

# Update the display
    update idletasks
}
