proc p4Update {item value} {
  global env
  global P4Widgets
  if {[string trim ${item}] == "p4Set"} {
    $P4Widgets(DEVICE) delete 0 end
    $P4Widgets(DEVICE) insert 0 xwindows\;$env(PID)xwin
    $P4Widgets(TITLE) delete 0 end
    $P4Widgets(TITLE) insert 0 A_U_T_O
    $P4Widgets(XOPT) delete 0 end
    $P4Widgets(XOPT) insert 0 BCNTSI
    $P4Widgets(YOPT) delete 0 end
    $P4Widgets(YOPT) insert 0 BCNTSI
    $P4Widgets(CHAR_HEIGHT) delete 0 end
    $P4Widgets(CHAR_HEIGHT) insert 0 1.0
    $P4Widgets(ISTART) delete 0 end
    $P4Widgets(ISTART) insert 0 -1
    $P4Widgets(IEND) delete 0 end
    $P4Widgets(IEND) insert 0 -1
    $P4Widgets(JSTART) delete 0 end
    $P4Widgets(JSTART) insert 0 -1
    $P4Widgets(JEND) delete 0 end
    $P4Widgets(JEND) insert 0 -1
    set P4Widgets(CSTYLE) COLOUR
    set P4Widgets(PLOT_AXES) 1
    set P4Widgets(PRE_ERASE_PLOT) 1

  } elseif {[string trim ${item}] == "buildP4Widgets1"} {
    if {[string toupper [string trim ${value}]] == "ALL"} {
      set P4Widgets(PLOT_WHOLE) 1
      set P4Widgets(AUTOSCALE) 1
      set P4Widgets(PORT_NO) 0
      set P4Widgets(DISPLAY_PLANE) DATA
      set P4Widgets(DISPLAY_TYPE) IMAGE
      $P4Widgets(DATA) delete 0 end
      $P4Widgets(DATA) insert 0 \$P4_CT/cgs4
    } elseif {[string toupper [string trim ${value}]] == "WHOLE"} {
      $P4Widgets(XSTART) delete 0 end
      $P4Widgets(XSTART) insert 0 1
      $P4Widgets(XEND) delete 0 end
      $P4Widgets(XEND) insert 0 256
      $P4Widgets(YSTART) delete 0 end
      $P4Widgets(YSTART) insert 0 1
      $P4Widgets(YEND) delete 0 end
      $P4Widgets(YEND) insert 0 256
    } elseif {[string toupper [string trim ${value}]] == "SCALE"} {
      $P4Widgets(HIGH) delete 0 end
      $P4Widgets(HIGH) insert 0 1000.0
      $P4Widgets(LOW) delete 0 end
      $P4Widgets(LOW) insert 0 0.0
    } elseif {[string toupper [string trim ${value}]] == "HISTOGRAM"} {
      $P4Widgets(HISTOGRAM_XSTEP) delete 0 end
      $P4Widgets(HISTOGRAM_XSTEP) insert 0 1
      $P4Widgets(HISTOGRAM_YSTEP) delete 0 end
      $P4Widgets(HISTOGRAM_YSTEP) insert 0 1
      $P4Widgets(HISTOGRAM_BINS) delete 0 end
      $P4Widgets(HISTOGRAM_BINS) insert 0 50
      $P4Widgets(HIST_SMOOTH) delete 0 end
      $P4Widgets(HIST_SMOOTH) insert 0 1
    } elseif {[string toupper [string trim ${value}]] == "CONTOUR"} {
      $P4Widgets(CONTOUR_LEVELS) delete 0 end
      $P4Widgets(CONTOUR_LEVELS) insert 0 10
      set P4Widgets(CONTOUR_TYPE) LIN
    } elseif {[string toupper [string trim ${value}]] == "GRAPH"} {
      $P4Widgets(SLICE_START) delete 0 end
      $P4Widgets(SLICE_START) insert 0 29.0
      $P4Widgets(SLICE_END) delete 0 end
      $P4Widgets(SLICE_END) insert 0 29.0
      set P4Widgets(PLOT_ERRORS) 1
      set P4Widgets(CUT_DIRECTION) X
      set P4Widgets(OVERCOLOUR) RED
    }

  } else {
    set P4Widgets(${item}) [string trim ${value}]
  }
  update idletasks
}
