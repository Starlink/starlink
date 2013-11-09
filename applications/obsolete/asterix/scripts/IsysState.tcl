#
proc startup_state {} {

  global mbar tbar stbar cbar attrib

  $mbar.edit configure -state disabled
  $mbar.profile configure -state disabled
  $mbar.source configure -state disabled
  $mbar.mark configure -state disabled
  $mbar.reg configure -state disabled
  $mbar.zoom configure -state disabled

  $tbar.disp configure -state disabled
  $tbar.smooth configure -state disabled
  $tbar.scale configure -state disabled
  $tbar.contour configure -state disabled
  $tbar.browse configure -state disabled
  $tbar.setpos configure -state disabled
  $tbar.centroid configure -state disabled
  $tbar.plot configure -state disabled

  $attrib.def configure -state disabled
  $attrib.win configure -state disabled
  $attrib.axes configure -state disabled
  $attrib.titles configure -state disabled
  $attrib.labels configure -state disabled
  $attrib.annotate configure -state disabled
  $attrib.grid configure -state disabled
  $attrib.key configure -state disabled
  $attrib.col configure -state disabled

#  $stbar.box configure -state disabled
#  $stbar.slice configure -state disabled
#  $stbar.poly configure -state disabled
#  $stbar.circle configure -state disabled
#  $stbar.annulus configure -state disabled
#  $stbar.ellipse configure -state disabled
#  $stbar.contour configure -state disabled
#  $stbar.add configure -state disabled
#  $stbar.and configure -state disabled
#  $stbar.not configure -state disabled
#  $stbar.cut configure -state disabled
#  $stbar.show configure -state disabled
#  $stbar.whole configure -state disabled

  DisableGroup region

  DisableGroup cache

}

proc active_state {} {

  global mbar tbar stbar cbar attrib

  $mbar.edit configure -state normal
  $mbar.profile configure -state normal
  $mbar.source configure -state normal
  $mbar.mark configure -state normal
  $mbar.reg configure -state normal
  $mbar.zoom configure -state normal

  $tbar.disp configure -state normal
  $tbar.smooth configure -state normal
  $tbar.scale configure -state normal
  $tbar.contour configure -state normal
  $tbar.browse configure -state normal
  $tbar.setpos configure -state normal
  $tbar.centroid configure -state normal
  $tbar.plot configure -state normal

  $attrib.def configure -state normal
  $attrib.win configure -state normal
  $attrib.axes configure -state normal
  $attrib.titles configure -state normal
  $attrib.labels configure -state normal
  $attrib.annotate configure -state normal
  $attrib.grid configure -state normal
  $attrib.key configure -state normal
  $attrib.col configure -state normal


  EnableGroup region

  EnableGroup cache

}
