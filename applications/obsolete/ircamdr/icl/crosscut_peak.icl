{ PROCEDURE CROSSCUT_PEAK : plots an x and a y cut on current image
proc crosscut_peak sig off
  get plt2d name_image (name_image)
  print "Last image plotted was " (name_image)
  print "Enter image to be used in CROSSCUT (Hit RETURN to use above image)"
  askname (st1) "Image name \ABOVE_IMAGE\ ? "
  compare (st1) "ABOVE_IMAGE" (brave_man)
  if brave_man = 0
    name_image = st1
  else
    print "O.K. using image " (name_image)
  end if
  yn2 = undefined(off)
  if yn2
    asknum (poffset) "Pixel offset from peak to plot over \25\ ? "
  else
    poffset = off
  end if
  pinc = integer(poffset/3.0+0.5)
  cccom = name_image
  print "Plotting image " (name_image)
  yn = undefined(sig)
  if yn
    obeyw plt2d nsigma (name_image)
  else
    obeyw plt2d nsigma (name_image) (sig)
  end if
  send plt2d set cursor_image (name_image)
  im = name_image
  print "Select star with cursor"
  CURSOR
  get plt2d x_cur_pixel (xcentre)
  get plt2d y_cur_pixel (ycentre)
  obeyw plt2d line_width 1
  xcentre = integer(xcentre)
  ycentre = integer(ycentre)
  xst = integer(xcentre-5.0)
  yst = integer(ycentre-5.0)
  obeyw rapi2d HISTO (im) (xst) (yst) 11 11 \
  getpar glob histo_max (maxval)
  getpar glob histo_min (minval)
  getpar glob histo_xmax (xcentre)
  getpar glob histo_ymax (ycentre)
  print "Peak signal = " (maxval) " at pixel " (xcentre) "," (ycentre)
  start = integer(xcentre-poffset)
  finish = integer(xcentre+poffset)
  send plt2d set cut_axisratio 1.0
  send plt2d set cut_linetype 'B'
  send plt2d set cut_scaling 'A'
  send plt2d set cut_annotation 'FULL'
  send plt2d set cut_xticst (start)
  send plt2d set cut_xticint (pinc)
  send plt2d set cut_magnif 6
  send plt2d set cut_positioning 'KEY'
  get plt2d workxcen (workxcen)
  get plt2d workycen (workycen)
  cxcen = integer(workxcen/2.0)
  cycen = integer(workycen)
  send plt2d set cut_xcen (cxcen)
  send plt2d set cut_ycen (cycen)
  send plt2d set cut_title 'RA crosscut'
  print "Plotting RA cut: image = " (name_image)
  print "  X-start,X-end = " (start) "," (finish)
  print "  Y-start,Y-end = " (ycentre) "," (ycentre)
  obeyw plt2d cut (name_image) (start) (ycentre) (finish) (ycentre)
  cxcen = integer(3.0*workxcen/2.0)
  cycen = integer(workycen)
  start = integer(ycentre-poffset)
  finish = integer(ycentre+poffset)
  print "Plotting DEC cut: image = " (name_image)
  print "  X-start,X-end = " (xcentre) "," (xcentre)
  print "  Y-start,Y-end = " (start) "," (finish)
  send plt2d set cut_xcen (cxcen)
  send plt2d set cut_ycen (cycen)
  send plt2d set cut_xticst (start)
  send plt2d set cut_title 'DEC crosscut'
  obeyw plt2d cut (name_image) (xcentre) (start) (xcentre) (finish)
  get plt2d im_yen (value1)
  value1 = integer(value1+30)
  send plt2d set cursor_cross 'NO'
  obeyw plt2d comment (cccom) (workxcen) (value1) 15
  print "Hardcopy these crosscuts ? "
  asklog (yn) "Hardcopy (Yes or No) \N\ ? "
  if yn = 1
    get plt2d worknam (worknam)
    get plt2d worknum (worknum)
    oldname = worknam
    oldnumb = worknum
    get plt2d max_xsize (oldmaxx)
    get plt2d max_ysize (oldmaxy)
    get plt2d cut_magnif (oldcmag)
    get plt2d hard_device (hard_device)
    obeyw plt2d close
    if hard_device = 14
      newcmag = 4
    else if hard_device = 15
      newcmag = 8
    else
      print "Hardcopy device set to Postscript landscape mode"
      hard_device = 15
      send plt2d set hard_device (hard_device)
      newcmag = 8
    end if
    POPEN (hard_device)
    obeyw plt2d line_width 3
    get plt2d max_xsize (newmaxx)
    get plt2d max_ysize (newmaxy)
    start = integer(xcentre-poffset)
    finish = integer(xcentre+poffset)
    send plt2d set cut_positioning 'KEY'
    send plt2d set cut_axisratio 1.0
    send plt2d set cut_linetype 'B'
    send plt2d set cut_scaling 'A'
    send plt2d set cut_annotation 'FULL'
    send plt2d set cut_xticst (start)
    send plt2d set cut_xticint (pinc)
    send plt2d set cut_magnif (newcmag)
    send plt2d set cut_positioning 'KEY'
    get plt2d workxcen (workxcen)
    get plt2d workycen (workycen)
    cxcen = integer(workxcen/2.0)
    cycen = integer(workycen)
    send plt2d set cut_xcen (cxcen)
    send plt2d set cut_ycen (cycen)
    send plt2d set cut_title 'RA crosscut'
    print "Plotting RA cut: image = " (name_image)
    print "  X-start,X-end = " (start) "," (finish)
    print "  Y-start,Y-end = " (ycentre) "," (ycentre)
    obeyw plt2d cut (name_image) (start) (ycentre) (finish) (ycentre)
    cxcen = integer(3.0*workxcen/2.0)
    cycen = integer(workycen)
    start = integer(ycentre-poffset)
    finish = integer(ycentre+poffset)
    print "Plotting DEC cut: image = " (name_image)
    start = integer(ycentre-poffset)
    finish = integer(ycentre+poffset)
    print "  X-start,X-end = " (xcentre) "," (xcentre)
    print "  Y-start,Y-end = " (start) "," (finish)
    send plt2d set cut_xcen (cxcen)
    send plt2d set cut_ycen (cycen)
    send plt2d set cut_xticst (start)
    send plt2d set cut_title 'DEC crosscut'
    obeyw plt2d cut (name_image) (xcentre) (start) (xcentre) (finish)
    get plt2d im_yen (value1)
    value1 = integer(value1+100)
    send plt2d set cursor_cross 'NO'
    obeyw plt2d comment (cccom) (workxcen) (value1) 20
    send plt2d set cut_magnif (oldcmag)
    print "Closing plotting on hardcopy device ..."
    obeyw plt2d close
    HARDPRINT (hard_device) Y
    print "Re-opening workstation " (oldname) " ..."
    worknum = oldnumb
    worknam = oldname
    POPEN (worknum)
  end if
end proc
