{ PROCEDURE CSTATS : procedure to run STATS rapi2d action
proc cstats bsiz
  yn = undefined(bsiz)
  set precision 6
  get plt2d name_image (name_image)
  print "Image being used for CSTATS = " (name_image)
  if yn
    asknum (xsiz) "Enter the stats box X-size in pixels \20\ : "
    asknum (ysiz) "Enter the stats box Y-size in pixels \20\ : "
  else
    xsiz = bsiz
    ysiz = bsiz
  end if
  send plt2d set cursor_image (name_image)
  print "Select the CENTRE of the box for STATISTICS CALCULATION ..."
  print "with the cursor ..."
  obeyw plt2d cursor
  get plt2d x_cur_pixel (xpix1)
  get plt2d y_cur_pixel (ypix1)
  xstb = int(xpix1-(xsiz/2.0))
  ystb = int(ypix1-(ysiz/2.0))
  if xstb < 1
    xstb = 1
  end if
  if ystb < 1
    ystb = 1
  end if
  send plt2d set cursor_cross 'NO'
  get plt2d platscal (opscal)
  send plt2d set platscal 1.0
  send plt2d set arcsec_pixel 1.0
  obeyw plt2d BOX (xstb) (ystb) (xsiz) (ysiz) 'BOTTOM_LEFT'
  send plt2d set platscal (opscal)
  send plt2d set arcsec_pixel (opscal)
  obeyw rapi2d HISTO (name_image) (xstb) (ystb) (xsiz) (ysiz) \
  obeyw rapi2d STATS (name_image) (xstb) (ystb) (xsiz) (ysiz) \
  getpar glob histo_max (maxval)
  getpar glob histo_min (minval)
  getpar glob histo_xmax (xmax)
  getpar glob histo_ymax (ymax)
  getpar glob histo_xmin (xmin)
  getpar glob histo_ymin (ymin)
  getpar glob stats_mean (meanval)
  getpar glob stats_median (medval)
  getpar glob stats_std (onesig)
  xmax = integer(xmax)
  ymax = integer(ymax)
  xmin = integer(xmin)
  ymin = integer(ymin)
  print " "
  print "Results from CSTATS"
  print "Pixel selected with cursor [centre of box] = " (xpix1) (ypix1)
  print "Maximum value = " (maxval) " at pixel " (xmax) (ymax)
  print "Minimum value = " (minval) " at pixel " (xmin) (ymin)
  print "Mean signal   = " (meanval)
  print "Median signal = " (medval)
  print "1-sigma std   = " (onesig)
  print " "
end proc
