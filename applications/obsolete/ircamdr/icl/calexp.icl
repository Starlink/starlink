{ PROCEDURE CALEXP to calculate optimum exposure on unsaturated object image
proc calexp framenum value6
  set precision 6
  get plt2d name_prefix (name_prefix)
  get plt2d filetype (filetype)
  compare name_prefix 'UNKNOWN' (brave_man)
  if brave_man = 1
    print "You must define data file prefix before running CALEXP"
    print "Use command SETFILE."
    return
  else
    print "IRCAM file prefix = " (name_prefix)
  end if
  yn1 = undefined(framenum)
  yn2 = undefined(value6)
  if yn1
    asknum (f2) "Observations number \1\ : "
  else
    f2 = integer(framenum)
  end if
  if yn2
    asknum (nsl) "NSIGMA level for display \20\ : "
  else
    nsl = value6
  end if
  dum = 1
  get plt2d name_image (last_image)
  get_imagename (f2) (dum) (name_out) (last_image)
  print "Work image = " (name_out)
  send plt2d set cursor_image (name_out)
  get plt2d workxcen (workxcen)
  get plt2d workycen (workycen)
  send plt2d set im_xcen (workxcen)
  send plt2d set im_ycen (workycen)
  get plt2d disp_mag (disp_mag)
  send plt2d set magnification (disp_mag)
  obeyw plt2d nsigma (name_out) (nsl)
  if filetype = 1
    obeyw plt2d ropars (name_out)
    get plt2d exposure_time (exp)
    expsec = exp
    get plt2d mode (mode)
    print "Observation mode = " (mode)
  else
    obeyw plt2d contpars (f2)
    get plt2d exposure_time (exp)
    get plt2d number_coadds (nc)
    expsec = exp*nc
    get plt2d mode (mode)
  end if
  mode2 = upcase(mode)
  if mode2 = "UNKNOWN"
    print "Give mode of observation (STARE,ND_STARE,CHOP) : "
    askname (mode) "Mode \ND_STARE\ ? "
  end if
  get plt2d filter (filtn)
  print "Exposure time of image = " (expsec) " seconds in filter " (filtn)
  print "Center cursor on object"
  obeyw plt2d cursor
  get plt2d x_cur_pixel (xobj)
  get plt2d y_cur_pixel (yobj)
  xobj = integer(xobj)
  yobj = integer(yobj)
  print "Pixel selected with cursor = " (xobj) (yobj)
  xst = max( 1, integer(xobj-5.0))
  yst = max( 1, integer(yobj-5.0))
  get plt2d platscal (opscal)
  send plt2d set platscal 1.0
  send plt2d set arcsec_pixel 1.0
  obeyw plt2d BOX (xst) (yst) 11 11 'BOTTOM_LEFT'
  send plt2d set platscal (opscal)
  send plt2d set arcsec_pixel (opscal)
  obeyw rapi2d HISTO (name_out) (xst) (yst) 11 11 \
  getpar glob histo_max (maxval)
  getpar glob histo_min (minval)
  getpar glob histo_xmax (xobj)
  getpar glob histo_ymax (yobj)
  print "Maximum signal of " (maxval) " found at " (xobj) (yobj)
  if filetype = 1 and mode = "STARE"
    counts = maxval-24000
  else
    counts = maxval
  end if
  perexp = counts
  persec = perexp/expsec
  print " "
  if mode = "STARE"
    print "Source has peak of " (perexp) " DN over bias level"
  else
  print "Source has peak of " (perexp) " DN"
  end if
  print "Which equals " (persec) " DN/second over " (expsec) " seconds"
  if filetype = 1
    optimum = 14500.0/persec
  else
    optimum = 26000.0/persec
  end if
  print "80% saturation level = " (optimum) " sec"
  print " "
end proc

