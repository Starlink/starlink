{ PROCEDURE STREHL : calculated strehl coefficients from S+A image
 proc strehl imno
  get plt2d name_prefix (ronam)
  get plt2d rosuf (suf)
  if suf = "NONE"
    suf = ""
  end if
  print "Current data file prefix = " (ronam)
  print "Current data file suffix = " (suf)
  yn = undefined(imno)
  if yn
    print "Enter S+A object number (0 for full image name) ? "
    asknum (imst) "Object \0\ ? "
  else
    imst = imno
  end if
  if imst = 0
    get plt2d name_image (last_image)
    print "Last image plotted = " (last_image)
    print "Enter full image name : "
    askname (im) "Image Name \-1\ ? "
    if im = "-1"
      im = last_image
    end if
    print "Enter filter used, on-chip exposure time and pixel scale : "
    askchoice (ifilt) "Filter (J,H,K,NBL,LP,NBM) \K\ ? "
    if ifilt = 1
      filter = "J"
    else if ifilt = 2
      filter = "H"
    else if ifilt = 3
      filter = "K"
    else if ifilt = 4
      filter = "NBL"
    else if ifilt = 5
      filter = "LP"
    else if ifilt = 6
      filter = "NBM"
    end if
    asknum (expo) "Exposure Time (in sec) \1\ ? "
    asknum (scale) "Pixel Scale (arcsec per pixel) \0.057\ ? "
  else
    im = ronam & imst & suf
    obeyw plt2d ropars (im)
    get plt2d object_name   (object)
    get plt2d filter        (filt)
    get plt2d exposure_time (expo)
    get plt2d number_exp    (nexp)
    get plt2d pix_size (scale)
    scale = real(scale)
    filter = substr(filt,1,1)
    filter = upcase(filter)
    if filter = "N"
      filter = substr(filt,1,3)
    end if
    print "Object name   = " (object)
    print "Filter        = " (filter)
    print "Exposure time = " (expo)
    print "Number Expo.  = " (nexp)
    print "Pixel Scale   = " (scale)
  end if
  send plt2d set arcsec_pixel (scale)
  send plt2d set platscal (scale)
  get plt2d workxcen (workxcen)
  get plt2d workycen (workycen)
  send plt2d set im_xcen (workxcen)
  send plt2d set im_ycen (workycen)
  get plt2d magnification (oldmag)
  obeyw rapi2d shsize (im)
  getpar glob shsize_xdim (xsz)
  if xsz = 256
    newmag = 0
  else if xsz = 128
    newmag = 0
  else if xsz = 64
    newmag = 0
  else
    newmag = 0
  end if
  obeyw plt2d clear
  send plt2d set magnification (newmag)
  send plt2d set cursor_image (im)
  send plt2d set name_image (im)
  send plt2d set cursor_image (im)
  obeyw plt2d nsigma (im)
  send plt2d set cursor_image (im)
  print "Select approx STAR peak pixel with cursor ..."
  obeyw plt2d cursor
  get plt2d x_cur_pixel (xpix)
  get plt2d y_cur_pixel (ypix)
  xpix = integer(xpix)
  ypix = integer(ypix)
  xst = xpix-5
  yst = ypix-5
  xst = integer(xst)
  yst = integer(yst)
  send plt2d set magnification (oldmag)
  obeyw rapi2d HISTO (im) (xst) (yst) 11 11 \
  getpar glob histo_max (maxval)
  getpar glob histo_min (minval)
  getpar glob histo_xmax (xobj)
  getpar glob histo_ymax (yobj)
  print "Peak signal = " (maxval) " at pixel " (xobj) "," (yobj)
  get plt2d disp_ap1 (ap1)
  get plt2d disp_ap2 (ap2)
  get plt2d disp_ap3 (ap3)
  print "Star aperture diameter           = " (ap1) "arcsec"
  print "Sky annulus inner/outer diameter = " (ap2) (ap3) "arcsec"
  obeyw plt2d circle (xobj) (yobj) (ap1)
  obeyw plt2d circle (xobj) (yobj) (ap2)
  obeyw plt2d circle (xobj) (yobj) (ap3)
  obeyw rapi2d APERADD (im) (xobj) (yobj) (ap1) (scale) "N"
  getpar glob aperadd_total (objval)
  getpar glob aperadd_numpix (nob)
  obeyw rapi2d APERADD (im) (xobj) (yobj) (ap2) (scale) "N"
  getpar glob aperadd_total (skyval)
  getpar glob aperadd_numpix (nos)
  obeyw rapi2d APERADD (im) (xobj) (yobj) (ap3) (scale) "N"
  getpar glob aperadd_total (skyval2)
  getpar glob aperadd_numpix (nos2)
  skypp = ((skyval2-skyval)/(nos2-nos))
  objpp = objval
  objval = (objval-((skyval2-skyval)*(nob/(nos2-nos))))
  if filter = "J"
    if scale = 0.057
      perf = 0.34967
    else if scale = 0.06
      perf = 0.34967
    else if scale = 0.143
      perf = 0.65554
    else if scale = 0.150
      perf = 0.65554
    else if scale = 0.286
      perf = 0.89316
    else
      perf = 0.0
    end if
  else if filter = "H"
    if scale = 0.057
      perf = 0.23975
    else if scale = 0.06
      perf = 0.23975
    else if scale = 0.143
      perf = 0.63392
    else if scale = 0.150
      perf = 0.63392
    else if scale = 0.286
      perf = 0.80997
    else
      perf = 0.0
    end if
  else if filter = "K"
    if scale = 0.057
      perf = 0.13838
    else if scale = 0.06
      perf = 0.13838
    else if scale = 0.143
      perf = 0.52751
    else if scale = 0.150
      perf = 0.52751
    else if scale = 0.286
      perf = 0.67199
    else
      perf = 0.0
    end if
  else if filter = "Lp"
    if scale = 0.057
      perf = 0.04954
    else if scale = 0.06
      perf = 0.04954
    else if scale = 0.143
      perf = 0.26043
    else if scale = 0.150
      perf = 0.26043
    else if scale = 0.286
      perf = 0.58867
    else
      perf = 0.0
    end if
  else if filter = "NBL"
    if scale = 0.057
      perf = 0.06137
    else if scale = 0.06
      perf = 0.06137
    else if scale = 0.143
      perf = 0.30931
    else if scale = 0.150
      perf = 0.30931
    else if scale = 0.286
      perf = 0.62163
    else
      perf = 0.0
    end if
  else if filter = "NBM"
    if scale = 0.057
      perf = 0.03567
    else if scale = 0.06
      perf = 0.03567
    else if scale = 0.143
      perf = 0.19692
    else if scale = 0.150
      perf = 0.19692
    else if scale = 0.286
      perf = 0.51683
    else
      perf = 0.0
      return
    end if
  else
    print "Non-standard filter used (i.e. not JHKnbLLp,nbM)"
    return
  end if
  if perf = 0.0
    print "Unusual pixel scale, no idea what perfect image ratio"
    print "value to use..." (scale)
    return
  end if
  rat = maxval/objval
  rat2 = rat/perf
  if imst <> 0
    print "Object name         = " (object)
  else
    print "Image name          = " (im)
  end if
  print "Filter              = " (filter)
  print "Exposure time       = " (expo)
  if imst <> 0
    print "Number Expo.        = " (nexp)
  end if
  print "Pixel Scale         = " (scale)
  print " "
  print "Peak signal         = " (maxval)
  print "Object total signal = " (objval)
  print " "
  print "Ratio peak/total    = " (rat)
  print "Perfect image ratio = " (perf)
  print " "
  print "Strehl ratio (Observed/Perfect) = " (rat2)
  print " "
end proc
