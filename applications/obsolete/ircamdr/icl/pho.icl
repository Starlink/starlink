{ PROCEDURE PHO : Photometry on image as in DISP
proc pho peak
  get plt2d image_workstn (image_workstn)
  if image_workstn <> 1
    print "PHO is not available from this terminal"
    return
  end if
  get plt2d filetype (filetype)
  yn = undefined(peak)
  if yn
    peak2 = "N"
  else
    peak2 = "Y"
    print "Optimization of aperture position on PEAK PIXEL selected"
  end if
  get plt2d name_image (last_image)
  print "Last image plotted = " (last_image)
  print "Give name of image for PHOTOMETRY (RETURN = last image plotted) : "
  askname (name_image) "Image name \-1\ ? "
  if name_image = "-1"
    print "OK using last image plotted" (last_image)
    name_image = last_image
  end if
  send plt2d set name_image (name_image)
  send plt2d set cursor_image (name_image)
  print "Exposure time of image (seconds) : "
  asknum (exp) "Exposure time \1.0\ ? "
  if filetype <> 1
    print "Number of coadds in image : "
    asknum (ncoadds) "Number of Coadds \1\ ? "
  else
    ncoadds = 1
  end if
  print "Filter used for image (J,H,K,nbL,Lp,nbM) : "
  askname (filtn) "Filter Used \K\ ? "
  filtn = upcase(filtn)
  get plt2d disp_mag (disp_mag)
  send plt2d set magnification (disp_mag)
  get plt2d im_xcen (workxcen)
  get plt2d im_ycen (workycen)
  if workxcen = 0
    get plt2d max_xsize (workxcen)
    workxcen = workxcen/2.0
  end if
  if workycen = 0
    get plt2d max_ysize (workycen)
    workycen = workycen/2.0
  end if
  obeyw plt2d clear
  print "Plotting image " (name_image)
  obeyw plt2d nsigma (name_image)
  send plt2d set cursor_cross 'NO'
  get plt2d im_yen (value1)
  value1 = value1+15
  obeyw plt2d comment (name_image) (workxcen) (value1) 20
  diffim = name_image
  get plt2d platscal (scale)
  if filetype <> 1
    print "  Image COADDS               = " (ncoadds)
  end if
  print "  Image EXPOSURE TIME (S)    = " (exp)
  print "  FILTER used                = " (filtn)
  print "  Plate scale (arcsec/pixel) = " (scale)
  get plt2d zeroj   (zeroj)
  get plt2d zeroh   (zeroh)
  get plt2d zerok   (zerok)
  get plt2d zeronbl (zeronbl)
  get plt2d zerolp  (zerolp)
  get plt2d zeronbm (zeronbm)
  total=ncoadds*exp
  zp = 0
  compare (filtn) "J" (brave_man)
  if brave_man = 1
    zp = zeroj
  end if
  compare (filtn) "H" (brave_man)
  if brave_man = 1
    zp = zeroh
  end if
  compare (filtn) "K" (brave_man)
  if brave_man = 1
    zp = zerok
  end if
  compare (filtn) "NBL" (brave_man)
  if brave_man = 1
    zp = zeronbl
  end if
  compare (filtn) "LP" (brave_man)
  if brave_man = 1
    zp = zerolp
  end if
  compare (filtn) "NBM" (brave_man)
  if brave_man = 1
    zp = zeronbm
  end if
  print "  Zeropoint to be used       = " (zp) " for " (filtn)
  get plt2d disp_ap1 (daper1)
  loop for dummy = 1 to 1000
    print "Aperture size (in arcsec) DEFAULT = " (daper1)
    asknum (aper) "Aperture Size \-1\ ? "
    if aper = -1
      aper = daper1
    end if
    print "Pick the OBJECT with the cursor"
    obeyw plt2d cursor
    get plt2d x_cur_pixel (xobj)
    get plt2d y_cur_pixel (yobj)
    xobj = integer(xobj)
    yobj = integer(yobj)
    if peak2 = "Y"
      xobj0 = integer(xobj)
      yobj0 = integer(yobj)
      xst = integer(xobj0-5.0)
      yst = integer(yobj0-5.0)
      obeyw rapi2d HISTO (name_image) (xst) (yst) 11 11 \
      getpar glob histo_max (maxval)
      getpar glob histo_min (minval)
      getpar glob histo_xmax (xobj)
      getpar glob histo_ymax (yobj)
      print "Pixel selected with cursor = " (xobj0) (yobj0)
      print "Maximum signal of " (maxval) " found at " (xobj) (yobj)
    end if
    obeyw plt2d circle (xobj) (yobj) (aper)
    print "Pick SKY with the cursor"
    obeyw plt2d cursor
    get plt2d x_cur_pixel (xsky)
    get plt2d y_cur_pixel (ysky)
    obeyw plt2d circle (xsky) (ysky) (aper)
    print "  SKY aperture center (X,Y)    = " (xsky) "," (ysky)
    print "Analysing image " (diffim)
    obeyw rapi2d APERADD (diffim) (xobj) (yobj) (aper) (scale) "N" "Y"
    getpar glob aperadd_total (objval)
    getpar glob aperadd_numpix (nob)
    obeyw rapi2d APERADD (diffim) (xsky) (ysky) (aper) (scale) "N" "Y"
    getpar glob aperadd_total (skyval)
    getpar glob aperadd_numpix (nos)
    objval = (objval-(skyval*nob/nos))/total
    mag = zp - 2.5*(log(abs(objval))/log(10))
    if objval < 0
      print "WARNING : OBJECT-SKY gives negative value, absolute value used"
    end if
    print "  Zeropoint                                = " (zp)
    print "  Star-Sky                                 = " (objval)
    print "  Magnitude                                = " (mag)
    print " "
    print "More PHOTOMETRY on image " (name_image) " ? "
    asklog (brave_man) "More Photometry \N\ ? "
    if brave_man = 0
      return
    end if
  end loop
end proc
