{ PROCEDURE PHO3 : Photometry on image with concentric apertures
proc pho3 peak
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
  print "Give name of image for PHOTOMETRY (RETURN = last image plotted) :"
  askname (name_image) "Image name \-1\ ? "
  if name_image = "-1"
    print "OK using last image plotted " (last_image)
    name_image = last_image
  end if
  send plt2d set name_image (name_image)
  send plt2d set cursor_image (name_image)
  print "Exposure time of image (seconds)  :"
  asknum (exp) "Exposure time \1.0\ ? "
  if filetype <> 1
    print "Number of coadds in image : "
    asknum (ncoadds) "Number of Coadds \1\ ? "
  else
    ncoadds = 1
  end if
  print "Filter used (J,H,K,nbL,Lp,nbM) : "
  askname (filtn) "Filter Used \K\ ? "
  filtn = upcase(filtn)
  print "Change aperture size in each successive calculation ? "
  asklog (chap) "Change Aperture (Yes or No) \N\ ? "
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
  if filtn = "J"
    zp = zeroj
  end if
  if filtn = "H"
    zp = zeroh
  end if
  if filtn = "K"
    zp = zerok
  end if
  if filtn = "NBL"
    zp = zeronbl
  end if
  if filtn = "LP"
    zp = zerolp
  end if
  if filtn = "NBM"
    zp = zeronbm
  end if
  print "  Zeropoint to be used       = " (zp) " for " (filtn)
  loop for dummy = 1 to 1000
    get plt2d disp_ap1 (daper1)
    get plt2d disp_ap2 (daper2)
    get plt2d disp_ap3 (daper3)
    if (chap = 0 and dummy = 1) or chap = 1
      print "Size of STAR APERTURE to use (in arcsec) ? "
      print "CURRENT DEFAULT = " (daper1) " arcsec"
      asknum (aper) "Star Aperture Size \-1\ ? "
      if aper < 0.001
        aper = daper1
        print (aper) "arcsec assumed..."
      end if
      send plt2d set disp_ap1 (aper)
      print "Size of INNER SKY APERTURE to use (in arcsec) ? "
      print "CURRENT DEFAULT = " (daper2) " arcsec"
      asknum (aper2) "Inner Sky Aperture Size \-1\ ? "
      if aper2 < 0.001
        aper2 = daper2
        print (aper2) "arcsec assumed..."
      end if
      send plt2d set disp_ap2 (aper2)
      print "Size of OUTER SKY APERTURE to use (in arcsec) ? "
      print "CURRENT DEFAULT = " (daper3) " arcsec"
      asknum (aper3) "Outer Sky Aperture Size \-1\ ? "
      if aper3 < 0.001
        aper3 = daper3
        print (aper3) "arcsec assumed..."
      end if
      send plt2d set disp_ap3 (aper3)
    end if
    print "Pick the STAR with the cursor"
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
    obeyw plt2d circle (xobj) (yobj) (aper2)
    obeyw plt2d circle (xobj) (yobj) (aper3)
    print "  Aperture center (X,Y) = " (xobj) "," (yobj)
    print "Analysing image " (diffim)
    obeyw rapi2d APERADD (diffim) (xobj) (yobj) (aper) ~
      (scale) "N" "Y"
    getpar glob aperadd_total (objval)
    getpar glob aperadd_numpix (nob)
    obeyw rapi2d APERADD (diffim) (xobj) (yobj) (aper2) ~
      (scale) "N" "Y"
    getpar glob aperadd_total (skyval)
    getpar glob aperadd_numpix (nos)
    obeyw rapi2d APERADD (diffim) (xobj) (yobj) (aper3) ~
      (scale) "N" "Y"
    getpar glob aperadd_total (skyval2)
    getpar glob aperadd_numpix (nos2)
    skypp = ((skyval2-skyval)/(nos2-nos))/total
    objpp = objval/total
    objval = (objval-((skyval2-skyval)*(nob/(nos2-nos))))/total
    mag = zp - 2.5*(log(abs(objval))/log(10))
    if objval < 0
      print "WARNING : OBJECT-SKY gives negative value, absolute value used"
    end if
    print "  Zeropoint                                = " (zp)
    print "  Object counts/sec in aperture            = " (objpp)
    print "  Sky/pixel/sec in sky annulus             = " (skypp)
    print "  Star-Sky                                 = " (objval)
    print "  Magnitude                                = " (mag)
    print " "
    print "More PHOTOMETRY on this image ? "
    asklog (brave_man) "More Photometry \N\ ? "
    if brave_man = 0
      return
    end if
  end loop
end proc
