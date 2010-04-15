{ PROCEDURE CHDISP : plots CHOP phases difference on workstation
proc chdisp value1
  get plt2d image_workstn (image_workstn)
  if image_workstn <> 1
    print "CHDISP is not available from this terminal!"
    return
  end if
  get plt2d zeroj   (zeroj)
  get plt2d zeroh   (zeroh)
  get plt2d zerok   (zerok)
  get plt2d zeronbl (zeronbl)
  get plt2d zerolp  (zerolp)
  get plt2d zeronbm (zeronbm)
  get plt2d filetype (filetype)
  get plt2d name_prefix (name_prefix)
  if name_prefix = "UNKNOWN"
    print "Sorry, you must define the IRCAM data file before running CHDISP"
    print "Use the command SETPRE to do this ..."
    return
  end if
  yn1 = undefined(value1)
  if yn1
    junk1 = -999
  else
    junk1 = value1
  end if
  if junk1 < 1 or junk1 > 5000
    print "Give CHOP observation number (1-5000) : "
    asknum (junk1) "Chop Obs. \1\ ? "
  end if
  temp = 1
  if filetype <> 1
    formname (junk1) (temp) (name_image)
  else
    formname2 (junk1) (name_image)
  end if
  print "Forming Phase A-B from obs. " (junk1)
  name_image2 = name_image&"a"
  name_out = name_image&"b"
  name_image = name_image2
  st4 = "f"&junk1&"amb"
  print "Difference image will be called " (st4)
  obeyw rapi2d SUB (name_image) (name_out) (st4) 'a-b'
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
  send plt2d set cursor_image (st4)
  print "Plotting image " (st4)
  obeyw plt2d nsigma (st4)
  obeyw plt2d border 2
  send plt2d set cursor_cross 'NO'
  get plt2d im_yen (v1)
  v1 = v1+20
  obeyw plt2d comment (st4) (workxcen) (v1) 20
{  print "Plot COLOUR BAR next to image with scale ?"
{  asklog (brave_man) "Colour Bar (Yes or No) /N/ ? "
  brave_man = 0
  if brave_man = 1
    get plt2d im_yst (v1)
    v1 = integer(v1)-15
    send plt2d set ct_annotation 'ANNOTATE'
    send plt2d set blocknum_where 'B'
    obeyw plt2d block (workxcen) (v1) 3 'H'
  end if
  print "Do you want PHOTOMETRY on the difference image ?"
  asklog (brave_man) "Photometry \N\ ? "
  send plt2d set name_image (st4)
  if brave_man = 0
    return
  else
    diffim = st4
  end if
  if filetype <> 1
    print "Reading header information from data file ... please wait"
    obeyw plt2d contpars (cnum)
    get plt2d number_coadds (ncoadds)
    get plt2d exposure_time (exp)
    get plt2d filter (filtn)
    filtn = upcase(filtn)
    exp = exp/1000
    print "  Image COADDS               = " (ncoadds)
  else
    ncoadds = 1
    tim = name_image
    print "Getting header info from file " (tim)
    obeyw plt2d ropars (tim)
    get plt2d object_name   (object)
    get plt2d filter        (filtn)
    get plt2d exposure_time (exp)
    get plt2d number_exp    (nexp)
    get plt2d airmass_start (amst)
    get plt2d airmass_end   (amen)
    get plt2d ra_off        (raoff)
    get plt2d dec_off       (decoff)
    object = upcase(object)
    filtn = upcase(filtn)
    airmass = (amst+amen)/2.0
    if filtn = "J"
      filtn = "BROAD J"
    end if
    if filtn = "H"
      filtn = "BROAD H"
    end if
    if filtn = "K"
      filtn = "BROAD K"
    end if
    if filtn = "NBL"
      filtn = "NB L"
    end if
    if filtn = "LP"
      filtn = "LP"
    end if
    if filtn = "NBM"
      filtn = "NB M"
    end if
  end if
  get plt2d platscal (scale)
  print "  Image EXPOSURE TIME (S)    = " (exp)
  print "  FILTER used                = " (filtn)
  print "  Plate scale (arcsec/pixel) = " (scale)
  total=ncoadds*exp
  zp = 0
  compare (filtn) "BROAD J" (brave_man)
  if brave_man = 1
    zp = zeroj
  end if
  compare (filtn) "BROAD H" (brave_man)
  if brave_man = 1
    zp = zeroh
  end if
  compare (filtn) "BROAD K" (brave_man)
  if brave_man = 1
    zp = zerok
  end if
  compare (filtn) "NB L" (brave_man)
  if brave_man = 1
    zp = zeronbl
  end if
  compare (filtn) "LP" (brave_man)
  if brave_man = 1
    zp = zerolp
  end if
  compare (filtn) "NB M" (brave_man)
  if brave_man = 1
    zp = zeronbm
  end if
  print "  Zeropoint to be used       = " (zp)
  loop for dummy = 1 to 1000
    print "What APERTURE size do you want to use (in arcsec) ?"
    asknum (aper) "Aperture Size \4\ ? "
    print "Pick the OBJECT with the cursor"
    obeyw plt2d cursor
    get plt2d x_cur_pixel (xobj)
    get plt2d y_cur_pixel (yobj)
    obeyw plt2d circle (xobj) (yobj) (aper)
    print "  OBJECT aperture center (X,Y) = " (xobj) "," (yobj)
    print "Pick SKY with the cursor"
    obeyw plt2d cursor
    get plt2d x_cur_pixel (xsky)
    get plt2d y_cur_pixel (ysky)
    obeyw plt2d circle (xsky) (ysky) (aper)
    print "  SKY aperture center (X,Y)    = " (xsky) "," (ysky)
    print "Analysing image " (diffim)
    obeyw rapi2d APERADD (diffim) (xobj) (yobj) (aper) (scale) "N"
    getpar glob aperadd_total (objval)
    obeyw rapi2d APERADD (diffim) (xsky) (ysky) (aper) (scale) "N"
    getpar glob aperadd_total (skyval)
    print "Analysing image " (name_image)
    obeyw rapi2d APERADD (name_image) (xsky) (ysky) (aper) (scale) "N"
    getpar glob aperadd_total (skyval0)
    print "Analysing image " (name_out)
    obeyw rapi2d APERADD (name_out) (xsky) (ysky) (aper) (scale) "N"
    getpar glob aperadd_total (skyval1)
    objval = (objval-skyval)/total
    mag = zp - 2.5*(log(abs(objval))/log(10))
    if objval < 0
      print "WARNING : OBJECT-SKY gives negative value, absolute value used"
    end if
    skych = (skyval0-skyval1)/(skyval0)*100
    print "  Zeropoint                                = " (zp)
    print "  Object-Sky (DN/s)                        = " (objval)
    print "  Magnitude                                = " (mag)
    print "  Sky change between OBJECT and SKY images = " (skych) "%"
    print "More PHOTOMETRY on this image ?"
    asklog (brave_man) "More Photometry \N\ ? "
    if brave_man = 0
      return
    end if
  end loop
end proc

