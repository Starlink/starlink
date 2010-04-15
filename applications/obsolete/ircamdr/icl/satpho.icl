proc satpho
{ xobj0 = 166
{ yobj0 = 114
xobj0 = 75
yobj0 = 109
ist = 207
ien = 1259
kk = 0
loop for jj = ist to ien
  kk = kk + 1
  name_image = "diff"&jj
  send plt2d set name_image (name_image)
  send plt2d set cursor_image (name_image)
  exp = 1.0
  ncoadds = 1
  filtn = "K"
  filtn = upcase(filtn)
  phons = 3
  send plt2d set magnification 0
  send plt2d set sigma_level (phons)
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
  imwork = 1
  if imwork = 1
    obeyw plt2d clear
    print "Plotting image " (name_image)
    obeyw plt2d nsigma (name_image)
    send plt2d set cursor_cross 'NO'
    get plt2d im_yen (value1)
    value1 = value1+15
    obeyw plt2d comment (name_image) (workxcen) (value1) 20
  end if
  diffim = name_image
  scale = 0.143
  get plt2d zeroj   (zeroj)
  get plt2d zeroh   (zeroh)
  get plt2d zerok   (zerok)
  get plt2d zeronbl (zeronbl)
  get plt2d zerolp  (zerolp)
  get plt2d zeronbm (zeronbm)
  get plt2d disp_ap1 (aper1)
  get plt2d disp_ap2 (aper2)
  get plt2d disp_ap3 (aper3)
  obeyw rapi2d shsize (diffim)
  getpar glob shsize_xdim (xdim)
  getpar glob shsize_ydim (ydim)
  xst = integer(xobj0-25.0)
  yst = integer(yobj0-25.0)
  std_sel = "A"
  if std_sel = "C"
    print "Select STAR with cursor"
    send plt2d set cursor_image (diffim)
    obeyw plt2d cursor
    get plt2d x_cur_pixel (xobj)
    get plt2d y_cur_pixel (yobj)
    print "Pixel selected = " (xobj) (yobj)
    xst = integer(xobj-11.0)
    yst = integer(yobj-11.0)
    obeyw rapi2d HISTO (diffim) (xst) (yst) 20 20 \
    getpar glob histo_max (maxval)
    getpar glob histo_xmax (xobj)
    getpar glob histo_ymax (yobj)
  else
    if imwork = 1
      obeyw plt2d box (xobj0) (yobj0) 8.0 8.0 'CENTRE'
      print "Box (8x8 arcsec) for peak pixel search displayed on image"
    end if
    obeyw rapi2d HISTO (diffim) (xst) (yst) 50 50 \
    getpar glob histo_max (maxval)
    getpar glob histo_min (minval)
    getpar glob histo_xmax (xobj)
    getpar glob histo_ymax (yobj)
  end if
  print "Maximum signal of " (maxval) " found at " (xobj) (yobj)
  if imwork = 1
    obeyw plt2d circle (xobj) (yobj) (aper1)
    obeyw plt2d circle (xobj) (yobj) (aper2)
    obeyw plt2d circle (xobj) (yobj) (aper3)
  end if
  print "Photometry apertures displayed on image"
  print "Analysing image " (diffim)
  scale = 0.143
  sap1 = aper1/scale
  sap2 = aper2/scale
  sap3 = aper3/scale
  sscale = 1.0
  obeyw obsrap APERPHOT (diffim) T x T Y (xobj) (yobj) (sap1) ~
    (sap2) (sap3) (sscale) N \
  getpar glob aperphot_med2 (skypp)
  getpar glob aperphot_objsky (objval)
  getpar glob aperphot_mag (mag)
  skypp = real(skypp)
  objval = real(objval)
  mag = real(mag)
  if objval < 0
    print "WARNING : OBJECT-SKY gives negative value, absolute value used"
  end if
  print "  Sky/pixel in sky annulus                 = " (skypp)
  print "  Star-Sky                                 = " (objval)
  print "  Magnitude                                = " (mag)
  print " "
  phbfile = file_exists("satpho.res")
  fclose_a
  if phbfile
    append afile "satpho.res"
  else
    create afile "satpho.res"
  end if
  disp_ap1 = real(aper1)
  disp_ap2 = real(aper2)
  disp_ap3 = real(aper3)
  scale = real(scale)
  cd1 = disp_ap1:6:2
  cd2 = disp_ap2:6:2
  cd3 = disp_ap3:6:2
  carc = scale:8:3
  cobjval = objval:12:5
  cmag = mag:8:3
  dline = kk&" "&jj&"  "&cobjval&"  "&cmag&"  "&cd1&" "&cd2&" "&cd3&" "&carc
  write afile (dline)
  fclose_a
end loop
end proc
