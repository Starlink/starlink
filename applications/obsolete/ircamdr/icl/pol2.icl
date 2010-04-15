{ PROCEDURE POL2 : Polarimetry on 4 image with concentric apertures
proc pol2
  jxdisp = -1
  jydisp = 73
  hxdisp = -1
  hydisp =71
  kxdisp = -1
  kydisp = 69
  nblxdisp = -1
  nblydisp = 61
  obeyw plt2d line_width 2
  get plt2d image_workstn (image_workstn)
  if image_workstn <> 1
    print "POL2 is not available from this terminal"
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
  print "Give name of    O DEG image :"
  askname (name_image1) "0 deg image  \pol0\ ? "
  print "Give name of   45 DEG image :"
  askname (name_image2) "45 deg image \pol45\ ? "
  print "Give name of 22.5 DEG image :"
  askname (name_image3) "22.5 deg image \pol22\ ? "
  print "Give name of 67.5 DEG image :"
  askname (name_image4) "67.5 deg image \pol67\ ? "
  send plt2d set name_image (name_image1)
  send plt2d set cursor_image (name_image1)
  print "Exposure time of image in seconds"
  print "  (1.0 if images from STRED)"
  print "  (actual if images straight difference e.g. SUB, DISP)  :"
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
  get plt2d platscal (scale)
  if filetype <> 1
    print "  Image COADDS               = " (ncoadds)
  end if
  if filtn = "J"
    xdisp = jxdisp
    ydisp = jydisp
  else if filtn = "H"
    xdisp = hxdisp
    ydisp = hydisp
  else if filtn = "K"
    xdisp = kxdisp
    ydisp = kydisp
  else if filtn = "NBL"
    xdisp = nblxdisp
    ydisp = nblydisp
  else
    print "Filter not J,H,K or nbL, no idea what x,y displacements"
    print "between o- and e- ray images are, you must enter them!"
    asknum (xdisp) "Enter X displacement between o- and -e ray images"
    asknum (ydisp) "Enter Y displacement between o- and -e ray images"
  end if
  print "  Image EXPOSURE TIME (S)    = " (exp)
  print "  FILTER used                = " (filtn)
  print "  X,Y o-, e- displacements   = " (xdisp) (ydisp)
  print "  Plate scale (arcsec/pixel) = " (scale)
  get plt2d zeroj   (zeroj)
  get plt2d zeroh   (zeroh)
  get plt2d zerok   (zerok)
  get plt2d zeronbl (zeronbl)
  get plt2d zerolp  (zerolp)
  get plt2d zeronbm (zeronbm)
  total=real(ncoadds*exp)
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
  get plt2d disp_ap1 (daper1)
  get plt2d disp_ap2 (daper2)
  get plt2d disp_ap3 (daper3)
  print "Size of STAR APERTURE to use (in arcsec) ? "
  print "CURRENT DEFAULT = " (daper1) " arcsec"
  asknum (aper1) "Star Aperture Size \-1\ ? "
  if aper1 < 0.001
    aper1 = daper1
    print (aper1) "arcsec assumed..."
  end if
  send plt2d set disp_ap1 (aper1)
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
  print "Optimize aperture on peak pixel ? "
  asklog (optp) "Optimize on Peak \Y\ ? "
  if optp = 1
    peak2 = "Y"
  else
    peak2 = "N"
  end if

{ Image at 0 degrees...
  diffim = name_image1
  obeyw plt2d clear
  print "Plotting image " (name_image1)
  obeyw plt2d nsigma (name_image1)
  send plt2d set cursor_cross 'NO'
  get plt2d im_yen (value1)
  value1 = value1+15
  obeyw plt2d comment (name_image1) (workxcen) (value1) 20
  print "Pick the southern (o-ray) of the two star images with cursor"
  obeyw plt2d cursor
  get plt2d x_cur_pixel (xobj)
  get plt2d y_cur_pixel (yobj)
  xobj = integer(xobj)
  yobj = integer(yobj)
  if peak2 = "Y"
    xobj0 = integer(xobj)
    yobj0 = integer(yobj)
    xst = integer(xobj0-7.0)
    yst = integer(yobj0-7.0)
    obeyw rapi2d HISTO (name_image1) (xst) (yst) 13 13 \
    getpar glob histo_max (maxval)
    getpar glob histo_min (minval)
    getpar glob histo_xmax (xobjo1)
    getpar glob histo_ymax (yobjo1)
    print "Pixel selected with cursor = " (xobj0) (yobj0)
    print "Maximum signal of " (maxval) " found at " (xobjo1) (yobjo1)
  else
    xobjo1 = xobj
    yobjo1 = yobj
  end if
  obeyw plt2d circle (xobjo1) (yobjo1) (aper1)
  obeyw plt2d circle (xobjo1) (yobjo1) (aper2)
  obeyw plt2d circle (xobjo1) (yobjo1) (aper3)
  sap1 = aper1/scale
  sap2 = aper2/scale
  sap3 = aper3/scale
  sscale = 1.0
  obeyw obsrap APERPHOT (diffim) T x T Y (xobjo1) (yobjo1) (sap1) ~
    (sap2) (sap3) (sscale) N \
  getpar glob aperphot_med2 (skyppo1)
  getpar glob aperphot_objsky (obo1)
  getpar glob aperphot_mag (mago1)
  skyppo1 = real(skyppo1)/total
  obo1 = real(obo1)/total
  mago1 = real(mago1)
  if obo1 < 0
    print "WARNING : OBJECT-SKY gives negative value, absolute value used"
  end if
  print "  o- beam 0 degree image " (diffim)
  print "  Sky/pixel in sky annulus                 = " (skyppo1)
  print "  Star-Sky                                 = " (obo1)
{  print "  Magnitude                                = " (mago1)
  print " "
  xobj = xobj+xdisp
  yobj = yobj+ydisp
{  print "Pick the northern (e-ray) of the two star images with cursor"
{  obeyw plt2d cursor
{  get plt2d x_cur_pixel (xobj)
{  get plt2d y_cur_pixel (yobj)
  xobj = integer(xobj)
  yobj = integer(yobj)
  if peak2 = "Y"
    xobj0 = integer(xobj)
    yobj0 = integer(yobj)
    xst = integer(xobj0-7.0)
    yst = integer(yobj0-7.0)
    obeyw rapi2d HISTO (name_image1) (xst) (yst) 13 13 \
    getpar glob histo_max (maxval)
    getpar glob histo_min (minval)
    getpar glob histo_xmax (xobje1)
    getpar glob histo_ymax (yobje1)
    print "Pixel selected with cursor = " (xobj0) (yobj0)
    print "Maximum signal of " (maxval) " found at " (xobje1) (yobje1)
  else
    xobje1 = xobj
    yobje1 = yobj
  end if
  obeyw plt2d circle (xobje1) (yobje1) (aper1)
  obeyw plt2d circle (xobje1) (yobje1) (aper2)
  obeyw plt2d circle (xobje1) (yobje1) (aper3)
  sap1 = aper1/scale
  sap2 = aper2/scale
  sap3 = aper3/scale
  sscale = 1.0
  obeyw obsrap APERPHOT (diffim) T x T Y (xobje1) (yobje1) (sap1) ~
    (sap2) (sap3) (sscale) N \
  getpar glob aperphot_med2 (skyppe1)
  getpar glob aperphot_objsky (obe1)
  getpar glob aperphot_mag (mage1)
  skyppe1 = real(skyppe1)/total
  obe1 = real(obe1)/total
  mage1 = real(mage1)
  if obe1 < 0
    print "WARNING : OBJECT-SKY gives negative value, absolute value used"
  end if
  print "  e- beam 0 degree image " (diffim)
  print "  Sky/pixel in sky annulus                 = " (skyppe1)
  print "  Star-Sky                                 = " (obe1)
{  print "  Magnitude                                = " (mage1)
  print " "

{ Image at 45 degrees...
  diffim = name_image2
  obeyw plt2d clear
  print "Plotting image " (name_image2)
  obeyw plt2d nsigma (name_image2)
  send plt2d set cursor_cross 'NO'
  get plt2d im_yen (value1)
  value1 = value1+15
  obeyw plt2d comment (name_image2) (workxcen) (value1) 20
  xobj = xobjo1
  yobj = yobjo1
  xobj = integer(xobj)
  yobj = integer(yobj)
  if peak2 = "Y"
    xobj0 = integer(xobj)
    yobj0 = integer(yobj)
    xst = integer(xobj0-7.0)
    yst = integer(yobj0-7.0)
    obeyw rapi2d HISTO (name_image2) (xst) (yst) 13 13 \
    getpar glob histo_max (maxval)
    getpar glob histo_min (minval)
    getpar glob histo_xmax (xobjo2)
    getpar glob histo_ymax (yobjo2)
    print "Pixel selected with cursor = " (xobj0) (yobj0)
    print "Maximum signal of " (maxval) " found at " (xobjo2) (yobjo2)
  else
    xobjo2 = xobj
    yobjo2 = yobj
  end if
  obeyw plt2d circle (xobjo2) (yobjo2) (aper1)
  obeyw plt2d circle (xobjo2) (yobjo2) (aper2)
  obeyw plt2d circle (xobjo2) (yobjo2) (aper3)
  sap1 = aper1/scale
  sap2 = aper2/scale
  sap3 = aper3/scale
  sscale = 1.0
  obeyw obsrap APERPHOT (diffim) T x T Y (xobjo2) (yobjo2) (sap1) ~
    (sap2) (sap3) (sscale) N \
  getpar glob aperphot_med2 (skyppo2)
  getpar glob aperphot_objsky (obo2)
  getpar glob aperphot_mag (mago2)
  skyppo2 = real(skyppo2)/total
  obo2 = real(obo2)/total
  mago2 = real(mago2)
  if obo2 < 0
    print "WARNING : OBJECT-SKY gives negative value, absolute value used"
  end if
  print "  o- beam 45 degree image " (diffim)
  print "  Sky/pixel in sky annulus                 = " (skyppo2)
  print "  Star-Sky                                 = " (obo2)
{  print "  Magnitude                                = " (mago2)
  print " "
  xobj = xobje1
  yobj = yobje1
  xobj = integer(xobj)
  yobj = integer(yobj)
  if peak2 = "Y"
    xobj0 = integer(xobj)
    yobj0 = integer(yobj)
    xst = integer(xobj0-7.0)
    yst = integer(yobj0-7.0)
    obeyw rapi2d HISTO (name_image2) (xst) (yst) 13 13 \
    getpar glob histo_max (maxval)
    getpar glob histo_min (minval)
    getpar glob histo_xmax (xobje2)
    getpar glob histo_ymax (yobje2)
    print "Pixel selected with cursor = " (xobj0) (yobj0)
    print "Maximum signal of " (maxval) " found at " (xobje2) (yobje2)
  else
    xobje2 = xobj
    yobje2 = yobj
  end if
  obeyw plt2d circle (xobje2) (yobje2) (aper1)
  obeyw plt2d circle (xobje2) (yobje2) (aper2)
  obeyw plt2d circle (xobje2) (yobje2) (aper3)
  sap1 = aper1/scale
  sap2 = aper2/scale
  sap3 = aper3/scale
  sscale = 1.0
  obeyw obsrap APERPHOT (diffim) T x T Y (xobje2) (yobje2) (sap1) ~
    (sap2) (sap3) (sscale) N \
  getpar glob aperphot_med2 (skyppe2)
  getpar glob aperphot_objsky (obe2)
  getpar glob aperphot_mag (mage2)
  skyppe2 = real(skyppe2)/total
  obe2 = real(obe2)/total
  mage2 = real(mage2)
  if obe2 < 0
    print "WARNING : OBJECT-SKY gives negative value, absolute value used"
  end if
  print "  e- beam 45 degree image " (diffim)
  print "  Sky/pixel in sky annulus                 = " (skyppe2)
  print "  Star-Sky                                 = " (obe2)
{  print "  Magnitude                                = " (mage2)
  print " "

{ Image at 22.5 degrees...
  diffim = name_image3
  obeyw plt2d clear
  print "Plotting image " (name_image3)
  obeyw plt2d nsigma (name_image3)
  send plt2d set cursor_cross 'NO'
  get plt2d im_yen (value1)
  value1 = value1+15
  obeyw plt2d comment (name_image3) (workxcen) (value1) 20
  xobj = xobjo1
  yobj = yobjo1
  xobj = integer(xobj)
  yobj = integer(yobj)
  if peak2 = "Y"
    xobj0 = integer(xobj)
    yobj0 = integer(yobj)
    xst = integer(xobj0-7.0)
    yst = integer(yobj0-7.0)
    obeyw rapi2d HISTO (name_image3) (xst) (yst) 13 13 \
    getpar glob histo_max (maxval)
    getpar glob histo_min (minval)
    getpar glob histo_xmax (xobjo3)
    getpar glob histo_ymax (yobjo3)
    print "Pixel selected with cursor = " (xobj0) (yobj0)
    print "Maximum signal of " (maxval) " found at " (xobjo3) (yobjo3)
  else
    xobjo3 = xobj
    yobjo3 = yobj
  end if
  obeyw plt2d circle (xobjo3) (yobjo3) (aper1)
  obeyw plt2d circle (xobjo3) (yobjo3) (aper2)
  obeyw plt2d circle (xobjo3) (yobjo3) (aper3)
  sap1 = aper1/scale
  sap2 = aper2/scale
  sap3 = aper3/scale
  sscale = 1.0
  obeyw obsrap APERPHOT (diffim) T x T Y (xobjo3) (yobjo3) (sap1) ~
    (sap2) (sap3) (sscale) N \
  getpar glob aperphot_med2 (skyppo3)
  getpar glob aperphot_objsky (obo3)
  getpar glob aperphot_mag (mago3)
  skyppo3 = real(skyppo3)/total
  obo3 = real(obo3)/total
  mago3 = real(mago3)
  if obo3 < 0
    print "WARNING : OBJECT-SKY gives negative value, absolute value used"
  end if
  print "  o- beam 22.5 degree image " (diffim)
  print "  Sky/pixel in sky annulus                 = " (skyppo3)
  print "  Star-Sky                                 = " (obo3)
{  print "  Magnitude                                = " (mago3)
  print " "
  xobj = xobje1
  yobj = yobje1
  xobj = integer(xobj)
  yobj = integer(yobj)
  if peak2 = "Y"
    xobj0 = integer(xobj)
    yobj0 = integer(yobj)
    xst = integer(xobj0-7.0)
    yst = integer(yobj0-7.0)
    obeyw rapi2d HISTO (name_image3) (xst) (yst) 13 13 \
    getpar glob histo_max (maxval)
    getpar glob histo_min (minval)
    getpar glob histo_xmax (xobje3)
    getpar glob histo_ymax (yobje3)
    print "Pixel selected with cursor = " (xobj0) (yobj0)
    print "Maximum signal of " (maxval) " found at " (xobje3) (yobje3)
  else
    xobje3 = xobj
    yobje3 = yobj
  end if
  obeyw plt2d circle (xobje3) (yobje3) (aper1)
  obeyw plt2d circle (xobje3) (yobje3) (aper2)
  obeyw plt2d circle (xobje3) (yobje3) (aper3)
  sap1 = aper1/scale
  sap2 = aper2/scale
  sap3 = aper3/scale
  sscale = 1.0
  obeyw obsrap APERPHOT (diffim) T x T Y (xobje3) (yobje3) (sap1) ~
    (sap2) (sap3) (sscale) N \
  getpar glob aperphot_med2 (skyppe3)
  getpar glob aperphot_objsky (obe3)
  getpar glob aperphot_mag (mage3)
  skyppe3 = real(skyppe3)/total
  obe3 = real(obe3)/total
  mage3 = real(mage3)
  if obe3 < 0
    print "WARNING : OBJECT-SKY gives negative value, absolute value used"
  end if
  print "  e- beam 22.5 degree image " (diffim)
  print "  Sky/pixel in sky annulus                 = " (skyppe3)
  print "  Star-Sky                                 = " (obe3)
{  print "  Magnitude                                = " (mage3)
  print " "

{ Image at 67 degrees...
  diffim = name_image4
  obeyw plt2d clear
  print "Plotting image " (name_image4)
  obeyw plt2d nsigma (name_image4)
  send plt2d set cursor_cross 'NO'
  get plt2d im_yen (value1)
  value1 = value1+15
  obeyw plt2d comment (name_image4) (workxcen) (value1) 20
  xobj = xobjo1
  yobj = yobjo1
  xobj = integer(xobj)
  yobj = integer(yobj)
  if peak2 = "Y"
    xobj0 = integer(xobj)
    yobj0 = integer(yobj)
    xst = integer(xobj0-7.0)
    yst = integer(yobj0-7.0)
    obeyw rapi2d HISTO (name_image4) (xst) (yst) 13 13 \
    getpar glob histo_max (maxval)
    getpar glob histo_min (minval)
    getpar glob histo_xmax (xobjo4)
    getpar glob histo_ymax (yobjo4)
    print "Pixel selected with cursor = " (xobj0) (yobj0)
    print "Maximum signal of " (maxval) " found at " (xobjo4) (yobjo4)
  else
    xobjo4 = xobj
    yobjo4 = yobj
  end if
  obeyw plt2d circle (xobjo4) (yobjo4) (aper1)
  obeyw plt2d circle (xobjo4) (yobjo4) (aper2)
  obeyw plt2d circle (xobjo4) (yobjo4) (aper3)
  sap1 = aper1/scale
  sap2 = aper2/scale
  sap3 = aper3/scale
  sscale = 1.0
  obeyw obsrap APERPHOT (diffim) T x T Y (xobjo4) (yobjo4) (sap1) ~
    (sap2) (sap3) (sscale) N \
  getpar glob aperphot_med2 (skyppo4)
  getpar glob aperphot_objsky (obo4)
  getpar glob aperphot_mag (mago4)
  skyppo4 = real(skyppo4)/total
  obo4 = real(obo4)/total
  mago4 = real(mago4)
  if obo4 < 0
    print "WARNING : OBJECT-SKY gives negative value, absolute value used"
  end if
  print "  o- beam 67.5 degree image " (diffim)
  print "  Sky/pixel in sky annulus                 = " (skyppo4)
  print "  Star-Sky                                 = " (obo4)
{  print "  Magnitude                                = " (mago4)
  print " "
  xobj = xobje1
  yobj = yobje1
  xobj = integer(xobj)
  yobj = integer(yobj)
  if peak2 = "Y"
    xobj0 = integer(xobj)
    yobj0 = integer(yobj)
    xst = integer(xobj0-7.0)
    yst = integer(yobj0-7.0)
    obeyw rapi2d HISTO (name_image4) (xst) (yst) 13 13 \
    getpar glob histo_max (maxval)
    getpar glob histo_min (minval)
    getpar glob histo_xmax (xobje4)
    getpar glob histo_ymax (yobje4)
    print "Pixel selected with cursor = " (xobj0) (yobj0)
    print "Maximum signal of " (maxval) " found at " (xobje4) (yobje4)
  else
    xobje4 = xobj
    yobje4 = yobj
  end if
  obeyw plt2d circle (xobje4) (yobje4) (aper1)
  obeyw plt2d circle (xobje4) (yobje4) (aper2)
  obeyw plt2d circle (xobje4) (yobje4) (aper3)
  sap1 = aper1/scale
  sap2 = aper2/scale
  sap3 = aper3/scale
  sscale = 1.0
  obeyw obsrap APERPHOT (diffim) T x T Y (xobje4) (yobje4) (sap1) ~
    (sap2) (sap3) (sscale) N \
  getpar glob aperphot_med2 (skyppe4)
  getpar glob aperphot_objsky (obe4)
  getpar glob aperphot_mag (mage4)
  skyppe4 = real(skyppe4)/total
  obe4 = real(obe4)/total
  mage4 = real(mage4)
  if obe4 < 0
    print "WARNING : OBJECT-SKY gives negative value, absolute value used"
  end if
  print "  e- beam 67.5 degree image " (diffim)
  print "  Sky/pixel in sky annulus                 = " (skyppe4)
  print "  Star-Sky                                 = " (obe4)
{  print "  Magnitude                                = " (mage4)
  print " "

{ Calculate polarization from intensities
  obeyw polrap polly2 (obo1) (obe1) (obo2) (obe2) (obo3) (obe3) ~
    (obo4) (obe4) 6 (zp) \
end proc
