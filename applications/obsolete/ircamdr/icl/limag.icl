{ PROCEDURE LIMAG : procedure to calculate sky noise lim mag.
proc limag bsiz filt expt
  set precision 3
  yn1 = undefined(bsiz)
  yn2 = undefined(filt)
  yn3 = undefined(expt)
  get plt2d name_image (name_image)
  print "Image being used for LIMAG = " (name_image)
  if yn1
    asknum (xsiz) "Enter the sky box X-size in pixels \20\ : "
    asknum (ysiz) "Enter the sky box Y-size in pixels \20\ : "
  else
    xsiz = bsiz
    ysiz = bsiz
  end if
  if yn2
    print "Give filter used : "
    askname (filt2) "Filter (J,H,K,nbL,LP,nbM) \K\ ? "
  else
    filt2 = filt
  end if
  filt2 = upcase(filt2)
  if filt2 = "J"
    ifilt = 1
  else if filt2 = "H"
    ifilt = 2
  else if filt2 = "K"
    ifilt = 3
  else if filt2 = "NBL"
    ifilt = 4
  else if filt2 = "LP"
    ifilt = 5
  else if filt2 = "NBM"
    ifilt = 6
  else
    ifilt2 = -999
  end if
  if yn3
    print "Give exposure time of image in seconds : "
    asknum (expt2) "Exposure Time \1.0\ ? "
  else
    expt2 = expt
  end if
  send plt2d set cursor_image (name_image)
  print "Select the CENTRE of the box for sky noise calculation ..."
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
  obeyw rapi2d STATS (name_image) (xstb) (ystb) (xsiz) (ysiz) \
  getpar glob stats_mean (meanval)
  getpar glob stats_std (onesig)
  meanval = meanval/expt2
  onesig = onesig/expt2
  get plt2d zeroj   (zeroj)
  get plt2d zeroh   (zeroh)
  get plt2d zerok   (zerok)
  get plt2d zeronbl (zeronbl)
  get plt2d zerolp  (zerolp)
  get plt2d zeronbm (zeronbm)
  if ifilt = 1
    zp = zeroj
  else if ifilt = 2
    zp = zeroh
  else if ifilt = 3
    zp = zerok
  else if ifilt = 4
    zp = zeronbl
  else if ifilt = 5
    zp = zerolp
  else if ifilt = 6
    zp = zeronbm
  end if
  get plt2d platscal (pscal)
  nump = (3.1415926*((3.0/2.0)/pscal)**2)
  sqanump = (1.0/(pscal**2))
  apcor = -2.5*(log(abs(sqrt(nump)))/log(10))
  sqacor = -2.5*(log(abs(sqrt(sqanump)))/log(10))
  fivesig = onesig*5.0
  tensig = onesig*10.0
  hunsig = onesig*100.0
  onemag = zp - 2.5*(log(abs(onesig))/log(10))
  fivemag = zp - 2.5*(log(abs(fivesig))/log(10))
  tenmag = zp - 2.5*(log(abs(tensig))/log(10))
  hunmag = zp - 2.5*(log(abs(hunsig))/log(10))
  aonemag = onemag + sqacor
  afivemag = fivemag + sqacor
  atenmag = tenmag + sqacor
  ahunmag = hunmag + sqacor
  oneap = onemag + apcor
  fiveap = fivemag + apcor
  tenap = tenmag + apcor
  hunap = hunmag + apcor
  print "Results from LIMAG"
  print "Sky box centred pixels            = " (xpix1) (ypix1)
  print "Pixel scale                       = " (pscal)
  print "Number pixels in 3arcsec aperture = " (nump)
  print "Zeropoint (1DN/s)                 = " (zp)
  print "Mean signal on sky (DN)           = " (meanval)
  print " "
  print "1- 5- 10- 100-sigma limiting mag/pixel (DN)        = " (onesig) ~
    (fivesig) (tensig) (hunsig)
  print "1- 5- 10- 100-sigma limiting mag/pixel (mag)       = " (onemag) ~
    (fivemag) (tenmag) (hunmag)
  print "1- 5- 10- 100-sigma limiting mag/sq arcsec (mag)   = " (aonemag) ~
    (afivemag) (atenmag) (ahunmag)
  print "1- 5- 10- 100-sigma limiting mag/3 arcsec ap (mag) = " (oneap) ~
    (fiveap) (tenap) (hunap)
end proc
