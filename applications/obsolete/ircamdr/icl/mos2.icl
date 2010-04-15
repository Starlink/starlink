{ PROCEDURE MOS2 : displays 2 images, get's stats and scales and mosaics
proc mos2 im1 im2
  yn1 = undefined(im1)
  yn2 = undefined(im2)
  if yn1
    askname (st1) "Name of 1st image to be matched ? "
  else
    st1 = im1
  end if
  if yn2
    askname (st2) "Name of 2nd image to be matched ? "
  else
   st2 = im2
  end if
  get plt2d workxcen (workxcen)
  get plt2d workycen (workycen)
  possx = workxcen*0.5
  possy = workycen*1.5
  obeyw plt2d clear
  get plt2d disp_mag (disp_mag)
  dflag = 0
  if disp_mag = 0.0
    get plt2d image_calmag (disp_mag)
    dflag = 1
  end if
  maggy = disp_mag/2.0
  print "Using DISP_MAG/2 magnification of " (maggy)
  send plt2d set magnification (maggy)
  send plt2d set im_xcen (possx)
  send plt2d set im_ycen (workycen)
  print "Plotting 1st image, name = " (st1)
  obeyw plt2d nsigma (st1)
  get plt2d im_xst (value5)
  get plt2d im_xen (value6)
  get plt2d im_yst (value7)
  get plt2d im_yen (value8)
  obeyw plt2d comment (st1) (possx) (possy) 15
  possx = workxcen*1.5
  possy = workycen*1.5
  send plt2d set im_xcen (possx)
  send plt2d set im_ycen (workycen)
  print "Plotting 2nd image, name = " (st2)
  obeyw plt2d nsigma (st2)
  obeyw plt2d comment (st2) (possx) (possy) 15
  print "Select centre of common box in RIGHT image, name = " (st2)
  send plt2d set cursor_image (st2)
  obeyw plt2d cursor
  get plt2d x_cur_pixel (x1)
  get plt2d y_cur_pixel (y1)
  print "X,Y position = " (x1) "," (y1)
  obeyw plt2d box (x1) (y1) 5 5 'CENTRE'
  print "Select centre of common box in LEFT  image, name = " (st1)
  send plt2d set im_xst (value5)
  send plt2d set im_xen (value6)
  send plt2d set im_yst (value7)
  send plt2d set im_yen (value8)
  send plt2d set cursor_image (st1)
  obeyw plt2d cursor
  get plt2d x_cur_pixel (x2)
  get plt2d y_cur_pixel (y2)
  print "X,Y position = " (x2) "," (y2)
  obeyw plt2d box (x2) (y2) 5 5 'CENTRE'
  xs = int(x1-2)
  ys = int(y1-2)
  obeyw rapi2d STATS (st2) (xs) (ys) 5 5 \
  getpar glob stats_median (med1)
  xs = int(x2-2)
  ys = int(y2-2)
  obeyw rapi2d STATS (st1) (xs) (ys) 5 5 \
  getpar glob stats_median (med2)
  diff = (med2-med1)
  off1 = int(x2-x1)
  off2 = int(y2-y1)
  get plt2d platscal (platscal)
  off1a = off1*platscal
  off2a = off2*platscal
  print "Image " (st2) " from image " (st1) " offsets are " (off1) "," (off2)
  print "  which translates to " (off1a) "," (off2a) " arcseconds"
  print "Adding " (diff) " to image " (st2)
  concat (st2) "x" (mos2o2)
  print "Output image will be called " (mos2o2)
  obeyw rapi2d CADD (st2) (diff) (mos2o2)
  print "Do you want to try a mosaic ?"
  asklog (brave_man) "Try it (Yes or No) \Y\ : "
  if brave_man <> 1
    return
  end if
  print "Mosaicing " (st1) " and " (mos2o2) " into mos2"
  print "Offset of 2nd image from 1st = " (off1) (off2)
  obeyw obsrap MOSAIC2 (st1) (mos2o2) (off1) (off2) mos2 \
  possy = workycen*1.5
  send plt2d set im_xcen (workxcen)
  send plt2d set im_ycen (workycen)
  print "Plotting mosaiced image ..."
  name_image = "mos2"
  send plt2d set name_image (name_image)
  if dflag = 1
    send plt2d set disp_mag 0
    send plt2d set magnification 0
  end if
  obeyw plt2d clear
  obeyw plt2d nsigma mos2
  obeyw plt2d comment 'Mosaiced Image' (workxcen) (possy) 15
end proc

