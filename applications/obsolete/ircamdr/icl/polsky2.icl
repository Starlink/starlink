{ Procedure POLSKY2 : subtracts median of area in 8 images from images
proc polsky2
  askname (polly1o) "   0 degrees o-ray image  \pol0ar\  ? "
  askname (polly1e) "   0 degrees e-ray image  \pol0br\  ? "
  askname (polly2o) "  45 degrees o-ray image \pol45ar\  ? "
  askname (polly2e) "  45 degrees e-ray image \pol45br\  ? "
  askname (polly3o) "22.5 degrees o-ray image \pol22ar\  ? "
  askname (polly3e) "22.5 degrees e-ray image \pol22br\  ? "
  askname (polly4o) "67.5 degrees o-ray image \pol67ar\  ? "
  askname (polly4e) "67.5 degrees e-ray image \pol67br\  ? "
  out1o = polly1o&"s"
  out1e = polly1e&"s"
  out2o = polly2o&"s"
  out2e = polly2e&"s"
  out3o = polly3o&"s"
  out3e = polly3e&"s"
  out4o = polly4o&"s"
  out4e = polly4e&"s"
  print "Output images will be called : "
  print (out1o) "," (out1e)
  print (out2o) "," (out2e)
  print (out3o) "," (out3e)
  print (out4o) "," (out4e)
  print "Plotting image " (polly1o)
  obeyw plt2d clear
  send plt2d set cursor_image (polly1o)
  obeyw plt2d nsigma (polly1o)
  print "Select BOTTOM-LEFT of SKY area using cursor"
  obeyw plt2d cursor
  get plt2d x_cur_pixel (xbl)
  get plt2d y_cur_pixel (ybl)
  print "Select TOP-RIGHT   of SKY area using cursor"
  obeyw plt2d cursor
  get plt2d x_cur_pixel (xtr)
  get plt2d y_cur_pixel (ytr)
  print "SKY area is from " (xbl) (ybl) " to " (xtr) (ytr)
  xbx = int(xtr-xbl+1)
  ybx = int(ytr-ybl+1)
  print "SKY box size (X,Y) = " (xbx) (ybx)
  if xbx < 4 or ybx < 4
    print "Error, BOX size must be greater or equal to 4x4 pixels"
    return
  end if
  get plt2d arcsec_pixel (ps)
  send plt2d set arcsec_pixel 1.0
  send plt2d set platscal 1.0
  send plt2d set cursor_cross 'NO'
  obeyw plt2d BOX (xbl) (ybl) (xbx) (ybx) 'BOTTOM_LEFT'
  send plt2d set arcsec_pixel (ps)
  send plt2d set platscal (ps)
  obeyw rapi2d STATS (polly1o) (xbl) (ybl) (xbx) (ybx) NO \
  getpar glob stats_median (med1o)
  obeyw rapi2d STATS (polly1e) (xbl) (ybl) (xbx) (ybx) NO \
  getpar glob stats_median (med1e)
  obeyw rapi2d STATS (polly2o) (xbl) (ybl) (xbx) (ybx) NO \
  getpar glob stats_median (med2o)
  obeyw rapi2d STATS (polly2e) (xbl) (ybl) (xbx) (ybx) NO \
  getpar glob stats_median (med2e)
  obeyw rapi2d STATS (polly3o) (xbl) (ybl) (xbx) (ybx) NO \
  getpar glob stats_median (med3o)
  obeyw rapi2d STATS (polly3e) (xbl) (ybl) (xbx) (ybx) NO \
  getpar glob stats_median (med3e)
  obeyw rapi2d STATS (polly4o) (xbl) (ybl) (xbx) (ybx) NO \
  getpar glob stats_median (med4o)
  obeyw rapi2d STATS (polly4e) (xbl) (ybl) (xbx) (ybx) NO \
  getpar glob stats_median (med4e)
  print "Median values in SKY box are : "
  print "Image " (polly1o) " = " (med1o)
  print "Image " (polly1e) " = " (med1e)
  print "Image " (polly2o) " = " (med2o)
  print "Image " (polly2e) " = " (med2e)
  print "Image " (polly3o) " = " (med3o)
  print "Image " (polly3e) " = " (med3e)
  print "Image " (polly4o) " = " (med4o)
  print "Image " (polly4e) " = " (med4e)
  print "Subtracting median values from each image respectively"
  obeyw rapi2d CSUB (polly1o) (med1o) (out1o)
  obeyw rapi2d CSUB (polly1e) (med1e) (out1e)
  obeyw rapi2d CSUB (polly2o) (med2o) (out2o)
  obeyw rapi2d CSUB (polly2e) (med2e) (out2e)
  obeyw rapi2d CSUB (polly3o) (med3o) (out3o)
  obeyw rapi2d CSUB (polly3e) (med3e) (out3e)
  obeyw rapi2d CSUB (polly4o) (med4o) (out4o)
  obeyw rapi2d CSUB (polly4e) (med4e) (out4e)
  print " "
  print "Output sky-subtracted image are :    0 deg. - " (out1o) (out1e)
  print "                                :   45 deg. - " (out2o) (out2e)
  print "                                : 22.5 deg. - " (out3o) (out3e)
  print "                                : 67.5 deg. - " (out4o) (out4e)
  print " "
end proc

