{ Procedure SKYSUB4 : subtracts median of area of 4 images image from images
proc skysub4
  print "Input image at  0 degrees : "
  askname (polly1) " 0 degrees image \F0\  ? "
  print "Input image at 45 degrees : "
  askname (polly2) "45 degrees image \F45\ ? "
  print "Input image at 22 degrees : "
  askname (polly3) "22 degrees image \F22\ ? "
  print "Input image at 67 degrees "
  askname (polly4) "67 degrees image \F67\ ? "
  concat (polly1) "s" (out1)
  concat (polly2) "s" (out2)
  concat (polly3) "s" (out3)
  concat (polly4) "s" (out4)
  print "Output images will be called : "
  print (out1) "," (out2) "," (out3) " and " (out4)
  print "Plotting image " (polly1)
  obeyw plt2d clear
  send plt2d set cursor_image (polly1)
  obeyw plt2d nsigma (polly1)
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
  obeyw plt2d box (xbl) (ybl) (xbx) (ybx) 'BOTTOM_LEFT'
  send plt2d set arcsec_pixel (ps)
  send plt2d set platscal (ps)
  obeyw rapi2d STATS (polly1) (xbl) (ybl) (xbx) (ybx) NO \
  getpar glob stats_median (med1)
  obeyw rapi2d STATS (polly2) (xbl) (ybl) (xbx) (ybx) NO \
  getpar glob stats_median (med2)
  obeyw rapi2d STATS (polly3) (xbl) (ybl) (xbx) (ybx) NO \
  getpar glob stats_median (med3)
  obeyw rapi2d STATS (polly4) (xbl) (ybl) (xbx) (ybx) NO \
  getpar glob stats_median (med4)
  print "Median values in SKY box are : "
  print "Image " (polly1) " = " (med1)
  print "Image " (polly2) " = " (med2)
  print "Image " (polly3) " = " (med3)
  print "Image " (polly4) " = " (med4)
  print "Subtracting median values from each image respectively"
  obeyw rapi2d CSUB (polly1) (med1) (out1)
  obeyw rapi2d CSUB (polly2) (med2) (out2)
  obeyw rapi2d CSUB (polly3) (med3) (out3)
  obeyw rapi2d CSUB (polly4) (med4) (out4)
  print " "
  print "Output sky-subtracted image = " (out1) (out2) (out3) (out4)
  print " "
end proc

