{ PROCEDURE POLREG : moves 4 polarization images to average position and trims
proc polreg
  set precision 6
  askname (polly_1) "Image at  0 degs.  \pol0\ : "
  askname (polly_3) "Image at 45 degs. \pol45\ : "
  askname (polly_2) "Image at 22 degs. \pol22\ : "
  askname (polly_4) "Image at 67 degs. \pol67\ : "
  out_1 = polly_1 & "r"
  out_2 = polly_2 & "r"
  out_3 = polly_3 & "r"
  out_4 = polly_4 & "r"
  print "Final output images will be called :"
  print (out_1) "," (out_2) "," (out_3) "," (out_4)
  obeyw rapi2d SHSIZE (polly_1) \
  getpar glob shsize_xdim (xsizim)
  getpar glob shsize_ydim (ysizim)
  print "Plotting image " (polly_1)
  obeyw plt2d clear
  send plt2d set cursor_image (polly_1)
  obeyw plt2d nsigma (polly_1)
  print "Image " (polly_1) " displayed, select registration STAR with cursor"
  obeyw plt2d cursor
  get plt2d x_cur_pixel (value1)
  get plt2d y_cur_pixel (value2)
  print "Pixel selected for registration star (X,Y) = " (value1) (value2)
  asknum (brave_man) "HIT RETURN to continue \0\ : "
  print "Calculating centroid of registration star in 4 images"
  obeyw rapi2d CENTROID (polly_1) (value1) (value2) \
  getpar glob centroid_xfinal (posx_1)
  getpar glob centroid_yfinal (posy_1)
  obeyw rapi2d CENTROID (polly_3) (value1) (value2) \
  getpar glob centroid_xfinal (posx_3)
  getpar glob centroid_yfinal (posy_3)
  obeyw rapi2d CENTROID (polly_2) (value1) (value2) \
  getpar glob centroid_xfinal (posx_2)
  getpar glob centroid_yfinal (posy_2)
  obeyw rapi2d CENTROID (polly_4) (value1) (value2) \
  getpar glob centroid_xfinal (posx_4)
  getpar glob centroid_yfinal (posy_4)
  print "X,Y centroid values of registration star : "
  print "Image " (polly_1) " X,Y = " (posx_1) (posy_1)
  print "Image " (polly_3) " X,Y = " (posx_3) (posy_3)
  print "Image " (polly_2) " X,Y = " (posx_2) (posy_2)
  print "Image " (polly_4) " X,Y = " (posx_4) (posy_4)
  print "Calculating AVERAGE position in X and Y ..."
  averx = ( posx_1 + posx_2 + posx_3 + posx_4)/4.0
  avery = ( posy_1 + posy_2 + posy_3 + posy_4)/4.0
  print "Average position of registration star in X and Y = " (averx) (avery)
  asknum (brave_man) "HIT RETURN to continue \0\ : "
  print "Calculating SHIFTS in X and Y ..."
  shftx_1 = averx - posx_1
  shfty_1 = avery - posy_1
  shftx_2 = averx - posx_2
  shfty_2 = avery - posy_2
  shfty_3 = avery - posy_3
  shftx_3 = averx - posx_3
  shfty_4 = avery - posy_4
  shftx_4 = averx - posx_4
  print "Shifts in X and Y are :"
  print "Image 0 degs.    = "  (shftx_1) (shfty_1)
  print "Image 45 degs.   = "  (shftx_3) (shfty_3)
  print "Image 22.5 degs. = "  (shftx_2) (shfty_2)
  print "Image 67.5 degs. = "  (shftx_4) (shfty_4)
  print "Shifting image 0 degs ..."
  obeyw rapi2d SHIFT (polly_1) (out_1) "Rapi2d - Shift" 'A' (shftx_1) (shfty_1)
  print "Shifting image 45 degs ..."
  obeyw rapi2d SHIFT (polly_3) (out_3) "Rapi2d - Shift" 'A' (shftx_3) (shfty_3)
  print "Shifting image 22.5  degs ..."
  obeyw rapi2d SHIFT (polly_2) (out_2) "Rapi2d - Shift" 'A' (shftx_2) (shfty_2)
  print "Shifting image 67.5  degs ..."
  obeyw rapi2d SHIFT (polly_4) (out_4) "Rapi2d - Shift" 'A' (shftx_4) (shfty_4)
  print "Re-calculating centroid of registration star in 4 images"
  obeyw rapi2d CENTROID (out_1) (averx) (avery) \
  getpar glob centroid_xfinal (posx_1)
  getpar glob centroid_yfinal (posy_1)
  obeyw rapi2d CENTROID (out_3) (averx) (avery) \
  getpar glob centroid_xfinal (posx_3)
  getpar glob centroid_yfinal (posy_3)
  obeyw rapi2d CENTROID (out_2) (averx) (avery) \
  getpar glob centroid_xfinal (posx_2)
  getpar glob centroid_yfinal (posy_2)
  obeyw rapi2d CENTROID (out_4) (averx) (avery) \
  getpar glob centroid_xfinal (posx_4)
  getpar glob centroid_yfinal (posy_4)
  print "New X,Y centroid values of registration star : "
  print "Image " (out_1) " X,Y = " (posx_1) (posy_1)
  print "Image " (out_3) " X,Y = " (posx_3) (posy_3)
  print "Image " (out_2) " X,Y = " (posx_2) (posy_2)
  print "Image " (out_4) " X,Y = " (posx_4) (posy_4)
  print "Residuals after shift :"
  value1 = averx - posx_1
  value2 = avery - posy_1
  print "Image " (out_1) " dX,dY = " (value1) (value2)
  value1 = averx - posx_3
  value2 = avery - posy_3
  print "Image " (out_3) " dX,dY = " (value1) (value2)
  value1 = averx - posx_2
  value2 = avery - posy_2
  print "Image " (out_2) " dX,dY = " (value1) (value2)
  value1 = averx - posx_4
  value2 = avery - posy_4
  print "Image " (out_4) " dX,dY = " (value1) (value2)
  asknum (brave_man) "HIT RETURN to continue \0\ : "
  print "Calculating minimum and maximum shifts in X and Y for edge trimming"
  shftx_max = max( shftx_1, shftx_2, shftx_3, shftx_4)
  shfty_max = max( shfty_1, shfty_2, shfty_3, shfty_4)
  shftx_min = min( shftx_1, shftx_2, shftx_3, shftx_4)
  shfty_min = min( shfty_1, shfty_2, shfty_3, shfty_4)
  print "Maximum, minimum X shifts = " (shftx_max) (shftx_min)
  print "Maximum, minimum Y shifts = " (shfty_max) (shfty_min)
  if shftx_max > 0.0
    xstrim = 1 + int( abs( shftx_max) + 1)
  else
    xstrim = 1
  end if
  if shfty_max > 0.0
    ystrim = 1 + int( abs( shfty_max) + 1)
  else
    ystrim = 1
  end if
  if shftx_min < 0.0
    xetrim = ( xsizim - xstrim + 1) - int( abs( shftx_min) + 1)
  else
    xetrim = xsizim
  end if
  if shfty_min < 0.0
    yetrim = ( ysizim - ystrim + 1) - int( abs( shfty_min) + 1)
  else
    yetrim = ysizim
  end if
  asknum (brave_man) "HIT RETURN to continue \0\ : "
  print "Trimming output images starting ... "
  print "X,Y start pixel = " (xstrim) (ystrim)
  print "X,Y end   pixel = " (xstrim+xetrim-1) (ystrim+yetrim+1)
  print "Output image size = " (xetrim) (yetrim)
  obeyw rapi2d cmult (out_1) 1.0 junk1
  obeyw rapi2d cmult (out_2) 1.0 junk2
  obeyw rapi2d cmult (out_3) 1.0 junk3
  obeyw rapi2d cmult (out_4) 1.0 junk4
  obeyw rapi2d PICKIM junk1 (xstrim) (ystrim) (xetrim) (yetrim) (out_1) \
  obeyw rapi2d PICKIM junk3 (xstrim) (ystrim) (xetrim) (yetrim) (out_3) \
  obeyw rapi2d PICKIM junk2 (xstrim) (ystrim) (xetrim) (yetrim) (out_2) \
  obeyw rapi2d PICKIM junk4 (xstrim) (ystrim) (xetrim) (yetrim) (out_4) \
  delfile junk1.sdf
  delfile junk2.sdf
  delfile junk3.sdf
  delfile junk4.sdf
end proc
