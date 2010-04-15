{ PROCEDURE SHIFT2 : moves 2 images to average position
proc shift2 cur
  yn = undefined(cur)
  if yn
    cur2 = 0
  else
    print "** Registration star selection using cursor requested **"
    cur2 = 1
  end if
  print "Give the NAMES of the 2 images to be shifted to average position"
  askname (polly_1) "Image 1 \image1\ ? "
  askname (polly_2) "Image 2 \image2\ ? "
  print "Give the NAMES for the IMAGES after SHIFTING"
  askname (out_1) "Output image 1 \a\ ? "
  askname (out_2) "Output image 2 \b\ ? "
  if cur2 = 0
    print "Give the POSITIONS of the SAME FEATURE in both images"
    asknum (posx_1) "X-Position in image 1 ? "
    asknum (posy_1) "Y-Position in image 1 ? "
    asknum (posx_2) "X-Position in image 2 ? "
    asknum (posy_2) "Y-Position in image 2 ? "
  else
    obeyw plt2d clear
    send plt2d set cursor_image (polly_1)
    obeyw plt2d nsigma (polly_1)
    print "Select registration STAR in image " (polly_1) " with cursor ..."
    obeyw plt2d cursor
    get plt2d x_cur_pixel (xpix1)
    get plt2d y_cur_pixel (ypix1)
    xpix1 = integer(xpix1)
    ypix1 = integer(ypix1)
    obeyw rapi2d CENTROID (polly_1) (xpix1) (ypix1) N 9 1 Y 5 0.02 \
    getpar glob centroid_xfinal (xpix1)
    getpar glob centroid_yfinal (ypix1)
    posx_1 = xpix1
    posy_1 = ypix1
    send plt2d set cursor_image (polly_2)
    obeyw plt2d nsigma (polly_2)
    print "Select registration STAR in image " (polly_2) " with cursor ..."
    obeyw plt2d cursor
    get plt2d x_cur_pixel (xpix2)
    get plt2d y_cur_pixel (ypix2)
    xpix2 = integer(xpix2)
    ypix2 = integer(ypix2)
    obeyw rapi2d CENTROID (polly_2) (xpix2) (ypix2) N 9 1 Y 5 0.02 \
    getpar glob centroid_xfinal (xpix2)
    getpar glob centroid_yfinal (ypix2)
    posx_2 = xpix2
    posy_2 = ypix2
  end if
  print "Calculating AVERAGE position in X and Y ..."
  avx = (real(posx_1)+real(posx_2))/2.0
  avy = (real(posy_1)+real(posy_2))/2.0
  print "Average position of FEATURE in X and Y = " (avx) (avy)
  print "Calculating SHIFTS in X and Y ..."
  shftx_1 = real(avx-posx_1)
  shfty_1 = real(avy-posy_1)
  shftx_2 = real(avx-posx_2)
  shfty_2 = real(avy-posy_2)
  print "Shifts in X and Y are :"
  print "  Image 1 = "  (shftx_1) (shfty_1)
  print "  Image 2 = "  (shftx_2) (shfty_2)
  print "Shifting image 1 ..."
  obeyw rapi2d SHIFT (polly_1) (out_1) 'Rapi2d - Shift' ~
    'A' (shftx_1) (shfty_1)
  print "Shifting image 2 ..."
  obeyw rapi2d SHIFT (polly_2) (out_2) 'Rapi2d - Shift' ~
    'A' (shftx_2) (shfty_2)
  if cur2 = 1
    obeyw rapi2d CENTROID (out_1) (avx) (avy) N 9 1 Y 5 0.02 \
    getpar glob centroid_xfinal (xpix1)
    getpar glob centroid_yfinal (ypix1)
    posx_1 = xpix1
    posy_1 = ypix1
    obeyw rapi2d CENTROID (out_2) (avx) (avy) N 9 1 Y 5 0.02 \
    getpar glob centroid_xfinal (xpix2)
    getpar glob centroid_yfinal (ypix2)
    posx_2 = xpix2
    posy_2 = ypix2
    resx = real(avx*2.0-posx_1-posx_2)
    resy = real(avy*2.0-posy_1-posy_2)
    print "Sum of residuals after shifting = " (resx) (resy)
  end if
end proc

