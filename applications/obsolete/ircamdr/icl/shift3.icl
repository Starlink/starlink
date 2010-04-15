{ PROCEDURE SHIFT3 : moves 3 images to average position
proc shift3 cur
  yn = undefined(cur)
  if yn
    cur2 = 0
  else
    print "** Registration star selection using cursor requested **"
    cur2 = 1
  end if
  print "Give the NAMES of the 3 images to be shifted to average position"
  askname (polly_1) "Image 1 \image1\ ? "
  askname (polly_2) "Image 2 \image2\ ? "
  askname (polly_3) "Image 3 \image3\ ? "
  print "Give the NAMES for the IMAGES after SHIFTING"
  askname (out_1) "Output image 1 \a\ ? "
  askname (out_2) "Output image 2 \b\ ? "
  askname (out_3) "Output image 3 \c\ ? "
  if cur2 = 0
    print "Give the POSITIONS of the SAME FEATURE in both images"
    asknum (posx_1) "X-Position in image 1 ? "
    asknum (posy_1) "Y-Position in image 1 ? "
    asknum (posx_2) "X-Position in image 2 ? "
    asknum (posy_2) "Y-Position in image 2 ? "
    asknum (posx_3) "X-Position in image 3 ? "
    asknum (posy_3) "Y-Position in image 3 ? "
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
    send plt2d set cursor_image (polly_3)
    obeyw plt2d nsigma (polly_3)
    print "Select registration STAR in image " (polly_3) " with cursor ..."
    obeyw plt2d cursor
    get plt2d x_cur_pixel (xpix3)
    get plt2d y_cur_pixel (ypix3)
    xpix3 = integer(xpix3)
    ypix3 = integer(ypix3)
    obeyw rapi2d CENTROID (polly_3) (xpix3) (ypix3) N 9 1 Y 5 0.02 \
    getpar glob centroid_xfinal (xpix3)
    getpar glob centroid_yfinal (ypix3)
    posx_3 = xpix3
    posy_3 = ypix3
  end if
  print "Calculating AVERAGE position in X and Y ..."
  avx = (real(posx_1)+real(posx_2)+real(posx_3))/3.0
  avy = (real(posy_1)+real(posy_2)+real(posy_3))/3.0
  print "Average position of FEATURE in X and Y = " (avx) (avy)
  print "Calculating SHIFTS in X and Y ..."
  shftx_1 = real(avx-posx_1)
  shfty_1 = real(avy-posy_1)
  shftx_2 = real(avx-posx_2)
  shfty_2 = real(avy-posy_2)
  shftx_3 = real(avx-posx_3)
  shfty_3 = real(avy-posy_3)
  print "Shifts in X and Y are :"
  print "  Image 1 = "  (shftx_1) (shfty_1)
  print "  Image 2 = "  (shftx_2) (shfty_2)
  print "  Image 3 = "  (shftx_3) (shfty_3)
  print "Shifting image 1 ..."
  obeyw rapi2d SHIFT (polly_1) (out_1) 'Rapi2d - Shift' ~
    'A' (shftx_1) (shfty_1)
  print "Shifting image 2 ..."
  obeyw rapi2d SHIFT (polly_2) (out_2) 'Rapi2d - Shift' ~
    'A' (shftx_2) (shfty_2)
  print "Shifting image 3 ..."
  obeyw rapi2d SHIFT (polly_3) (out_3) 'Rapi2d - Shift' ~
    'A' (shftx_3) (shfty_3)
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
    obeyw rapi2d CENTROID (out_3) (avx) (avy) N 9 1 Y 5 0.02 \
    getpar glob centroid_xfinal (xpix3)
    getpar glob centroid_yfinal (ypix3)
    posx_3 = xpix3
    posy_3 = ypix3
    resx = real(avx*3.0-posx_1-posx_2-posx_3)
    resy = real(avy*3.0-posy_1-posy_2-posy_3)
    print "Sum of residuals after shifting = " (resx) (resy)
  end if
end proc

