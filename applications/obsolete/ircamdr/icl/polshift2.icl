{ PROCEDURE POLSHIFT2 : moves 8 polarization images to average position
proc polshift2 cur
  yn = undefined(cur)
  if yn
    print "** Registration star selection using cursor requested **"
    cur2 = 1
  else
    cur2 = 0
  end if
  print "Give the NAMES of the 8 images to be shifted to average position"
  askname (polly_1o) "o-ray image at    0 deg.  \pol0a\ ? "
  askname (polly_1e) "e-ray image at    0 deg.  \pol0b\ ? "
  askname (polly_2o) "o-ray image at   45 deg. \pol45a\ ? "
  askname (polly_2e) "e-ray image at   45 deg. \pol45b\ ? "
  askname (polly_3o) "o-ray image at 22.5 deg. \pol22a\ ? "
  askname (polly_3e) "e-ray image at 22.5 deg. \pol22b\ ? "
  askname (polly_4o) "o-ray image at 67.5 deg. \pol67a\ ? "
  askname (polly_4e) "e-ray image at 67.5 deg. \pol67b\ ? "
  out_1o = polly_1o&"r"
  out_1e = polly_1e&"r"
  out_2o = polly_2o&"r"
  out_2e = polly_2e&"r"
  out_3o = polly_3o&"r"
  out_3e = polly_3e&"r"
  out_4o = polly_4o&"r"
  out_4e = polly_4e&"r"
  if cur2 = 0
    print "Give the pixel positions of the SAME FEATURE in all images"
    asknum (posx_1o) "X-Position in o-ray image at    0 deg. ? "
    asknum (posy_1o) "Y-Position in o-ray image at    0 deg. ? "
    asknum (posx_1e) "X-Position in e-ray image at    0 deg. ? "
    asknum (posy_1e) "Y-Position in e-ray image at    0 deg. ? "
    asknum (posx_2o) "X-Position in o-ray image at   45 deg. ? "
    asknum (posy_2o) "Y-Position in o-ray image at   45 deg. ? "
    asknum (posx_2e) "X-Position in e-ray image at   45 deg. ? "
    asknum (posy_2e) "Y-Position in e-ray image at   45 deg. ? "
    asknum (posx_3o) "X-Position in o-ray image at 22.5 deg. ? "
    asknum (posy_3o) "Y-Position in o-ray image at 22.5 deg. ? "
    asknum (posx_3e) "X-Position in e-ray image at 22.5 deg. ? "
    asknum (posy_3e) "Y-Position in e-ray image at 22.5 deg. ? "
    asknum (posx_4o) "X-Position in o-ray image at 67.5 deg. ? "
    asknum (posy_4o) "Y-Position in o-ray image at 67.5 deg. ? "
    asknum (posx_4e) "X-Position in e-ray image at 67.5 deg. ? "
    asknum (posy_4e) "Y-Position in e-ray image at 67.5 deg. ? "
  else
{ 0 degrees o- e- images
    obeyw plt2d clear
    send plt2d set cursor_image (polly_1o)
    obeyw plt2d nsigma (polly_1o)
    print "Select registration STAR in image " (polly_1o) " with cursor ..."
    obeyw plt2d cursor
    get plt2d x_cur_pixel (xpix1)
    get plt2d y_cur_pixel (ypix1)
    xpix1 = integer(xpix1)
    ypix1 = integer(ypix1)
    obeyw rapi2d CENTROID (polly_1o) (xpix1) (ypix1) N 13 1 Y 5 0.02 \
    getpar glob centroid_xfinal (xpix1)
    getpar glob centroid_yfinal (ypix1)
    posx_1o = xpix1
    posy_1o = ypix1
    send plt2d set cursor_image (polly_1e)
    obeyw plt2d nsigma (polly_1e)
    print "Select registration STAR in image " (polly_1e) " with cursor ..."
    obeyw plt2d cursor
    get plt2d x_cur_pixel (xpix2)
    get plt2d y_cur_pixel (ypix2)
    xpix2 = integer(xpix2)
    ypix2 = integer(ypix2)
    obeyw rapi2d CENTROID (polly_1e) (xpix2) (ypix2) N 13 1 Y 5 0.02 \
    getpar glob centroid_xfinal (xpix2)
    getpar glob centroid_yfinal (ypix2)
    posx_1e = xpix2
    posy_1e = ypix2
{ 45 degrees o- e- images
    obeyw plt2d clear
    send plt2d set cursor_image (polly_2o)
    obeyw plt2d nsigma (polly_2o)
    print "Select registration STAR in image " (polly_2o) " with cursor ..."
    obeyw plt2d cursor
    get plt2d x_cur_pixel (xpix1)
    get plt2d y_cur_pixel (ypix1)
    xpix1 = integer(xpix1)
    ypix1 = integer(ypix1)
    obeyw rapi2d CENTROID (polly_2o) (xpix1) (ypix1) N 13 1 Y 5 0.02 \
    getpar glob centroid_xfinal (xpix1)
    getpar glob centroid_yfinal (ypix1)
    posx_2o = xpix1
    posy_2o = ypix1
    send plt2d set cursor_image (polly_2e)
    obeyw plt2d nsigma (polly_2e)
    print "Select registration STAR in image " (polly_2e) " with cursor ..."
    obeyw plt2d cursor
    get plt2d x_cur_pixel (xpix2)
    get plt2d y_cur_pixel (ypix2)
    xpix2 = integer(xpix2)
    ypix2 = integer(ypix2)
    obeyw rapi2d CENTROID (polly_2e) (xpix2) (ypix2) N 13 1 Y 5 0.02 \
    getpar glob centroid_xfinal (xpix2)
    getpar glob centroid_yfinal (ypix2)
    posx_2e = xpix2
    posy_2e = ypix2
{ 22.5 degrees o- e- images
    obeyw plt2d clear
    send plt2d set cursor_image (polly_3o)
    obeyw plt2d nsigma (polly_3o)
    print "Select registration STAR in image " (polly_3o) " with cursor ..."
    obeyw plt2d cursor
    get plt2d x_cur_pixel (xpix1)
    get plt2d y_cur_pixel (ypix1)
    xpix1 = integer(xpix1)
    ypix1 = integer(ypix1)
    obeyw rapi2d CENTROID (polly_3o) (xpix1) (ypix1) N 13 1 Y 5 0.02 \
    getpar glob centroid_xfinal (xpix1)
    getpar glob centroid_yfinal (ypix1)
    posx_3o = xpix1
    posy_3o = ypix1
    send plt2d set cursor_image (polly_3e)
    obeyw plt2d nsigma (polly_3e)
    print "Select registration STAR in image " (polly_3e) " with cursor ..."
    obeyw plt2d cursor
    get plt2d x_cur_pixel (xpix2)
    get plt2d y_cur_pixel (ypix2)
    xpix2 = integer(xpix2)
    ypix2 = integer(ypix2)
    obeyw rapi2d CENTROID (polly_3e) (xpix2) (ypix2) N 13 1 Y 5 0.02 \
    getpar glob centroid_xfinal (xpix2)
    getpar glob centroid_yfinal (ypix2)
    posx_3e = xpix2
    posy_3e = ypix2
{ 67.5 degrees o- e- images
    obeyw plt2d clear
    send plt2d set cursor_image (polly_4o)
    obeyw plt2d nsigma (polly_4o)
    print "Select registration STAR in image " (polly_4o) " with cursor ..."
    obeyw plt2d cursor
    get plt2d x_cur_pixel (xpix1)
    get plt2d y_cur_pixel (ypix1)
    xpix1 = integer(xpix1)
    ypix1 = integer(ypix1)
    obeyw rapi2d CENTROID (polly_4o) (xpix1) (ypix1) N 13 1 Y 5 0.02 \
    getpar glob centroid_xfinal (xpix1)
    getpar glob centroid_yfinal (ypix1)
    posx_4o = xpix1
    posy_4o = ypix1
    send plt2d set cursor_image (polly_4e)
    obeyw plt2d nsigma (polly_4e)
    print "Select registration STAR in image " (polly_4e) " with cursor ..."
    obeyw plt2d cursor
    get plt2d x_cur_pixel (xpix2)
    get plt2d y_cur_pixel (ypix2)
    xpix2 = integer(xpix2)
    ypix2 = integer(ypix2)
    obeyw rapi2d CENTROID (polly_4e) (xpix2) (ypix2) N 13 1 Y 5 0.02 \
    getpar glob centroid_xfinal (xpix2)
    getpar glob centroid_yfinal (ypix2)
    posx_4e = xpix2
    posy_4e = ypix2
  end if
  print "Calculating average position in X and Y ..."
  avx = (real(posx_1o)+real(posx_1e)+~
         real(posx_2o)+real(posx_2e)+~
         real(posx_3o)+real(posx_3e)+~
         real(posx_4o)+real(posx_4e))/8.0
  avy = (real(posy_1o)+real(posy_1e)+~
         real(posy_2o)+real(posy_2e)+~
         real(posy_3o)+real(posy_3e)+~
         real(posy_4o)+real(posy_4e))/8.0
  print "Average position of feature in X and Y = " (avx) (avy)
  print "Calculating shifts in X and Y ..."
  shftx_1o = real(avx-posx_1o)
  shfty_1o = real(avy-posy_1o)
  shftx_1e = real(avx-posx_1e)
  shfty_1e = real(avy-posy_1e)
  shftx_2o = real(avx-posx_2o)
  shfty_2o = real(avy-posy_2o)
  shftx_2e = real(avx-posx_2e)
  shfty_2e = real(avy-posy_2e)
  shftx_3o = real(avx-posx_3o)
  shfty_3o = real(avy-posy_3o)
  shftx_3e = real(avx-posx_3e)
  shfty_3e = real(avy-posy_3e)
  shftx_4o = real(avx-posx_4o)
  shfty_4o = real(avy-posy_4o)
  shftx_4e = real(avx-posx_4e)
  shfty_4e = real(avy-posy_4e)
  print "Shifts in X and Y are :"
  print "  o-ray image    0 deg. = "  (shftx_1o) (shfty_1o)
  print "  e-ray Image    0 deg. = "  (shftx_1e) (shfty_1e)
  print "  o-ray image   45 deg. = "  (shftx_2o) (shfty_2o)
  print "  e-ray Image   45 deg. = "  (shftx_2e) (shfty_2e)
  print "  o-ray image 22.5 deg. = "  (shftx_3o) (shfty_3o)
  print "  e-ray Image 22.5 deg. = "  (shftx_3e) (shfty_3e)
  print "  o-ray image 67.5 deg. = "  (shftx_4o) (shfty_4o)
  print "  e-ray Image 67.5 deg. = "  (shftx_4e) (shfty_4e)
  print "Shifting o-ray image 0 deg. ..."
  obeyw rapi2d SHIFT (polly_1o) (out_1o) 'Rapi2d - Shift' ~
    'A' (shftx_1o) (shfty_1o)
  print "Shifting e-ray image 0 deg. ..."
  obeyw rapi2d SHIFT (polly_1e) (out_1e) 'Rapi2d - Shift' ~
    'A' (shftx_1e) (shfty_1e)
  print "Shifting o-ray image 45 deg. ..."
  obeyw rapi2d SHIFT (polly_2o) (out_2o) 'Rapi2d - Shift' ~
    'A' (shftx_2o) (shfty_2o)
  print "Shifting e-ray image 45 deg. ..."
  obeyw rapi2d SHIFT (polly_2e) (out_2e) 'Rapi2d - Shift' ~
    'A' (shftx_2e) (shfty_2e)
  print "Shifting o-ray image 22.5 deg. ..."
  obeyw rapi2d SHIFT (polly_3o) (out_3o) 'Rapi2d - Shift' ~
    'A' (shftx_3o) (shfty_3o)
  print "Shifting e-ray image 22.5 deg. ..."
  obeyw rapi2d SHIFT (polly_3e) (out_3e) 'Rapi2d - Shift' ~
    'A' (shftx_3e) (shfty_3e)
  print "Shifting o-ray image 67.5 deg. ..."
  obeyw rapi2d SHIFT (polly_4o) (out_4o) 'Rapi2d - Shift' ~
    'A' (shftx_4o) (shfty_4o)
  print "Shifting e-ray image 67.5 deg. ..."
  obeyw rapi2d SHIFT (polly_4e) (out_4e) 'Rapi2d - Shift' ~
    'A' (shftx_4e) (shfty_4e)
  if cur2 = 1
    obeyw rapi2d CENTROID (out_1o) (avx) (avy) N 13 1 Y 5 0.02 \
    getpar glob centroid_xfinal (xpix1)
    getpar glob centroid_yfinal (ypix1)
    posx_1o = xpix1
    posy_1o = ypix1
    obeyw rapi2d CENTROID (out_1e) (avx) (avy) N 13 1 Y 5 0.02 \
    getpar glob centroid_xfinal (xpix1)
    getpar glob centroid_yfinal (ypix1)
    posx_1e = xpix1
    posy_1e = ypix1
    obeyw rapi2d CENTROID (out_2o) (avx) (avy) N 13 1 Y 5 0.02 \
    getpar glob centroid_xfinal (xpix1)
    getpar glob centroid_yfinal (ypix1)
    posx_2o = xpix1
    posy_2o = ypix1
    obeyw rapi2d CENTROID (out_2e) (avx) (avy) N 13 1 Y 5 0.02 \
    getpar glob centroid_xfinal (xpix1)
    getpar glob centroid_yfinal (ypix1)
    posx_2e = xpix1
    posy_2e = ypix1
    obeyw rapi2d CENTROID (out_3o) (avx) (avy) N 13 1 Y 5 0.02 \
    getpar glob centroid_xfinal (xpix1)
    getpar glob centroid_yfinal (ypix1)
    posx_3o = xpix1
    posy_3o = ypix1
    obeyw rapi2d CENTROID (out_3e) (avx) (avy) N 13 1 Y 5 0.02 \
    getpar glob centroid_xfinal (xpix1)
    getpar glob centroid_yfinal (ypix1)
    posx_3e = xpix1
    posy_3e = ypix1
    obeyw rapi2d CENTROID (out_4o) (avx) (avy) N 13 1 Y 5 0.02 \
    getpar glob centroid_xfinal (xpix1)
    getpar glob centroid_yfinal (ypix1)
    posx_4o = xpix1
    posy_4o = ypix1
    obeyw rapi2d CENTROID (out_4e) (avx) (avy) N 13 1 Y 5 0.02 \
    getpar glob centroid_xfinal (xpix1)
    getpar glob centroid_yfinal (ypix1)
    posx_4e = xpix1
    posy_4e = ypix1
    resx0o = real(avx-posx_1o)
    resy0o = real(avy-posy_1o)
    resx0e = real(avx-posx_1e)
    resy0e = real(avy-posy_1e)
    resx45o = real(avx-posx_2o)
    resy45o = real(avy-posy_2o)
    resx45e = real(avx-posx_2e)
    resy45e = real(avy-posy_2e)
    resx22o = real(avx-posx_3o)
    resy22o = real(avy-posy_3o)
    resx22e = real(avx-posx_3e)
    resy22e = real(avy-posy_3e)
    resx67o = real(avx-posx_4o)
    resy67o = real(avy-posy_4o)
    resx67e = real(avx-posx_4e)
    resy67e = real(avy-posy_4e)
    print "Residuals after shifting ..."
    print "o-ray    0 deg. = " (resx0o) (resy0o)
    print "e-ray    0 deg. = " (resy0e) (resy0e)
    print "o-ray   45 deg. = " (resx45o) (resy45o)
    print "e-ray   45 deg. = " (resx45e) (resy45e)
    print "o-ray 22.5 deg. = " (resx22o) (resy22o)
    print "e-ray 22.5 deg. = " (resx22e) (resy22e)
    print "o-ray 67.5 deg. = " (resx67o) (resy67o)
    print "e-ray 67.5 deg. = " (resx67e) (resy67e)
  end if
end proc

