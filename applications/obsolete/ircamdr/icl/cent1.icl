{ PROCEDURE CENT1 : calculated accurate position of source in image
proc cent1 imnam ibox
  yn = undefined(imnam)
  yn2 = undefined(ibox)
  if yn
    get plt2d name_image (last_im)
    print "Last image plotted = " (last_im)
    print "Enter image for centroiding (RETURN=LAST IMAGE) ? "
    askname (im) "Image Name \-1\ ? "
    if im = "-1"
      im = last_im
    end if
  else
    im = imnam
  end if
  if yn2
    print "Give box size for centroid search : "
    asknum (box) "Box Size in pixels \9\ ? "
  else
    box = ibox
  end if
  box = integer(box)
  get plt2d platscal (platscal)
  if platscal = 1.0
    print "Give pixel scale (arcsec/pixel) : "
    asknum (platscal) "Pixel Scale \0.286\ ? "
    send plt2d set platscal (platscal)
  end if
  get plt2d workxcen (workxcen)
  get plt2d workycen (workycen)
  send plt2d set im_xcen (workxcen)
  send plt2d set im_ycen (workycen)
  get plt2d disp_mag (disp_mag)
  obeyw plt2d clear
  print "Plotting image " (im)
  send plt2d set magnification (disp_mag)
  send plt2d set cursor_image (im)
  obeyw plt2d nsigma (im)
  obeyw plt2d line_width 2
  more = 1
  loop while more = 1
    print "Select centroiding point with cursor"
    obeyw plt2d cursor
    get plt2d x_cur_pixel (xcur)
    get plt2d y_cur_pixel (ycur)
    print "Pixel selected = " (xcur) (ycur)
    obeyw rapi2d CENTROID (im) (xcur) (ycur) N (box) 1 Y 5 0.05 \
    getpar glob centroid_xfinal (xacc1)
    getpar glob centroid_yfinal (yacc1)
    obeyw plt2d cross (xacc1) (yacc1) 5
    xacc1 = real(xacc1)
    yacc1 = real(yacc1)
    print " "
    print "R.A., Dec pixel position of feature = " (xacc1) (yacc1)
    print " "
    asklog (yn1) "Another go (Yes or No) \N\ ? "
    if yn1 = 0
      more = 0
    end if
    print " "
  end loop
end proc
