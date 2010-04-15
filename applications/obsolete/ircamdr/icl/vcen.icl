proc vcen
  asknum (j1) "Start obs no. ? "
  asknum (j2) "End   obs no. ? "
  asknum (box) "Box Size in pixels ? "
  box = integer(box)
  get plt2d platscal (platscal)
  if platscal = 1.0
    print "Give pixel scale (arcsec/pixel) : "
    asknum (platscal) "Pixel Scale \0.286\ ? "
    send plt2d set platscal (platscal)
  end if
  obeyw plt2d clear
  get plt2d workxcen (workxcen)
  get plt2d workycen (workycen)
  send plt2d set im_xcen (workxcen)
  send plt2d set im_ycen (workycen)
  iflag = 0
  delfile vcen.dat
  fclose_a
  create afile "vcen.dat"
  loop for jj = j1 to j2
    im = "c"&jj
    print "Centroiding images " (im)
    send plt2d set cursor_image (im)
    obeyw plt2d nsigma (im)
    obeyw plt2d line_width 2
    if iflag = 0
      iflag = 1
      print "Select centroiding point with cursor"
      obeyw plt2d cursor
      get plt2d x_cur_pixel (xcur)
      get plt2d y_cur_pixel (ycur)
      print "Pixel selected = " (xcur) (ycur)
      xcur = integer(xcur)
      ycur = integer(ycur)
    end if
    xst = integer(xcur-9.0)
    yst = integer(ycur-9.0)
    obeyw rapi2d HISTO (im) (xst) (yst) 20 20 \
    getpar glob histo_max (maxval)
    getpar glob histo_min (minval)
    getpar glob histo_xmax (xobj)
    getpar glob histo_ymax (yobj)
    print "Maximum signal of " (maxval) " found at " (xobj) (yobj)
    obeyw rapi2d CENTROID (im) (xobj) (yobj) N (box) 1 Y 5 0.05 \
    getpar glob centroid_xfinal (xacc2)
    getpar glob centroid_yfinal (yacc2)
    obeyw plt2d cross (xacc2) (yacc2) 5
    xacc2 = real(xacc2)
    yacc2 = real(yacc2)
    cx = xacc2:10:2
    cy = yacc2:10:2
    dline = cx&"   "&cy
    write afile (dline)
  end loop
  fclose_a
end proc
