{ PROCEDURE COFF : calculated accurate offsets and creates offset file
proc coff
  print "Enter image names PREFIX ? "
  askname (pref) "Prefix   \im_\ ? "
  print "Enter start number ? "
  asknum (imst) "Start          \1\ ? "
  print "Enter number of images in sequence ? "
  asknum (numin) "Number Images \9\ ? "
  print "Enter increment between OBJECT images : "
  asknum (obinc) "Increment   \1\ ? "
  imen = integer(imst+numin-1)
  if obinc > 1
    numin2 = ifix(numin/obinc+0.5)
  else
    numin2 = numin
  end if
  print "Enter image names SUFFIX (NONE = no suffix) ? "
  askname (suff) "Suffix   \NONE\ ? "
  suff2 = upcase(suff)
  if suff2 = "NONE"
    suff = ""
  end if
  print "Use Centroid of feature at cursor point or Peak pixel ? "
  askchoice (usewhat) "Centroid or Peak pixel (C,P) \C\ ? "
  print "Give name for offset file created : "
  askname (toff) "Offset File \coff.dat\ ? "
  fclose_c
  delfile (toff)
  create cfile (toff)
  print "File " (toff) " created and opened"
  get plt2d platscal (platscal)
  if platscal = 1.0
    print "Give pixel scale (arcsec/pixel) : "
    asknum (platscal) "Pixel Scale \0.286\ ? "
    send plt2d set platscal (platscal)
  end if
  obeyw plt2d line_width 2
  get plt2d workxcen (workxcen)
  get plt2d workycen (workycen)
  send plt2d set im_xcen (workxcen)
  send plt2d set im_ycen (workycen)
  get plt2d disp_mag (disp_mag)
  iflag = 0
  loop for dummy = (imst) to (imen) step (obinc)
    im = pref & dummy & suff
    if iflag = 0
      obeyw plt2d clear
    end if
    print "Plotting image " (im)
    send plt2d set magnification (disp_mag)
    send plt2d set cursor_image (im)
    obeyw plt2d nsigma (im)
    if iflag = 0
      iflag = 1
      print "Select reference point (star) with cursor"
      obeyw plt2d cursor
      get plt2d x_cur_pixel (xcur)
      get plt2d y_cur_pixel (ycur)
      print "Pixel selected = " (xcur) (ycur)
      if usewhat = 1
        cd = 15*platscal
        obeyw plt2d circle (xcur) (ycur) (cd) \
        obeyw rapi2d CENTROID (im) (xcur) (ycur) N 15 1 Y 5 0.05 \
        getpar glob centroid_xfinal (xacc0)
        getpar glob centroid_yfinal (yacc0)
      else
        bx = 15.0*platscal
        obeyw plt2d box (xcur) (ycur) (bx) (bx) 'CENTRE' \
        xobj0 = integer(xcur)
        yobj0 = integer(ycur)
        xst = integer(xobj0-7.0)
        yst = integer(yobj0-7.0)
        obeyw rapi2d HISTO (im) (xst) (yst) 15 15 \
        getpar glob histo_max (maxval)
        getpar glob histo_min (minval)
        getpar glob histo_xmax (xacc0)
        getpar glob histo_ymax (yacc0)
        print "Maximum signal of " (maxval) " found at " (xacc0) (yacc0)
      end if
      xsh = 0.0
      ysh = 0.0
      cxsh = xsh:10:2
      cysh = ysh:10:2
      dline = (cxsh)&(cysh)
      write cfile (dline)
    else
      if usewhat = 1
        cd = 15*platscal
        obeyw plt2d circle (xcur) (ycur) (cd) \
        obeyw rapi2d CENTROID (im) (xcur) (ycur) N 15 1 Y 5 0.05 \
        getpar glob centroid_xfinal (xacc)
        getpar glob centroid_yfinal (yacc)
      else
        bx = 15.0*platscal
        obeyw plt2d box (xcur) (ycur) (bx) (bx) 'CENTRE' \
        xobj0 = integer(xcur)
        yobj0 = integer(ycur)
        xst = integer(xobj0-7.0)
        yst = integer(yobj0-7.0)
        obeyw rapi2d HISTO (im) (xst) (yst) 15 15 \
        getpar glob histo_max (maxval)
        getpar glob histo_min (minval)
        getpar glob histo_xmax (xacc)
        getpar glob histo_ymax (yacc)
        print "Maximum signal of " (maxval) " found at " (xacc) (yacc)
      end if
      xsh = (xacc-xacc0)*platscal
      ysh = -1.0*(yacc-yacc0)*platscal
      cxsh = xsh:10:2
      cysh = ysh:10:2
      dline = (cxsh)&(cysh)
      write cfile (dline)
    end if
  end loop
  fclose_c
end proc
