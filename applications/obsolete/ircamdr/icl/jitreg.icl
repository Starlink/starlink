{ PROCEDURE JITREG : registers and mosaics N jittered images
proc jitreg peak
  yn = undefined(peak)
  print "Enter image names PREFIX ? "
  askname (pref) "Prefix   \im_\ ? "
  print "Enter start number ? "
  asknum (imst) "Start          \1\ ? "
  print "Enter number of images in sequence ? "
  asknum (numin) "Number Images \9\ ? "
  imen = integer(imst+numin-1)
  print "Enter image names SUFFIX (NONE = no suffix) ? "
  askname (suff) "Suffix   \NONE\ ? "
  suff2 = upcase(suff)
  if suff2 = "NONE"
    suff = ""
  end if
  print "Give name of telescope offset file used : "
  askname (toff) "Offset File \stred.off\ ? "
  fexist = file_exists(toff)
  if fexist
    fclose_c
    open cfile (toff)
    print "File " (toff) " found and opened"
  else
    print "File " (toff) " not found - quiting"
    return
  end if
  get plt2d platscal (platscal)
  if platscal = 1.0
    print "Give pixel scale (arcsec/pixel) : "
    asknum (platscal) "Pixel Scale \0.286\ ? "
    send plt2d set platscal (platscal)
    send plt2d set arcsec_pixel (platscal)
  end if
  get plt2d cursor_workstn (curyesno)
  fclose_b
  delfile jitreg.dat
  create bfile "jitreg.dat"
  if curyesno = 1
    obeyw plt2d line_width 2
  end if
  iflag = 0
  loop for dummy = (imst) to (imen)
    if suff2 = "NONE"
      im = pref & dummy
    else
      im = pref & dummy & suff
    end if
    readr cfile (xoff) (yoff)
    print "Telescope offsets for image " (im) " are " (xoff) (yoff)
    xpix = real(xoff/platscal)
    ypix = real(yoff/platscal)
    if iflag = 0
      xpix0 = xpix
      ypix0 = ypix
      obeyw plt2d clear
    end if
    if curyesno = 1
      get plt2d workxcen (workxcen)
      get plt2d workycen (workycen)
      send plt2d set im_xcen (workxcen)
      send plt2d set im_ycen (workycen)
      dflag = 0
      get plt2d disp_mag (disp_mag)
      if disp_mag = 0
        dflag = 1
        get plt2d image_calmag (disp_mag)
      end if
{      send plt2d set magnification (disp_mag)
      send plt2d set magnification 0
      send plt2d set cursor_image (im)
      obeyw plt2d nsigma (im)
    end if
    if iflag = 0
      iflag = 1
      if curyesno = 1
        print "Select centroiding point with cursor"
        obeyw plt2d cursor
        get plt2d x_cur_pixel (xc)
        get plt2d y_cur_pixel (yc)
        print "Pixel selected = " (xc) (yc)
        if yn
        else
          xst = integer(xc-11.0)
          yst = integer(yc-11.0)
          obeyw rapi2d HISTO (im) (xst) (yst) 20 20 \
          getpar glob histo_max (maxval)
          getpar glob histo_xmax (xc)
          getpar glob histo_ymax (yc)
       end if
      else
        print "Give x,y pixel of source to centroid on : "
        asknum (xv) "X coordinate \1\ ? "
        asknum (yv) "Y coordinate \1\ ? "
        xc = xv
        yc = yv
      end if
      xcur = xc - (xpix0-xpix)
      ycur = yc + (ypix0-ypix)
      if curyesno = 1
        obeyw plt2d circle (xcur) (ycur) 8 \
      end if
      obeyw rapi2d CENTROID (im) (xcur) (ycur) N 21 \
      getpar glob centroid_xfinal (xacc0)
      getpar glob centroid_yfinal (yacc0)
      xsh = 0.0
      ysh = 0.0
      cxsh = xsh:10:2
      cysh = ysh:10:2
      dline = (cxsh)&(cysh)
      write bfile (dline)
    else
      xcur = xc - (xpix0-xpix)
      ycur = yc + (ypix0-ypix)
      if yn
      else
        xst = integer(xcur-11.0)
        yst = integer(ycur-11.0)
        obeyw rapi2d HISTO (im) (xst) (yst) 20 20 \
        getpar glob histo_max (maxval)
        getpar glob histo_xmax (xcur)
        getpar glob histo_ymax (ycur)
     end if
{      xcur = xc + xpix
{      ycur = yc - ypix
      if curyesno = 1
        obeyw plt2d circle (xcur) (ycur) 8 \
      end if
      obeyw rapi2d CENTROID (im) (xcur) (ycur) 21 \
      getpar glob centroid_xfinal (xacc)
      getpar glob centroid_yfinal (yacc)
      xsh = (xacc-xacc0)*platscal
      ysh = -1.0*(yacc-yacc0)*platscal
      cxsh = xsh:10:2
      cysh = ysh:10:2
      dline = (cxsh)&(cysh)
      write bfile (dline)
    end if
  end loop
  fclose_b
  fclose_c
  if dflag = 1
    send plt2d set disp_mag 0
    send plt2d set magnification 0
  end if
  print " "
  print "Output file with accurate offsets called jitreg.dat"
  print " "
end proc
