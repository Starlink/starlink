{ PROCEDURE ROCENT2 :
proc rocent
  get plt2d image_workstn (image_workstn)
  get plt2d cursor_workstn (cursor_workstn)
  get plt2d name_image (name_image)
  get plt2d worknam (worknam)
  get plt2d filetype (filetype)
  if image_workstn <> 1
    print "You CANNOT plot IMAGES on this workstation - " (worknam)
    return
  end if
  box = 9
  get plt2d platscal (platscal)
  if platscal = 1.0
    print "Give pixel scale (arcsec/pixel) : "
    asknum (platscal) "Pixel Scale \0.286\ ? "
    send plt2d set platscal (platscal)
  end if
  fclose_a
  delfile rocent.im
  create afile "rocent.im"
  fclose_b
  delfile rocent.dat
  create bfile "rocent.dat"
  fclose_c
  delfile rocent.off
  create cfile "rocent.off"
  more = 1
  obeyw plt2d clear
  iflag = 0
  loop while more = 1
    num_obsele2 = -999
    code_image2 = -999
    value2 = -999
    promptfor = 1
    name_out = " "
    get plt2d name_image (last_image)
    get_imagename (num_obsele2) (code_image2) (name_out) (last_image)
    im = name_out
    get plt2d workxcen (workxcen)
    get plt2d workycen (workycen)
    send plt2d set im_xcen (workxcen)
    send plt2d set im_ycen (workycen)
    get plt2d disp_mag (disp_mag)
    get plt2d disp_ns (disp_ns)
    print "Plotting image " (im)
    send plt2d set magnification (disp_mag)
    send plt2d set cursor_image (im)
    obeyw plt2d nsigma (im) (disp_ns)
    print "Select centroiding point with cursor"
    obeyw plt2d cursor
    get plt2d x_cur_pixel (xcur)
    get plt2d y_cur_pixel (ycur)
    print "Pixel selected = " (xcur) (ycur)
    obeyw rapi2d CENTROID (im) (xcur) (ycur) N (box) 1 Y 5 0.02 \
    getpar glob centroid_xfinal (xacc1)
    getpar glob centroid_yfinal (yacc1)
    if iflag = 0
      iflag = 1
      xoff0 = xacc1
      yoff0 = yacc1
    end if
    xoff = (xacc1-xoff0)*platscal
    yoff = (yacc1-yoff0)*platscal
    dline = (im)
    write afile (dline)
    cxoff = xoff:10:2
    cyoff = yoff:10:2
    dline = (cxoff)&(cyoff)
    write cfile (dline)
    xacc1 = real(xacc1)
    yacc1 = real(yacc1)
    cxsh = xacc1:10:2
    cysh = yacc1:10:2
    dline = (im)&"    "&(cxsh)&(cysh)
    write bfile (dline)
    asklog (yn1) "Another go (Yes or No) \Y\ ? "
    if yn1 = 0
      more = 0
    end if
    print " "
    print "File rocent.dat contains centroid information."
    print "File rocent.off contains spatial offsets between images"
    print "File rocent.im  contains list of images entered"
    print " "
  end loop
  fclose_a
  fclose_b
  fclose_c
end proc
