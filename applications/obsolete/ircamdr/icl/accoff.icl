{ PROCEDURE ACCOFF : displays 2 consecutive images and works out offsets
proc accoff
  print "Enter a PREFIX for the image(s) : "
  askname (init) "Prefix    \im_\ ? "
  print "Define Sequence of numbered images or User specified numbers ? "
  askchoice (numbwhat) "Sequence or User specified (S,U) \S\ ? "
  if numbwhat = 1
    print "Enter start observation number ? "
    asknum (value2) "Start           \1\ ? "
    print "Enter number of images to be considered ? "
    asknum (numin) "Number of Images \5\ ? "
    value3 = value2+numin-1
    print "End image is therefore number " (value3)
  else
    value2 = 1
    print "Give number of images to be processed ? "
    asknum (value3) "Number of Images \1\ ? "
  end if
  print "Give a SUFFIX for the image(s) (NONE = no suffix) : "
  askname (suff) "Suffix        \NONE\ ? "
  suff2 = upcase(suff)
  if suff2 = "NONE"
    suff = ""
    print "No suffix specified"
  end if
  get plt2d platscal (platscal)
  if platscal = 1.0
    print "Give pixel scale (arcsaec/pixel) : "
    asknum (platscal) "Pixel Scale \0.286\ ? "
    send plt2d set platscal (platscal)
    send plt2d set arcsec_pixel (platscal)
  end if
  print "Give search box size for pair matching : "
  asknum (sbox) "Box Size \15\ ? "
  boxbl = sbox
  boxbl = integer(boxbl)
  print "Images plotted side-by-side (S) or one-below-other (O) : "
  askchoice (locn) "S or O (S,O) \S\ ? "
  get plt2d workxcen (workxcen)
  get plt2d workycen (workycen)
  get plt2d disp_mag (disp_mag)
  if disp_mag = 0
    mag0 = 1
    get plt2d image_calmag (disp_mag)
    print "Display mag was 0, using calculated value of " (disp_mag)
  else
    mag0 = 0
  end if
  maggy = real(disp_mag/2.0)
  print "Using DISP_MAG/2 magnification of " (maggy)
  send plt2d set magnification (maggy)
  value3 = value3-1
  delfile accoff.dat
  fclose_a
  create afile "accoff.dat"
  doff1 = 0
  doff2 = 0
  coff1 = doff1:10:2
  coff2 = doff2:10:2
  dline = (coff1) & (coff2)
  write afile (dline)
  print (dline)
  loop for dummy = (value2) to (value3)
    if numbwhat = 1
      dummy2 = dummy+1
    else
      print "Give observation number 1 ? "
      asknum (dummy) "Obs Number 1 \1\ ? "
      print "Give observation number 2 ? "
      asknum (dummy2) "Obs Number 2 \2\ ? "
    end if
    st1 = init & dummy2 & suff
    st2 = init & dummy & suff
    if locn = 1
      possx = workxcen*0.5
      possy = workycen
    else
      possx = workxcen
      possy = workycen*1.5
    end if
    send plt2d set im_xcen (possx)
    send plt2d set im_ycen (possy)
    obeyw plt2d clear
    print "Plotting 1st image, name = " (st1)
    obeyw plt2d nsigma (st1)
    get plt2d im_xst (value5)
    get plt2d im_xen (value6)
    get plt2d im_yst (value7)
    get plt2d im_yen (value8)
    obeyw plt2d comment (st1) (possx) (possy) 12
    if locn = 1
      possx = workxcen*1.5
      possy = workycen
    else
      possx = workxcen
      possy = workycen*0.5
    end if
    send plt2d set im_xcen (possx)
    send plt2d set im_ycen (possy)
    print "Plotting 2nd image, name = " (st2)
    obeyw plt2d nsigma (st2)
    obeyw plt2d comment (st2) (possx) (possy) 12
    if locn = 1
      print "Select centre of common box in RIGHT image, name = " (st2)
    else
      print "Select centre of common box in BOTTOM image, name = " (st2)
    end if
    send plt2d set cursor_image (st2)
    obeyw plt2d cursor
    get plt2d x_cur_pixel (x1)
    get plt2d y_cur_pixel (y1)
    print "X,Y position = " (x1) "," (y1)
    send plt2d set platscal 1
    send plt2d set arcsec_pixel 1
    obeyw plt2d box (x1) (y1) (sbox) (sbox) 'CENTRE'
    send plt2d set platscal (platscal)
    send plt2d set arcsec_pixel (platscal)
    if locn = 1
      print "Select centre of common box in LEFT  image, name = " (st1)
    else
      print "Select centre of common box in TOP   image, name = " (st1)
    end if
    send plt2d set im_xst (value5)
    send plt2d set im_xen (value6)
    send plt2d set im_yst (value7)
    send plt2d set im_yen (value8)
print (value5) (value6) (value7) (value8) (st1)
    send plt2d set cursor_image (st1)
    obeyw plt2d cursor
    get plt2d x_cur_pixel (x2)
    get plt2d y_cur_pixel (y2)
    print "X,Y position = " (x2) "," (y2)
    send plt2d set platscal 1
    send plt2d set arcsec_pixel 1
    obeyw plt2d box (x2) (y2) (sbox) (sbox) 'CENTRE'
    send plt2d set platscal (platscal)
    send plt2d set arcsec_pixel (platscal)
    xs = x1-boxbl
    ys = y1-boxbl
    xs = integer(xs)
    ys = integer(ys)
    obeyw rapi2d HISTO (st2) (xs) (ys) (sbox) (sbox) \
    getpar glob histo_max (max1)
    getpar glob histo_xmax (xmax1)
    getpar glob histo_ymax (ymax1)
    xs = x2-boxbl
    ys = y2-boxbl
    xs = integer(xs)
    ys = integer(ys)
    obeyw rapi2d HISTO (st1) (xs) (ys) (sbox) (sbox) \
    getpar glob histo_max (max2)
    getpar glob histo_xmax (xmax2)
    getpar glob histo_ymax (ymax2)
    off1 = xmax2-xmax1
    off2 = ymax2-ymax1
    off1 = integer(off1)
    off2 = integer(off2)
    print "Offsets = " (off1) "," (off2)
    print "Max in " (st1) " = " (max1)
    print "Max in " (st2) " = " (max2)
    print "Mosaicing " (st1) " and " (st2)
    print "Offset of 2nd image from 1st = " (off1) (off2)
    obeyw obsrap MOSAIC2 (st1) (st2) (off1) (off2) junk ~
      Y Y V -1.0e-20 \
    possy = workycen*1.5
    send plt2d set im_xcen (workxcen)
    send plt2d set im_ycen (workycen)
    print "Plotting mosaiced image ..."
    name_image = "junk"
    send plt2d set name_image (name_image)
    obeyw plt2d clear
    obeyw plt2d nsigma junk
    obeyw plt2d comment 'Mosaiced Image' (workxcen) (possy) 20
    off1 = -1.0*off1*platscal
    off2 = off2*platscal
    off1 = doff1-off1
    off2 = doff2-off2
    coff1 = off1:10:2
    coff2 = off2:10:2
    dline = (coff1) & (coff2)
    write afile (dline)
    doff1 = off1
    doff2 = off2
    delfile junk.sdf
    if dummy < value3
      asklog (yesno) "Hit RETURN for next pair \Y\ : "
    end if
  end loop
  fclose_a
  if mag0 = 1
   send plt2d set magnification 0
  end if
  print " "
  print "Accurate offsets saved in disk file accoff.dat"
  print " "
end proc
