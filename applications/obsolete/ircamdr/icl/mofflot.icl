{ PROCEDURE MOFFLOT : calculates accurate offsets for images using MOFF
proc mofflot
  get plt2d filetype (filetype)
  get plt2d rosuf (rosuf)
  rosuf = upcase(rosuf)
  print "Enter a PREFIX for the image(s) : "
  askname (init) "Prefix         \im_\ ? "
  print "Enter start observation number ? "
  asknum (value2) "Start           \1\ ? "
  print "Enter END   OBSERVATION number ? "
  asknum (VALUE3) "END             \1\ ? "
  numin = value3-value2+1
  print "Number of images to be processed = " (numin)
  print "Give a SUFFIX for the image(s) (NONE = no suffix) : "
  askname (suff) "Suffix        \NONE\ ? "
  suff2 = upcase(suff)
  if suff2 = "NONE"
    suff = ""
  end if
  print "Input name of BAD PIXEL MASK to be used :"
  askname (mask) "Bad Pixel Mask \$LIRCAMDIR/bpm_fpa42_256x256\ ? "
  gf = mask & ".sdf"
  fexist = file_exists(gf)
  if fexist
    print "File " (gf) " found"
  else
    print "File " (gf) " not found, try again!"
    print "Input name of BAD PIXEL MASK to be used :"
    askname (mask) "Bad Pixel Mask \$LIRCAMDIR/bpm_fpa42_256x256\ ? "
    gf = mask & ".sdf"
    fexist = file_exists(gf)
    if fexist
      print "File " (gf) " found"
    else
      print "File " (gf) " not found, EXITING"
      return
    end if
  end if
  get plt2d platscal (platscal)
  if platscal = 1.0
    print "Enter pixel scale in arcsec/pixel : "
    asknum (platscal) "Pixel Scale \0.286\ ? "
    send plt2d set platscal (platscal)
    send plt2d set arcsec_pixel (platscal)
  end if
  print "Pixel scale = " (platscal)
  print "Give name of telescope offset (ASCII) file for mosaic : "
  askname (toff) "Offset file \off.dat\ ? "
  fexist = file_exists(toff)
  if fexist
    print "File " (toff) " found"
  else
    print "File " (toff) " not found, try again!"
    print "Give name of (ASCII) file file with NOMINAL TELESCOPE OFFSETS : "
    askname (toff) "Offset file \off.dat\ ? "
    fexist = file_exists(toff)
    if fexist
      print "File " (toff) " found"
    else
     print "File " (toff) " not found, EXITING"
      return
    end if
  end if
  print "Give search box size (in pixels) for offset search : "
  asknum (box) "Box Size \11\ ? "
  fclose_c
  open cfile (toff)
  loop for dummy = 1 to (numin)
    readr cfile (x) (y)
  end loop
  fclose_c
  open cfile (toff)
  fclose_b
  delfile mofflot.dat
  create bfile "mofflot.dat"
  readr cfile (x0) (y0)
  xo = x0:10:2
  yo = y0:10:2
  dline = (xo) & (yo)
  write bfile (dline)
  value4 = value3-1
  loop for dummy = (value2) to (value4)
    readr cfile (x) (y)
    x2 = (x0-x)/platscal
    y2 = -1.0*(y0-y)/platscal
    x2 = integer(x2)
    y2 = integer(y2)
    dummy2 = dummy + 1
    im1 = init & dummy & suff
    im2 = init & dummy2 & suff
    print "Analysing images " (im1) " and " (im2) " ..."
    print "  Nominal offsets = " (x) (y)
    obeyw rapi2d MOFF (im1) (im2) (x2) (y2) (box) Y (mask) \
    getpar glob moff_dcoff (dcoff)
    getpar glob moff_xoff (xoff)
    getpar glob moff_yoff (yoff)
    xoff = (x0-xoff*platscal)
    yoff = y0+(yoff*platscal)
    xo = xoff:10:2
    yo = yoff:10:2
    dco = dcoff:20:2
    dline = (xo) & (yo)
    write bfile (dline)
    x0 = xoff
    y0 = yoff
  end loop
  close cfile
  close bfile
  exception EOF
    close cfile
    print "Error, EOF detected on check of file " (toff)
  end exception
end proc
