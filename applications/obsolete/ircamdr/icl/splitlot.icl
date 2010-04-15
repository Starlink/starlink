{ PROCEDURE SPLITLOT : splits a number of images into quadrants
proc splitlot
  print "Procedure to split image into four quadrants"
  print "Do you want to enter IMAGE NAMES or PREFIX/RANGE OF NUMBER ?"
  askchoice (value1) "Names or Prefix,Numbers,Suffix (N,P) \P\ ? "
  if value1 = 2
    print "Enter image names PREFIX ? "
    askname (ffpre) "Prefix   \im_\ ? "
    print "Enter start number ? "
    asknum (value2) "Start      \1\ ? "
    print "Enter end number ? "
    asknum (value3) "End       \20\ ? "
    print "Enter image names SUFFIX (NONE = no suffix) ? "
    askname (ffsuf) "Suffix  \NONE\ ? "
    ffsuf2 = upcase(ffsuf)
    if ffsuf2 = "NONE"
      ffsuf = ""
    end if
  else
    value2 = 1
    value3 = 1000
  end if
  loop for dummy = (value2) to (value3)
    if value1 = 1
      print "Enter SOURCE image to be processed (RETURN to stop)"
      askname (im) "Source Image \-1\ ? "
      compare (im) "-1" (brave_man)
      if brave_man = 1
        print "O.K. finished using SPLITLOT ..."
        return
      end if
    else
      im = ffpre & dummy & ffsuf
    end if
    obeyw rapi2d shsize (im) \
    getpar glob shsize_xdim (xdim)
    getpar glob shsize_ydim (ydim)
    x1 = 1
    y1 = 1
    x1z = int(xdim/2.0+0.5)
    y1z = int(ydim/2.0+0.5)
    out = im & "a"
    print "Splitting image " (im) " ... quadrant bottom-left = a" (out)
    print "  X,Y start = " (x1) "," (y1) " - X,Y size = " (x1z) "x" (y1z)
    obeyw rapi2d PICKIM (im) (x1) (y1) (x1z) (y1z) (out) \
    print "  Output image = " (out)
    x2 = x1z+1
    y2 = 1
    x2z = xdim-x1z
    y2z = int(ydim/2.0+0.5)
    out = im & "b"
    print "Splitting image " (im) " ... quadrant bottom-right= b" (out)
    print "  X,Y start = " (x2) "," (y2) " - X,Y size = " (x2z) "x" (y2z)
    obeyw rapi2d PICKIM (im) (x2) (y2) (x2z) (y2z) (out) \
    print "  Output image = " (out)
    x3 = 1
    y3 = y1z+1
    x3z = int(xdim/2.0+0.5)
    y3z = ydim-y1z
    out = im & "c"
    print "Splitting image " (im) " ... quadrant top-left = c" (out)
    print "  X,Y start = " (x3) "," (y3) " - X,Y size = " (x3z) "x" (y3z)
    obeyw rapi2d PICKIM (im) (x3) (y3) (x3z) (y3z) (out) \
    print "  Output image = " (out)
    x4 = x1z+1
    y4 = y1z+1
    x4z = xdim-x1z
    y4z = ydim-y1z
    out = im & "d"
    print "Splitting image " (im) " ... quadrant top-right= d" (out)
    print "  X,Y start = " (x4) "," (y4) " - X,Y size = " (x4z) "x" (y4z)
    obeyw rapi2d PICKIM (im) (x4) (y4) (x4z) (y4z) (out) \
    print "Output image = " (out)
    print "+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++"
  end loop
end proc

