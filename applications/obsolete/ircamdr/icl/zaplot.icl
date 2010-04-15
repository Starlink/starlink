{ PROCEDURE ZAPLOT : zaplin2 a number of images
proc zaplot
  print "Do you want to enter IMAGE NAMES or PREFIX/RANGE OF NUMBER ?"
  askchoice (value1) "Names or Prefix,Numbers,Suffix (N,P) \P\ ? "
  if value1 = 2
    print "Enter image names PREFIX ? "
    askname (ffpre) "Prefix    \im_\ ? "
    print "Enter start number ? "
    asknum (value2) "Start       \1\ ? "
    print "Enter end number ? "
    asknum (value3) "End        \20\ ? "
    print "Enter image names SUFFIX ? "
    askname (ffsuf) "Suffix   \NONE\ ? "
    ffsuf2 = upcase(ffsuf)
    if ffsuf2 = "NONE"
      ffsuf = ""
      print "NO suffix specified"
    end if
  else
    value2 = 1
    value3 = 1000
  end if
  print "Do numbers refer to rows or columns in image ?"
  askchoice (rc) "Columns or Rows (C,R) \R\ ? "
  if rc = 1
    crc = "C"
  else
    crc = "R"
  end if
  print "Enter start/end row/column number :"
  askchar (startr) "Start Row\Column \1/ ? "
  askchar (endr)   "End   Row\Column \1/ ? "
  loop for dummy = (value2) to (value3)
    if value1 = 1
      print "Enter SOURCE image to be processed (RETURN to stop) "
      askname (im) "Source Image \-1\ ? "
      compare (im) "-1" (brave_man)
      if brave_man = 1
        print "O.K. finished using ZAPLOT ..."
        return
      end if
      out = im & "z"
    else
      im = ffpre & dummy & ffsuf
      out = im & "z"
    end if
    print "ZAPLIN-ing image " (im )
    obeyw rapi2d ZAPLIN (im) (out) (crc) (startr) (endr) N \
    print "Image ZAPLIN-ed, output to : " (out)
    print "+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++"
  end loop
end proc
