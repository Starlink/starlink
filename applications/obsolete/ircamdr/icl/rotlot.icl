{ PROCEDURE ROTLOT : rotates a number of images
proc rotlot
  print "Do you want to enter IMAGE NAMES or PREFIX/RANGE OF NUMBER ?"
  askchoice (value1) "Names or Prefix,Numbers,Suffix (N,P) \P\ ? "
  if value1 = 2
    print "Enter image names PREFIX ? "
    askname (ffpre) "Prefix   \im_\ ? "
    print "Enter start number ? "
    asknum (value2) "Start      \1\ ? "
    print "Enter end number ? "
    asknum (value3) "End       \20\ ? "
    print "Enter image names SUFFIX (NONE=no suffix) ? "
    askname (ffsuf) "Suffix   \NONE\ ? "
    ffsuf2 = upcase(ffsuf)
    if ffsuf2 = "NONE"
      print "No SUFFIX specified"
      ffsuf = ""
    end if
  else
    value2 = 1
    value3 = 1000
  end if
  print "Enter rotation in degrees clockwise ?"
  asknum (rotval) "Rotation in Degrees \90.0\ ? "
  loop for dummy = (value2) to (value3)
    if value1 = 1
      print "Enter SOURCE image to be processed (RETURN to stop)"
      askname (im) "Source Image \-1\ ? "
      compare (im) "-1" (brave_man)
      if brave_man = 1
        print "O.K. finished using ROTLOT ..."
        return
      end if
      out = im
    else
      tochar (dummy) (st1)
      im = ffpre & dummy & ffsuf
      out = im & "r"
    end if
    print "Rotating image " (im) " ..."
    print "Rotating by " (rotval) " degrees clockwise"
    obeyw rapi2d ROTATE (im) (rotval) (out) \
    print "Output image = " (out)
    print "+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++"
  end loop
end proc
