{ Procedure WRAPLOT : corrects for wrap around in a number of images
proc wraplot
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
  print "Enter value below which number is added ?"
  asknum (datval) "Value in Data \0.0\ ? "
  print "Enter value added to pixels below above number ?"
  asknum (valadd) "Value Added \65536\ ? "
  loop for dummy = (value2) to (value3)
    if value1 = 1
      print "Enter SOURCE image to be processed (RETURN to stop)"
      askname (im) "Source Image \-1\ ? "
      compare (im) "-1" (brave_man)
      if brave_man = 1
        print "O.K. finished using WRAPLOT ..."
        return
      end if
      out = im &"w"
    else
      im = ffpre & dummy & ffsuf
      out = im & "w"
    end if
    print "Correcting image " (im) " for wraparound ..."
    print "Values below " (datval) " will have " (valadd) " added"
    obeyw obsrap WRAPCOR (im) (out) (datval) (valadd) \
    print "Output image = " (out)
    print "+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++"
  end loop
end proc

