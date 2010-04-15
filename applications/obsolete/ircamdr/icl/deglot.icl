{ PROCEDURE DEGLOT : deglitches a number of images
proc deglot
  print "Do you want to enter IMAGE NAMES or PREFIX/RANGE OF NUMBER ?"
  askchoice (value1) "Names or Prefix,Numbers,Suffix (N,P) \P\ ? "
  if value1 = 2
    print "Enter image names PREFIX ? "
    askname (ffpre) "Prefix      \im_\ ? "
    print "Enter start number ? "
    asknum (value2) "Start         \1\ ? "
    print "Enter number of images in sequence ? "
    asknum (numin) "Number Images \20\ ? "
    value3 = integer(value2+numin-1)
    print "Enter image names SUFFIX (NONE = no suffix) ? "
    askname (ffsuf) "Suffix   \NONE\ ? "
    ffsuf2 = upcase(ffsuf)
    if ffsuf2 = "NONE"
      ffsuf = ""
    end if
  else
    value2 = 1
    value3 = 1000
  end if
  print "Use ASCII glitch file or search for magic number ? "
  askchoice (gformn) "Glitch file or Magic number (G,M) \M\ ? "
  if gformn = 1
    print "Input name of ASCII glitch file to be used :"
    askchar (glitchf) "Glitch File \glitch.list\ ? "
  else
    print "Give magic number to indicate bad pixel : "
    asknum (magnum) "Magic Number \-1.0E-20\ ? "
  end if
  loop for dummy = (value2) to (value3)
    if value1 = 1
      print "Enter SOURCE image to be processed (RETURN to stop)"
      askname (im) "Source Image \-1\ ? "
      if im = "-1"
        print "O.K. finished using DEGLOT ..."
        return
      end if
      out = im & "g"
    else
      im = ffpre & dummy & ffsuf
      out = im & "g"
    end if
    if gformn = 1
      print "Deglitching image " (im) " using " (glitchf) " ..."
      obeyw rapi2d GLITCH (im) (out) 'DEGLITCHED' F (glitchf)
    else
      print "Deglitching image " (im) " using magic number " (magnum) " ..."
      obeyw rapi2d GLITCH (im) (out) 'DEGLITCHED' A junk.dat (magnum)
    end if
    print "Deglitched image output to : " (out)
    print "+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++"
  end loop
end proc

