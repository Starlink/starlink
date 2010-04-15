{ PROCEDURE FLATLOT : flat fields a number of images
proc flatlot
  print "Flatfield images using user-specified flatfield image."
  print "Enter name of flatfield image to be used "
  askname (flat) "Flatfield image ? "
  flatn = flat & "_norm"
  print "Do you want the flatfield image normalized to unity ?"
  asklog (normy) "Normalize Flatfield (Yes or No) \Y\ ? "
  if normy = 1
    print "Calculating normalization factor for flatfield " (flat)
    print "N.B. Normalization area is the whole image ..."
    obeyw rapi2d STATS (flat) 1 1 10000 10000 NO \
    getpar glob stats_median (medscal)
    print "Median of normalization area is " (medscal)
    print "Normalizing flatfield image " (flat) ": Output = " (flatn)
    obeyw rapi2d CDIV (flat) (medscal) (flatn)
  else
    flatn = flat
  end if
  print "Do you want to enter IMAGE NAMES or PREFIX/RANGE OF NUMBER ?"
  askchoice (value1) "Names or Prefix,Numbers,Suffix (N,P) \P\ ? "
  if value1 = 2
    print "Enter image names PREFIX ? "
    askname (ffpre) "Prefix    \im_\ ? "
    print "Enter start number ? "
    asknum (value2) "Start       \1\ ? "
    print "Enter end number ? "
    asknum (value3) "End        \20\ ? "
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
  loop for dummy = (value2) to (value3)
    if value1 = 1
      print "Enter SOURCE image to be processed (RETURN to stop) "
      askname (im) "Source Image \-1\ ? "
      compare (im) "-1" (brave_man)
      if brave_man = 1
        print "O.K. finished using FLATLOT ..."
        return
      end if
      out = im & "f"
    else
      im  = ffpre & dummy & ffsuf
      out = im & "f"
    end if
    print "Flatfielding image " (im) " ..."
    obeyw rapi2d DIV (im) (flatn) (out)
    print "Flatfielded image output to : " (OUT)
    print "+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++"
  end loop
end proc

