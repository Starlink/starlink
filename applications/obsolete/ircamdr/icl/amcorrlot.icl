{ PROCEDURE AMCORRLOT : airmass correct a number of images
proc amcorrlot
  print "Procedure to airmass correct images"
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
    print "Input filter used for observations : "
    askchoice (ifilt) "Filter Used (J,H,K,NBL,LP,NBM) \K\ ? "
    if ifilt = 1
      filter = "J"
    else if ifilt = 2
      filter = "H"
    else if ifilt = 3
      filter = "K"
    else if ifilt = 4
      filter = "NBL"
    else if ifilt = 5
      filter = "LP"
    else if ifilt = 6
      filter = "NBM"
    end if
    print "Give airmass at time of observation : "
    asknum (airm) "Airmass Value \1.0\ ? "
  else
    value2 = 1
    value3 = 1000
  end if
  loop for dummy = (value2) to (value3)
    if value1 = 1
      print "Enter SOURCE image to be processed (RETURN to stop)"
      askname (im) "Source Image \-1\ ? "
      if im = "-1"
        print "O.K. finished using AMCORRLOT ..."
        return
      end if
      print "Input filter used for observations : "
      askchoice (ifilt) "Filter Used (J,H,K,NBL,LP,NBM) \K\ ? "
      if ifilt = 1
        filter = "J"
      else if ifilt = 2
        filter = "H"
      else if ifilt = 3
        filter = "K"
      else if ifilt = 4
        filter = "NBL"
      else if ifilt = 5
        filter = "LP"
      else if ifilt = 6
        filter = "NBM"
      end if
      print "Give airmass at time of observation : "
      asknum (airm) "Airmass Value \1.0\ ? "
    else
      im = ffpre & dummy & ffsuf
    end if
    out = im & "a"
    print "Airmass correcting image " (im)
    print "  Filter  = " (filter)
    print "  Airmass = " (airm)
    obeyw obsrap AMCORR (im) (out) (filter) (airm) \
    print "Airmass corrected image output to " (out)
    print "+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++"
  end loop
end proc

