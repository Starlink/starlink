{ PROCEDURE SCALELOT : scales a number of images
proc scalelot
  print "Do you want to enter IMAGE NAMES or PREFIX/RANGE OF NUMBER ?"
  askchoice (value1) "Names or Prefix,Numbers,Suffix (N,P) \P\ ? "
  print "Are all exposure times/number of coadds the SAME or DIFFERENT :"
  askchoice (samediff) "Same or Different (S,D) \S\ ? "
  samediff = samediff - 1
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
      print "No suffix specified"
      ffsuf = ""
    end if
  else
    value2 = 1
    value3 = 1000
  end if
  if samediff = 0
    print "Enter exposure time in seconds of images :"
    asknum (nexp) "Exposure Time \1\ ? "
    print "Enter number of coadds in images :"
    asknum (nco) "Number of Coadds \1\ ? "
  end if
  loop for dummy = value2 to value3
    if value1 = 1
      print "Enter SOURCE image to be processed (RETURN to stop) "
      askname (im) "Source Image \-1\ ? "
      compare (im) "-1" (brave_man)
      if brave_man = 1
        print "O.K. finished using SCALELOT ..."
        return
      end if
      concat (im) "s" (out)
    else
      tochar (dummy) (st1)
      concat (ffpre) (st1) (st2)
      concat (st2) (ffsuf) (im)
      concat (im) "s" (out)
    end if
    if samediff = 1
      print "Enter exposure time in seconds for image " (im)
      asknum (nexp) "Exposure Time \1\ ? "
      print "Enter number of coadds for image " (im)
      asknum (nco) "Number of Coadds \1\ ? "
    end if
    ntot = nco*nexp
    print "Scaling image " (im) " by factor x" (ntot)
    obeyw rapi2d CDIV (im) (ntot) (out)
    print "Image scaled, output to : " (out)
    print "+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++"
  end loop
end proc

