{ PROCEDURE MEDLOT : subtracts median from a number of images
proc medlot
  print "Procedure to subtract median of image from images"
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
    suf2 = upcase(ffsuf)
    if suf2 = "NONE"
      ffsuf = ""
      print "NO suffix specified"
    end if
  else
    value2 = 1
    value3 = 1000
  end if
  print "X-start of median calculation area ?"
  asknum (mxst) "X-Start \1\ ? "
  print "Y-start of median calculation area ?"
  asknum (myst) "Y-Start \1\ ? "
  print "X-size  of median calculation area ?"
  asknum (mxsz) "X-Size  \1000\ ? "
  print "Y-size  of median calculation area ?"
  asknum (mysz) "Y-Size  \1000\ ? "
  loop for dummy = (value2) to (value3)
    if value1 = 1
      print "Enter SOURCE image to be processed (RETURN to stop)"
      askname (im) "Source Image \-1\ ? "
      compare (im) "-1" (brave_man)
      if brave_man = 1
        print "O.K. finished using MEDLOT ..."
        return
      end if
      out = im & "m"
    else
      im = ffpre & dummy & ffsuf
      out = im & "m"
    end if
    print "Calculating and subtracting median from image " (im) " ..."
    obeyw rapi2d STATS (im) (mxst) (myst) (mxsz) (mysz) NO \
    getpar glob stats_median (medscal)
    print "Median in specified area = " (medscal)
    obeyw rapi2d CSUB (im) (medscal) (out)
    print "Output image = " (out)
    print "+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++"
  end loop
end proc

