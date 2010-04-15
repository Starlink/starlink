{ PROCEDURE STLOT : sky subtracts and thresholds a number of images
proc stlot
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
    end if
  else
    value2 = 1
    value3 = 1000
  end if
  loop for dummy = value2 to value3
    delfile $ADAM_USER/GLOBAL.sdf
    if value1 = 1
      print "Enter SOURCE image to be processed (RETURN to stop) "
      askname (im) "Source Image \-1\ ? "
      compare (im) "-1" (brave_man)
      if brave_man = 1
        print "O.K. finished using STLOT ..."
        return
      end if
      out = im & "st"
    else
      tochar (dummy) (st1)
      st2 = ffpre & st1
      im = st2 & ffsuf
      out = im & "st"
    end if
    print "Sky-subtracting and thresholding image " (im)
    obeyw plt2d nsigma (im)
    send plt2d set cursor_image (im)
    print "Select the CENTRE of the box for STATISTICS CALCULATION ..."
    print "with the cursor ..."
    obeyw plt2d cursor
    get plt2d x_cur_pixel (xpix1)
    get plt2d y_cur_pixel (ypix1)
    xstb = int(xpix1-15)
    ystb = int(ypix1-15)
    if xstb < 1
      xstb = 1
    end if
    if ystb < 1
      ystb = 1
    end if
    send plt2d set cursor_cross 'NO'
    get plt2d platscal (opscal)
    send plt2d set platscal 1.0
    send plt2d set arcsec_pixel 1.0
    obeyw plt2d BOX (xstb) (ystb) 30 30 'BOTTOM_LEFT'
    send plt2d set platscal (opscal)
    send plt2d set arcsec_pixel (opscal)
    obeyw rapi2d HISTO (im) (xstb) (ystb) 30 30 \
    obeyw rapi2d STATS (im) (xstb) (ystb) 30 30 \
    getpar glob histo_max (maxval)
    getpar glob histo_min (minval)
    getpar glob stats_median (medval)
    getpar glob stats_std (onesig)
    print "Pixel selected with cursor [centre of box] = " (xpix1) (ypix1)
    print "Maximum value = " (maxval)
    print "Minimum value = " (minval)
    print "Median signal = " (medval)
    delfile junk.sdf
    obeyw rapi2d CSUB (im) (medval) junk
    tensig = onesig*10.0
    minth = -1.0*tensig
    print "1- and 10-sigma in sky = " (onesig) (tensig)
    obeyw rapi2d THRESH junk (out) "junk" (minth) 0 1.0e20 1.0e20
    delfile junk.sdf
    print "Image sky-substracted and thresholded, output to : " (out)
    print "+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++"
  end loop
end proc
