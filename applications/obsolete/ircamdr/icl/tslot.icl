{ PROCEDURE TSLOT : scales a number of images
proc tslot
  print "Threshold/scale MIN,MAX to 0,255 or other ? "
  askchoice (scch) "Scale Choice (M,O) \M\ ? "
  if scch = 2
    print "Give lower and upper threshold values : "
    asknum (low) "Lower Threshold Value \0\ ? "
    asknum (upe) "Upper Threshold Value \255\ ? "
    print "Do you want to enter IMAGE NAMES or PREFIX/RANGE OF NUMBER ?"
  end if
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
    if value1 = 1
      print "Enter SOURCE image to be processed (RETURN to stop) "
      askname (im) "Source Image \-1\ ? "
      compare (im) "-1" (brave_man)
      if brave_man = 1
        print "O.K. finished using TSLOT ..."
        return
      end if
      out = im & "ts"
    else
      tochar (dummy) (st1)
      st2 = ffpre & st1
      im = st2 & ffsuf
      out = im & "st"
    end if
    print "Scaling and thresholding image " (im)
    if scch = 1
      obeyw rapi2d HISTO (im) \
      getpar glob histo_max (maxval)
      getpar glob histo_min (minval)
{print "1. max,min = " (maxval) (minval)
      obeyw rapi2d CSUB (im) (minval) junk
      obeyw rapi2d HISTO junk \
      getpar glob histo_max (maxval)
      getpar glob histo_min (minval)
{print "2. max,min = " (maxval) (minval)
      obeyw rapi2d THRESH junk junk2 "junk" 0 0 1.0e20 1.0e20
      delfile junk.sdf
    else
      minval = real(low)
      maxval = real(upe)
      obeyw rapi2d THRESH (im) junk "junk" (minval) (minval) (maxval) (maxval)
      obeyw rapi2d CSUB junk (minval) junk2
    end if
    if scch = 1
      obeyw rapi2d HISTO junk2 \
      getpar glob histo_max (maxval)
      getpar glob histo_min (minval)
{print "3. max,min = " (maxval) (minval)
      scal = maxval/255.0
{print "scal = " (scal)
      obeyw rapi2d CDIV junk2 (scal) (out)
      delfile junk2.sdf
    else
      scal = (maxval-minval)/255.0
      print "Scale factor = " (scal)
      obeyw rapi2d CDIV junk2 (scal) (out)
    end if
    print "Image scaled and thresholded, output to : " (out)
    print "+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++"
  end loop
end proc
