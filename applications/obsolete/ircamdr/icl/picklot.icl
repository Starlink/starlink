{ PROCEDURE PICKLOT : pickims a number of images
proc picklot usepeak curall
  print "Procedure to extract sub-array from from images"
  print "Do you want to enter IMAGE NAMES or PREFIX/RANGE OF NUMBER ?"
  askchoice (value1) "Names or Prefix,Numbers,Suffix (N,P) \P\ ? "
  yn = undefined(usepeak)
  yn2 = undefined(curall)
  if yn
    usepeak2 = "N"
  else
    usepeak2 = upcase(usepeak)
    usepeak2 = substr(usepeak2,1,1)
    if usepeak2 = "Y"
      usepeak2 = "Y"
      print "Optimizing image centre to peak pixel"
    else
      usepeak2 = "N"
    end if
  end if
  if yn2
    curall2 = "N"
  else
    curall2 = upcase(curall)
    if curall2 = "YUP"
      curall2 = "Y"
    else
      curall2 = "N"
    end if
  end if
  if value1 = 2
    print "Enter image names PREFIX ? "
    askname (ffpre) "Prefix \im_\ ? "
    print "Enter start number ? "
    asknum (value2) "Start   \1\ ? "
    print "Enter end number ? "
    asknum (value3) "End    \20\ ? "
    print "Enter image names SUFFIX (NONE = no suffix)  ? "
    askname (ffsuf) "Suffix  \NONE\ ? "
    ffsuf2 = upcase(ffsuf)
  else
    value2 = 1
    value3 = 1000
  end if
  print "Use cursor to define image centre ? "
  asklog (ucur) "Use Cursor (Yes or No) \N\ ? "
  iflag = 0
  loop for dummy = (value2) to (value3)
    if value1 = 1
      print "Enter SOURCE image to be processed (RETURN to stop)"
      askname (im) "Source Image \-1\ ? "
      if im = "-1"
        print "O.K. finished using PICKLOT ..."
        return
      end if
      out = im & "p"
    else
      st2 = ffpre & dummy
      if ffsuf2 = "NONE"
        im = st2
      else
        im = st2 & ffsuf
      end if
      out = im & "p"
    end if
    if curall2 = "Y" or iflag = 0
      if iflag = 0
        iflag = 1
      end if
      if ucur = 1
        send plt2d set cursor_image (im)
        obeyw plt2d nsigma (im)
        print "Select the IMAGE CENTRE with cursor ..."
        obeyw plt2d cursor
        get plt2d x_cur_pixel (xpix)
        get plt2d y_cur_pixel (ypix)
        print "Pixel selected = " (xpix) (ypix)
        xpix = integer(xpix)
        ypix = integer(ypix)
        if usepeak2 = "Y"
          xst = integer(xpix-5.0)
          yst = integer(ypix-5.0)
print (xst) (yst)
          obeyw rapi2d HISTO (im) (xst) (yst) 11 11 \
          getpar glob histo_max (maxval)
          getpar glob histo_min (minval)
          getpar glob histo_xmax (xcentre)
          getpar glob histo_ymax (ycentre)
          print "Peak signal = " (maxval) " at pixel " (xcentre) "," (ycentre)
          xpix = integer(xcentre)
          ypix = integer(ycentre)
        end if
        print "Give out image size (in pixels) : "
        asknum (pickxsz) "X Image Size \64\ ? "
        asknum (pickysz) "Y Image Size \64\ ? "
        pickxsz = integer(pickxsz)
        pickysz = integer(pickysz)
        pickxst = integer(xpix-(pickxsz/2.0))
        pickyst = integer(ypix-(pickysz/2.0))
      else
        print "Enter the X-start pixel for the extraction :"
        asknum (pickxst) "X-Start  \1\ ? "
        print "Enter the Y-start pixel for the extraction :"
        asknum (pickyst) "Y-Start  \1\ ? "
        print "Enter the X-size of sub-array extracted :"
        asknum (pickxsz) "X-Size \256\ ? "
        print "Enter the Y-size of sub-array extracted :"
        asknum (pickysz) "Y-Size \256\ ? "
      end if
    end if
    print "Extracting sub-array from image " (im) " ..."
    obeyw rapi2d PICKIM (im) (pickxst) (pickyst) (pickxsz) ~
      (pickysz) (out) \
    print "Output image = " (out)
    print "+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++"
  end loop
end proc

