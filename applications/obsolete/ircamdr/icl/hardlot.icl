{ PROCEDURE HARDLOT : plots and does stuff on a number of images
proc hardlot
  print "Procedure to hardcopy a number of images"
  print "Do you want to enter IMAGE NAMES or PREFIX/RANGE OF NUMBER ?"
  askchoice (pltyp) "Names or Prefix,Numbers,Suffix (N,P) \P\ ? "
  if pltyp = 2
    print "Enter image names PREFIX ? "
    askname (ffpre) "Prefix    \im_\ ? "
    print "Enter start number ? "
    asknum (plnst)  "Start       \1\ ? "
    print "Enter end number ? "
    asknum (plnen)  "End        \20\ ? "
    print "Enter image names SUFFIX (NONE = no suffix) ? "
    askname (ffpost) "Suffix  \NONE\ ? "
    suff = upcase(ffpost)
    if suff = "NONE"
      ffpost = ""
    end if
  else
    plnst = 1
    plnen = 1000
  end if
  get plt2d hard_device (hard_device)
  get plt2d magnification (plmag)
  if plmag = 0
    mag0 = 1
    get plt2d image_calmag (plmag)
  else
    mag0 = 0
  end if
  oplmag = plmag
  print "Current hardcopy device = " (hard_device)
  print "Enter magnification for hardcopy, current value = " (plmag)
  asknum (plmag) "Magnification \4\ ? "
  send plt2d set magnification (plmag)
  print "Use PLOT or NSIGMA to display images ? "
  askchoice (plwt) "Plot How (P,N) \N\ ? "
  if plwt = 1
    get plt2d maximum (plmx)
    get plt2d minimum (plmn)
    print "Enter plot MAXIMUM and MINIMUM, current values = " (plmx) (plmn)
    asknum (plmx) "Maximum \100\ ? "
    asknum (plmn) "Minimum   \0\ ? "
    send plt2d set maximum (plmx)
    send plt2d set minimum (plmn)
  end if
  if plwt = 2
    get plt2d sigma_level (plmn)
    print "Enter NSIGMA sigma-level for plot, current value = " (plmn)
    asknum (plmn) "Sigma Level \2\ ? "
    send plt2d set sigma_level (plmn)
  end if
  print "Plot BORDER and NUMBERS around plot ? "
  asklog (surit) "Surround It \N\ ? "
  get plt2d worknum (worknum)
  get plt2d workxcen (oworkxcen)
  get plt2d workycen (oworkycen)
  obeyw plt2d close
  POPEN (hard_device)
  loop for dummy = (plnst) to (plnen)
    if pltyp = 1
      print "Enter SOURCE image to be plotted (RETURN to stop)"
      askname (im) "Source image \-1\ ? "
      compare (im) "-1" (brave_man)
      if brave_man = 1
        print "O.K. finished using PLOTLOT ..."
        return
      end if
    else
      tochar (dummy) (st1)
      concat (ffpre) (st1) (st2)
      concat (st2) (ffpost) (im)
    end if
    name_image = im
    send plt2d set name_image (name_image)
    send plt2d set cursor_image (name_image)
    get plt2d workxcen (workxcen)
    get plt2d workycen (workycen)
    send plt2d set im_xcen (workxcen)
    send plt2d set im_ycen (workycen)
    if plwt = 1
      obeyw plt2d plot (im)
      print "Image " (im) " displayed using PLOT ..."
    end if
    if plwt = 2
      obeyw plt2d nsigma (im)
      print "Image " (im) " displayed using NSIGMA ..."
    end if
    obeyw plt2d line_width 3
    if surit = 1
      obeyw plt2d surround
      obeyw plt2d border 1
    end if
    value1 = workxcen
    value2 = workycen*1.8
    send plt2d set cursor_cross 'NO'
    obeyw plt2d comment (im) (value1) (value2) 50
    obeyw plt2d line_width 3
    obeyw plt2d clear
    print "+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++"
  end loop
  obeyw plt2d close
  POPEN (worknum)
  if mag0 = 1
    send plt2d set magnification 0
  else
    send plt2d set magnification (oplmag)
  end if
  send plt2d set im_xcen (oworkxcen)
  send plt2d set im_ycen (oworkycen)
end proc

