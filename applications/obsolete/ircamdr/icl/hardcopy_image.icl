{ PROCEDURE HARDCOPY_IMAGE : closes plotting, opens it on hard workstation...
proc hardcopy_image
  get plt2d annotate_colour (oancoa)
  oancoa = upcase(oancoa)
  get plt2d border_colour (oancob)
  oancob = upcase(oancob)
  get plt2d worknam (worknam)
  get plt2d worknum (worknum)
  oldname = worknam
  oldnumb = worknum
  print "Closing plotting on " (worknam) " ..."
  get plt2d max_xsize (oldmaxx)
  get plt2d max_ysize (oldmaxy)
  get plt2d magnification (oldcmag)
  if oldcmag = 0
    mag0 = 1
    get plt2d image_calmag (oldcmag)
    print "Magnification = 0 specified, calculated value = " (oldcmag)
  else
    mag0 = 0
  end if
  obeyw plt2d close
  get plt2d hard_device (hard_device)
  POPEN (hard_device)
  get plt2d max_xsize (newmaxx)
  get plt2d max_ysize (newmaxy)
  print " Size of hardcopy device (X,Y) = " (newmaxx) "," (newmaxy)
  value1 = newmaxx/2.0
  value2 = newmaxy/2.0
  get plt2d im_xst (xims)
  get plt2d im_yst (yims)
  get plt2d im_xen (xime)
  get plt2d im_yen (yime)
  ximc = (xime-xims)/oldcmag
  yimc = (yime-yims)/oldcmag
  print "  Current image size (X,Y) = " (ximc) "," (yimc)
  if ximc > 0
    temp1 = newmaxx/ximc
  else
    temp1 = 1
  end if
  if yimc > 0
    temp2 = newmaxy/yimc
  else
    temp2 = 1
  end if
  newcmag = 0.8*(min(temp1,temp2))
  print "  Old,New Magnifications = " (oldcmag) "," (newcmag)
  print "Do you want to change the NEW magnification ?"
  asklog (chmag) "Change New Mag (Yes or No) \N\ : "
  if chmag = 1
    value6 = newcmag
    print "Enter NEW magnification (DEFAULT = SAME AS ABOVE) : "
    asknum (newcmag) "New Value ? \-1\ : "
    if newcmag = -1
      newcmag = value6
    end if
    print "New magnification = " (newcmag)
  end if
  rat = newcmag/oldcmag
  send plt2d set im_xcen (value1)
  send plt2d set im_ycen (value2)
  send plt2d set magnification (newcmag)
  if hard_device=18 or hard_device=19 or hard_device=20 or hard_device=21
    print "Give colour code for ANNOTATION, BORDER and TICKS : "
    askchar (anco) "Colour Code \R\ : "
    anco = upcase(anco)
    send plt2d set annotate_colour (anco)
    send plt2d set border_colour (anco)
    get plt2d last_lut (last_lut)
    print "    Loading colour table " (last_lut) " please wait"
    obeyw plt2d coltab (last_lut)
    print "    Colour table loaded ..."
  end if
  get plt2d last_plot (last_plot)
  get plt2d name_image (name_image)
  if last_plot = 1 or last_plot = 4
    print "Last image display was a PLOT ..."
    obeyw plt2d plot (name_image)
  else if last_plot = 2 or last_plot = 5
    print "Last image display was an NSIGMA ..."
    obeyw plt2d nsigma (name_image)
  else if last_plot = 3 or last_plot = 6
    print "Last image display was a RANPLOT ..."
    obeyw plt2d ranplot (name_image)
  else if last_plot = 7 or last_plot = 8
    print "Last image display was a FLASH ..."
    obeyw plt2d flash (name_image)
  else if last_plot = 9 or last_plot = 10
    print "Last image display was a VARGREY ..."
    obeyw plt2d vargrey (name_image)
  end if
  HARDANNOT (rat)
  value1 = oldmaxx/2.0
  value2 = oldmaxy/2.0
  send plt2d set im_xcen (value1)
  send plt2d set im_ycen (value2)
  if mag0 = 1
    send plt2d set magnification 0
  else
    send plt2d set magnification (oldcmag)
  end if
  send plt2d set im_xst (xims)
  send plt2d set im_yst (yims)
  send plt2d set im_xen (xime)
  send plt2d set im_yen (yime)
  print "Finished ..."
  print "Closing plotting on hardcopy device ..."
  obeyw plt2d close
  HARDPRINT (hard_device)
  print "Re-opening workstation " (oldname) " ... "
  worknum = oldnumb
  worknam = oldname
  POPEN (worknum)
  if hard_device=18 or hard_device=19 or hard_device=20 or hard_device=21
    send plt2d set annotate_colour (oancoa)
    send plt2d set border_colour (oancob)
  end if
  print "Finished ..."
end proc

