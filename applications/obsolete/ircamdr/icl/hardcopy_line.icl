{ PROCEDURE HARDCOPY_LINE : closes plotting, opens it on hardcopy workstation...
proc hardcopy_line
  get plt2d last_line (last_line)
  if last_line < 1 or last_line > 4
    print "Error, LINE PLOT is not available cause you ain't done one yet..."
    return
  end if
  get plt2d worknam (worknam)
  get plt2d worknum (worknum)
  oldname = worknam
  oldnumb = worknum
  print "Closing plotting on " (worknam) " ..."
  get plt2d max_xsize (oldmaxx)
  get plt2d max_ysize (oldmaxy)
  if last_line = 1 or last_line = 3
    get plt2d cut_xcen (oldxcen)
    get plt2d cut_ycen (oldycen)
    get plt2d cut_magnif (oldcmag)
    if oldcmag = 0
      mag0 = 1
      get plt2d cut_calmag (oldcmag)
    else
      mag0 = 0
    end if
  else if last_line = 2
    get plt2d contour_xcen (oldxcen)
    get plt2d contour_ycen (oldycen)
    get plt2d contour_magnif (oldcmag)
    if oldcmag = 0
      mag0 = 1
      get plt2d contour_calmag (oldcmag)
    else
      mag0 = 0
    end if
  else if last_line = 4
    get plt2d pol_xcen (oldxcen)
    get plt2d pol_ycen (oldycen)
    get plt2d polmag (oldcmag)
    if oldcmag = 0
      mag0 = 1
      get plt2d polax_calmag (oldcmag)
    else
      mag0 = 0
    end if
  end if
  get plt2d hard_device (hard_device)
  obeyw plt2d close
  POPEN (hard_device)
  get plt2d max_xsize (newmaxx)
  get plt2d max_ysize (newmaxy)
  print " Size of hardcopy device (X,Y) = " (newmaxx) "," (newmaxy)
  if last_line = 1 or last_line = 3
    print "Re-plotting the last 1D CUT ..."
    send plt2d set cut_positioning 'KEY'
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
    newcmag = 0.9*min(temp1,temp2)
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
    send plt2d set cut_xcen (value1)
    send plt2d set cut_ycen (value2)
    obeyw plt2d cut
    send plt2d set cut_xcen (oldxcen)
    send plt2d set cut_ycen (oldycen)
    if mag0 = 1
      send plt2d set cut_magnif 0
    else
      send plt2d set cut_magnif (oldcmag)
    end if
    send plt2d set im_xst (xims)
    send plt2d set im_yst (yims)
    send plt2d set im_xen (xime)
    send plt2d set im_yen (yime)
  else if last_line = 2 or last_line = 4
    if last_line = 2
      print "Re-plotting the last CONTOUR MAP ..."
    else
      print "Re-plotting the last POLARIZATION VECTOR MAP ..."
    end if
    send plt2d set contour_posn 'KEY'
    send plt2d set pol_positioning 'KEY'
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
    newcmag = 0.75*min(temp1,temp2)
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
    if last_line = 2
      send plt2d set contour_xcen (value1)
      send plt2d set contour_ycen (value2)
      send plt2d set contour_magnif (newcmag)
      obeyw plt2d contour
      send plt2d set contour_xcen (oldxcen)
      send plt2d set contour_ycen (oldycen)
      if mag0 = 1
        send plt2d set contour_magnif 0
      else
        send plt2d set contour_magnif (oldcmag)
      end if
      send plt2d set im_xst (xims)
      send plt2d set im_yst (yims)
      send plt2d set im_xen (xime)
      send plt2d set im_yen (yime)
    else
      send plt2d set polmag (newcmag)
      send plt2d set pol_xcen (value1)
      send plt2d set pol_ycen (value2)
      obeyw plt2d polax
      if mag0 = 1
        send plt2d set polmag 0
      else
        send plt2d set polmag (oldcmag)
      end if
      send plt2d set pol_xcen (oldxcen)
      send plt2d set pol_ycen (oldycen)
      send plt2d set im_xst (xims)
      send plt2d set im_yst (yims)
      send plt2d set im_xen (xime)
      send plt2d set im_yen (yime)
    end if
  end if
  print "Finished ..."
  print "Closing plotting on hardcopy device ..."
  obeyw plt2d close
  HARDPRINT (hard_device)
  print "Re-opening workstation " (oldname) " ..."
  worknum = oldnumb
  worknam = oldname
  POPEN (worknum)
  print "Finished ..."
end proc

