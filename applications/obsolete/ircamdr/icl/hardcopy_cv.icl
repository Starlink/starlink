{ PROCEDURE HARDCOPY_CV : contour+vector map on META etc ...
proc hardcopy_cv
  get plt2d last_line (last_line)
  if last_line < 1 or last_line > 4
    print "Error, LINE PLOT is not available cause you ain't done one yet..."
    return
  end if
  get plt2d worknum (worknum)
  get plt2d worknam (worknam)
  oldname = worknam
  oldnumb = worknum
  print "Closing plotting on device " (worknam) " ..."
  get plt2d max_xsize (oldmaxx)
  get plt2d max_ysize (oldmaxy)
  get plt2d contour_xcen (oldxcen)
  get plt2d contour_ycen (oldycen)
  get plt2d contour_magnif (oldcmag)
  if oldcmag = 0
    mag0 = 1
    get plt2d contour_calmag (oldcmag)
  else
    mag0 = 0
  end if
  get plt2d hard_device (hard_device)
  obeyw plt2d close
  POPEN (hard_device)
  get plt2d max_xsize (newmaxx)
  get plt2d max_ysize (newmaxy)
  print " Size of hardcopy device (X,Y) = " (newmaxx) "," (newmaxy)
  print "Re-plotting the last CONTOUR MAP ..."
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
  send plt2d set contour_xcen (value1)
  send plt2d set contour_ycen (value2)
  send plt2d set contour_magnif (newcmag)
  send plt2d set polmag (newcmag)
  send plt2d set pol_xcen (value1)
  send plt2d set pol_ycen (value2)
  obeyw plt2d contour
  print "Re-plotting the last POLARIZATION VECTOR MAP  ..."
  obeyw plt2d polax
  send plt2d set contour_xcen (oldxcen)
  send plt2d set contour_ycen (oldycen)
  if mag0 = 1
    send plt2d set contour_magnif 0
  else
    send plt2d set contour_magnif (oldcmag)
  end if
  send plt2d set polmag (oldcmag)
  send plt2d set pol_xcen (oldxcen)
  send plt2d set pol_ycen (oldycen)
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
  print "Finished ..."
end proc

