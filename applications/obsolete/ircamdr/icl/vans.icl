{ PROCEDURE VANS : plots vector array
 proc vans imno
  get plt2d name_prefix (ronam)
  utd = substr(ronam,3,6)
  pref = "i" & utd & "_"
  suf = "_1v"
  yn = undefined(imno)
  if yn
    print "Enter object number ? "
    asknum (imst) "Object \1\ ? "
  else
    imst = imno
  end if
  act1 = pref & imst & suf
  snode = getenv("HOST")
  snode1 = upcase(snode)
  if snode1 = "IRTDR::"
    idir_prefix = "[.-.IDIR]"
    print "Looking in [.-.IDIR] directory for vector array images" (act1)
  else
    idir_prefix = ""
    print "Looking in current directory for vector array images" (act1)
  end if
  act1 = idir_prefix & act1
  get plt2d workxcen (workxcen)
  get plt2d workycen (workycen)
  send plt2d set im_xcen (workxcen)
  send plt2d set im_ycen (workycen)
  get plt2d magnification (oldmag)
  get plt2d maximum (omax)
  get plt2d minimum (omin)
  obeyw rapi2d shsize (act1)
  getpar glob shsize_xdim (xsz)
  newmag = 0
  obeyw plt2d clear
  send plt2d set magnification (newmag)
  send plt2d set cursor_image (act1)
  send plt2d set name_image (act1)
  send plt2d set cursor_image (act1)
  print "Plotting " (act1) " from MAX to MIN"
  send plt2d set maximum 0
  send plt2d set minimum 0
  obeyw plt2d plot (act1)
  obeyw plt2d border 1
  send plt2d set maximum (omax)
  send plt2d set minimum (omin)
  send plt2d set magnification (oldmag)
end proc
