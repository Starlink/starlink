{ PROCEDURE HARDCOM : comments an image in hardcopy
proc hardcom value6
  yn = undefined(value6)
  if yn
    value6 = 10
  end if
  print "Title for top of Plot ? "
  askchar (st1) "Title \IRCAM IMAGE\ : "
  print "X-axis title ?"
  askchar (st2) "X-axis \R.A. OFFSETS (ARCSECONDS)\ : "
  print "Y-axis title ?"
  askchar (st3) "Y-axis \DEC. OFFSETS (ARCSECONDS)\ : "
  chsiz = 10*(value6)
  print "Current COMMENT size scaling factor = " (chsiz)
  asknum (comscal) "Enter New Value (RETURN to leave unchanged) \-1\ : "
  if comscal <> -1
    chsiz = comscal
  end if
  get plt2d im_xst (imxs)
  get plt2d im_xen (imxe)
  get plt2d im_yst (imys)
  get plt2d im_yen (imye)
  send plt2d set cursor_cross 'NO'
  value1 = imxs+(imxe-imxs)/2.0
  value2 = imys+(imye-imys)/2.0
  test1 = substr(st1,2,1)
  test2 = substr(st2,2,1)
  test3 = substr(st3,2,1)
  if test1 <> "."
    value5 = chsiz*2
    value3 = value1
    value4 = imye+1.5*value5
    send plt2d set comment_orient 'HR'
    obeyw plt2d comment (st1) (value3) (value4) (value5)
  else
    print "No TITLE written ..."
  end if
  if test2 <> "."
    value5 = chsiz
    value3 = value1
    get plt2d jumbo (jumbo)
    value4 = imys-jumbo*value5
    send plt2d set comment_orient 'HR'
    obeyw plt2d comment (st2) (value3) (value4) (value5)
  else
    print "No X-AXIS ANNOTATION written ..."
  end if
  if test3 <> "."
    value5 = chsiz
    value3 = imxs-jumbo*value5
    value4 = value2
    send plt2d set comment_orient 'VR'
    obeyw plt2d comment (st3) (value3) (value4) (value5)
  else
    print "No Y-AXIS ANNOTATION written ..."
  end if
  send plt2d set comment_orient 'HR'
  print "HARDCOM complete..."
end proc

