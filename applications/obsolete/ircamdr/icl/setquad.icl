{ PROCEDURE SETQUAD : sets a quadrant for an image plot
proc setquad value1
  yn = undefined(value1)
  if yn
    promptfor = 1
  else
    if value1 < 1 or value1 > 5
      promtpfor = 1
    else
      promptfor = 0
    end if
  end if
  if promptfor = 1
    print "Quadrant codes are : "
    print "                     Top Left    = 1, Top Right    = 2"
    print "                     Bottom Left = 3, Bottom Right = 4"
    print "                     Centre      = 5"
    print "In which QUADRANT do you wish to PLOT images ?"
    askchoice (value2) "Quadrant (1,2,3,4,5) \1\ : "
  else
    value2 = value1
  end if
  get plt2d quadmag (quadmag)
  get plt2d max_xsize (quadx)
  get plt2d max_ysize (quady)
  get plt2d worknam (worknam)
  get plt2d worknum (worknum)
  if worknum = 8
    quadx = quadx/2.0
    quady = quady/2.0
  end if
  if value2 = 1
    quadx = quadx/4.0
    quady = quady*3.0/4.0
  else if value2 = 2
    quadx = quadx*3.0/4.0
    quady = quady*3.0/4.0
  else if value2 = 3
    quadx = quadx/4.0
    quady = quady/4.0
  else if value2 = 4
    quadx = quadx*3.0/4.0
    quady = quady/4.0
  else if value2 = 5
    quadx = quadx/2.0
    quady = quady/2.0
  end if
  send plt2d set im_xcen (quadx)
  send plt2d set im_ycen (quady)
  if value2 = 1
    print "O.K. quadrant set to TOP LEFT ..."
  else if value2 = 2
    print "O.K. quadrant set to TOP RIGHT ..."
  else if value2 = 3
    print "O.K. quadrant set to BOTTOM LEFT ..."
  else if value2 = 4
    print "O.K. quadrant set to BOTTOM RIGHT ..."
  else if value2 = 5
    print "O.K. quadrant set to CENTRE SCREEN ..."
  end if
end proc

