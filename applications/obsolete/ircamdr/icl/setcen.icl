{ PROCEDURE SETCEN : sets the centre of the image in a PLOT/NSIGMA
proc setcen value1 value2
  yn1 = undefined(value1)
  yn2 = undefined(value2)
  if yn1 or yn2
    get plt2d workxcen (workxcen)
    get plt2d workycen (workycen)
    print "X,Y centre of current workstation = " (workxcen) "," (workycen)
    print "Give NEW X,Y CENTRE (in raster units) (RETURN=above values) ? "
    asknum (value3) "Image Plot X Centre \-1\ : "
    asknum (value4) "Image Plot Y Centre \-1\ : "
    if value3 = -1
      value3 = workxcen
    end if
    if value4 = -1
      value4 = workycen
    end if
  else
    value3 = value1
    value4 = value2
  end if
  send plt2d set im_xcen (value3)
  send plt2d set im_ycen (value4)
  print "Set CENTRE for image display to " (value3) "," (value4)
end proc

