{ PROCEDURE SETMAG : sets the magnification for subsequent PLOTS/NSIGMAS
proc setmag value1
  yn = undefined(value1)
  if yn
    get plt2d magnification (value2)
    print "Current image magnification = " (value2)
    print "Give the new image MAGNIFICATION "
    asknum (value1) "Magnification \0\ : "
  end if
  send plt2d set magnification (value1)
  send plt2d set disp_mag (value1)
  value2 = integer(value1/2.0)
  send plt2d set quadmag (value2)
  print "O.K. a MAGNIFICATION of " (value1) " has been set ..."
end proc

