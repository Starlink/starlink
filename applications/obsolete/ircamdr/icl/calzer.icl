{ PROCEDURE CALZER : calculates zeropoint from number and magnitude
proc calzer value1 value2
  yn1 = undefined(value1)
  yn2 = undefined(value2)
  if yn1
    print "Enter MAGNITUDE of object : "
    asknum (v1) "Magnitude \0.0\ ? "
  else
    v1 = value1
  end if
  if yn2
    print "Enter INTENSITY in aperture (DN/SECOND) : "
    asknum (v2) "Intensity \0.0\ ? "
  else
    v2 = value2
  end if
  print "Zeropoint = " ((v1)+(2.5*log(v2)/log(10)))
end proc

