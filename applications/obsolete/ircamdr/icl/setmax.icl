{ PROCEDURE SETMAX : sets the maximum for image scaling in a PLOT
proc setmax value1
  yn = undefined(value1)
  if yn
    get plt2d maximum (value2)
    print "MAXIMUM plot value was " (value2)
    print "Give new MAXIMUM plot value ? "
    askname (st1) "Image Plot Maximum (RETURN=LEAVE UNCHANGED) \SAME\ : "
    if st1 = "SAME"
    else
      value2 = real(st1)
    end if
  else
    value2 = value1
  end if
  send plt2d set maximum (value2)
  print "O.K. have set MAXIMUM plot value to " (value2)
end proc

