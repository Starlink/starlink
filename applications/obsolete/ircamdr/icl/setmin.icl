{ PROCEDURE SETMIN : sets the minimum for image scaling in a PLOT
proc setmin value1
  yn = undefined(value1)
  if yn
    get plt2d minimum (value2)
    print "Current MINIMUM plot value was " (value2)
    print "Give new MINIMUM plot value ? "
    askname (st2) "Image Plot Minimum (RETURN=LEAVE UNCHANGED) \SAME\ : "
    if st2 = "SAME"
    else
      value2 = real(st2)
    end if
  else
    value2 = value1
  end if
  send plt2d set minimum (value2)
  print "O.K. have set MINIMUM plot value to " (value2)
end proc

