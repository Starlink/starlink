{ PROCEDURE SETMM : sets the maximum and minimum for image scaling in a PLOT
proc setmm value2 value1
  yn1 = undefined(value2)
  yn2 = undefined(value1)
  if yn1
    get plt2d minimum (min)
    get plt2d maximum (max)
    print "Specified current plot MINIMUM = " (min)
    print "                       MAXIMUM = " (max)
    print "Give new plot MINIMUM and MAXIMUM value : "
    asknum (val2) "New Minimum \0\ ? "
    asknum (val1) "New Maximum \0\ ? "
  else
    val2 = value2
    if yn2
      get plt2d maximum (max)
      print "Specified current plot MAXIMUM = " (max)
      print "Give new plot MAXIMUM value : "
      asknum (val1) "New Maximum \0\ ? "
    else
      val1 = value1
    end if
  end if
  send plt2d set maximum (val1)
  send plt2d set minimum (val2)
end proc

