{ PROCEDURE SETCUTAXRAT : sets the axis ratio for cut
proc setcutaxrat value1
  yn = undefined(value1)
  if yn
    print "AXIS RATIOS < 1 give longer X axes ..."
    print "AXIS RATIOS > 1 give longer Y axes ..."
    print "Give the new AXIS RATIO for subsequent cuts ?"
    asknum (value2) "Axis Ratio \1.0\ : "
  else
    value2 = value1
  end if
  send plt2d set cut_axisratio (value2)
end proc

