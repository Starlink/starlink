{ PROCEDURE LINE_WIDTH : sets line width of subsequent plots
proc line_width value1
  yn = undefined(value1)
  if yn
    print "Input line width scaling factor (1-10) ? "
    asknum (value2) "Line Width \3\ : "
  else
    value2 = value1
  end if
  obeyw plt2d line_width (value2)
  print "A line width scaling factor of " (value2) " has been set"
end proc

