{ PROCEDURE BORDER : plots a border around last image
proc border value1
  yn = undefined(value1)
  if yn
    print "Give the BORDER width (number) ? "
    asknum (value2) "Border Width \2\ : "
  else
    value2 = value1
  end if
  obeyw plt2d border (value2)
end proc

