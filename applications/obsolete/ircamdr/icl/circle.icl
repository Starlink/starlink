{ PROCEDURE CIRCLE : plots a circle
proc circle value1 value2 value3
  yn1 = undefined(value1)
  yn2 = undefined(value2)
  yn3 = undefined(value3)
  if yn1
    print "Give CIRCLE X CENTRE (in pixels) ? "
    asknum (value4) "Circle Centre in X \32\ : "
  else
    value4 = value1
  end if
  if yn2
    print "Give CIRCLE Y CENTRE (in pixels) ? "
    asknum (value5) "Circle Centre in Y \32\ : "
  else
    value5 = value2
  end if
  if yn3
    print "Give CIRCLE DIAMETER (in arcseconds) ? "
    asknum (value6) "Circle Diameter \10\ : "
  else
    value6 = value3
  end if
  send plt2d set cursor_cross 'NO'
  obeyw plt2d circle (value4) (value5) (value6)
end proc

