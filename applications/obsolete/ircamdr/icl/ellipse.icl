{ PROCEDURE ELLIPSE : plots a ellipse
proc ellipse value1 value2 value3 value4 value5
  yn1 = undefined(value1)
  yn2 = undefined(value2)
  yn3 = undefined(value3)
  yn4 = undefined(value4)
  yn5 = undefined(value5)
  if yn1
    print "Give ELLIPSE X CENTRE (in pixels) "
    asknum (value6) "ELLIPSE Centre in X \32\ : "
  else
    value6 = value1
  end if
  if yn2
    print "Give ELLIPSE Y CENTRE (in pixels)"
    asknum (value7) "ELLIPSE Centre in Y \32\ : "
  else
    value7 = value2
  end if
  if yn3
    print "Give ELLIPSE MAJOR AXIS (in arcseconds)"
    asknum (value8) "ELLIPSE Major Axis \10\ : "
  else
    value8 = value3
  end if
  if yn4
    print "Give ELLIPSE ECCENTRICITY"
    asknum (value9) "ELLIPSE Eccentricity \0.5\ : "
  else
    value9 = value4
  end if
  if yn5
    print "Give ELLIPSE POSITION ANGLE (degrees)"
    asknum (value10) "ELLIPSE Position Angle \45.0\ : "
  else
    value10 = value5
  end if
  send plt2d set cursor_cross 'NO'
  obeyw plt2d ellipse (value6) (value7) (value8) (value9) (value10)
end proc

