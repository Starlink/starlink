{ PROCEDURE CELLIPSE : plots a ELLIPSE at cursor
proc cellipse value1 value2 value3
  get plt2d cursor_workstn (cursor_workstn)
  if cursor_workstn = 1
    yn1 = undefined(value1)
    yn2 = undefined(value2)
    yn3 = undefined(value3)
    if yn1
      print "Give the ELLIPSE MAJOR AXIS (in arcseconds),"
      asknum (value4) "ELLIPSE Major Axis \10\ : "
    else
      value4 = value1
    end if
    if yn2
      print "Give the ELLIPSE ECCENTRICITY "
      asknum (value5) "ELLIPSE Eccentricity \0.5\ : "
    else
      value5 = value2
    end if
    if yn3
      print "Give the ELLIPSE POSITION ANGLE "
      asknum (value6) "ELLIPSE Position Angle \45.0\ : "
    else
      value6 = value3
    end if
    print "Select CENTRE of ELLIPSE with CURSOR ..."
    send plt2d set cursor_cross 'NO'
    obeyw plt2d CURELLIPSE (value4) (value5) (value6)
  else
    print "You CANNOT use a CURSOR on this workstation"
    print "Try routine ELLIPSE instead"
  end if
end proc

