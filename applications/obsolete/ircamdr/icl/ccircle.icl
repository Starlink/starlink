{ PROCEDURE CCIRCLE : plots a circle at cursor
proc ccircle value1
  get plt2d cursor_workstn (cursor_workstn)
  if cursor_workstn = 1
    yn = undefined(value1)
    if yn
      print "Give the CIRCLE DIAMETER (in arcseconds) ? "
      asknum (value2) "Circle Diameter \5\ : "
    else
      value2 = value1
    end if
    print "Select CENTRE of CIRCLE with CURSOR ..."
    send plt2d set cursor_cross 'NO'
    obeyw plt2d curcir (value2)
  else
    print "You CANNOT use a CURSOR on this workstation"
    print "Try routine CIRCLE instead"
  end if
end proc

