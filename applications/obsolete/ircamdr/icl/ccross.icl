{ PROCEDURE CCROSS : plots a cross at a cursor position
proc ccross value1
  get plt2d cursor_workstn (cursor_workstn)
  if cursor_workstn = 1
    yn = undefined(value1)
    if yn
      print "Define CROSS size in pixels"
      asknum (value2) "Cross Size \5\ : "
    else
      value2 = value1
    end if
    print "Select CENTRE of CROSS with CURSOR ..."
    obeyw plt2d curcro (value2)
  else
    print "You CANNOT plot CURSOR on this workstation"
    print "Try routine CROSS instead"
  end if
end proc

