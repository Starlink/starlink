{ PROCEDURE PENCOL : sets the colour of a pen
proc pencol value1 st1
  get plt2d image_workstn (image_workstn)
  if image_workstn = 1
    yn = undefined(value1)
    if yn
      print "Give PEN NUMBER (0-255) whose colour is to be SET "
      asknum (value2) "Pen Number \0\ : "
    else
      value2 = value1
    end if
    yn = undefined(st1)
    if yn
      print "Give PEN COLOUR (W R G B Y P C S N) ? "
      askchar (st2) "Pen Colour \N\ : "
    else
      st2 = st1
    end if
    print "Setting PEN " (value2) " to colour " (st2)
    send plt2d set gun_spec 'COLOUR'
    send plt2d set pen_colour (st2)
    obeyw plt2d setcol (value2)
  else
    print "You CANNOT set COLOURS on this workstation"
  end if
end proc

