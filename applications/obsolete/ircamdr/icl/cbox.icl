{ PROCEDURE CBOX : plots a box centred on cursor
proc cbox value1 value2 value3
  get plt2d cursor_workstn (cursor_workstn)
  if cursor_workstn = 1
    yn = undefined(value3)
    if yn
      print "BOX position on CENTRE of BOX or BOTTOM LEFT corner ?"
      askchoice (box_pos) "Box Positioning (CENTRE,BL,C,B) \CENTRE\ : "
    else
      box_pos = value3
    end if
    yn1 = undefined(value1)
    yn2 = undefined(value2)
    if yn1 or yn2
      print "Give the BOX X,Y SIZE (in arcseconds) ? "
      asknum (value4) "Box X Size \5\ : "
      asknum (value5) "Box Y Size \5\ : "
    else
      value4 = value1
      value5 = value2
    end if
    if box_pos = 2 or box_pos = 4
      print "Select BOTTOM LEFT CORNER of BOX with CURSOR ..."
      send plt2d set cursor_cross 'NO'
      obeyw plt2d curbox (value4) (value5) 'BOTTOM_LEFT'
    else
      print "Select CENTRE of BOX with CURSOR ..."
      send plt2d set cursor_cross 'NO'
      obeyw plt2d curbox (value4) (value5) 'CENTRE'
    end if
  else
    print "You CANNOT use a CURSOR on this device"
    print "Try routine BOX instead"
  end if
end proc

