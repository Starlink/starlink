{ PROCEDURE BOX : plots a box
proc box value1 value2 value3 value4 value5
  yn1 = undefined(value1)
  yn2 = undefined(value2)
  yn3 = undefined(value3)
  yn4 = undefined(value4)
  yn5 = undefined(value5)
  if yn5
    print "BOX position on CENTRE of BOX or BOTTOM LEFT corner ?"
    askchoice (box_pos) "Box Positioning (CENTRE,BL,C,B) \CENTRE\ : "
  else
    if value5 < 1 or value5 > 4
      box_pos = 1
    else
      box_pos = value5
    end if
  end if
  if yn1 or yn2
    print "Give the BOX X,Y POSITION (in pixels) ? "
    asknum (value6) "Box X Pixel Position \1\ : "
    asknum (value7) "Box Y Pixel Position \1\ : "
  else
    value6 = value1
    value7 = value2
  end if
  if yn3 or yn4
    print "Give the BOX X,Y SIZE (in arcseconds) ? "
    asknum (value8) "Box X Size \5\ : "
    asknum (value9) "Box Y Size \5\ : "
  else
    value8 = value3
    value9 = value4
  end if
  if box_pos = 2 or box_pos = 4
    send plt2d set cursor_cross 'NO'
    obeyw plt2d box (value6) (value7) (value8) (value9) 'BOTTOM_LEFT'
  else
    send plt2d set cursor_cross 'NO'
    obeyw plt2d box (value6) (value7) (value8) (value9) 'CENTRE'
  end if
end proc

