{ PROCEDURE COLINV : inverts the colour table on subsequent COLTABs
proc colinv
  get plt2d colour_state (colour_state)
  if colour_state <= 1
    colour_state = 2
    send plt2d set colour_state (colour_state)
    send plt2d set ct_direction 'INVERSE'
    print "Colour table now INVERTED in subsequent COLTABs"
  else
    colour_state = 1
    send plt2d set colour_state (colour_state)
    send plt2d set ct_direction 'CORRECT'
    print "Colour table now CORRECT in subsequent COLTABs"
  end if
  get plt2d last_lut (llut)
  obeyw plt2d coltab (llut)
end proc

