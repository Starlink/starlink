{ PROCEDURE SETCOMORI : changes the character orientation
proc setcomori value1
  yn = undefined(value1)
  if yn
    print "COMMENT ORIENTATIONS are : Horizontal_Right= HR"
    print "                           Horizontal-Left = HL"
    print "                           Vertical-Right  = VR"
    print "                           Vertical-Left   = VL"
    askchoice (value2) "Give Orientation (HR,HL,VR,VL) \HR\ : "
  else
    value2 = value1
  end if
  if value2 = 1
    st1 = "HR"
  else if value2 = 2
    st1 = "HL"
  else if value2 = 3
    st1 = "VR"
  else if value2 = 4
    st1 = "VL"
  end if
  send plt2d set comment_orient (st1)
end proc

