{ PROCEDURE SETCUR : sets where the cursor refers to in CPLOT,CNSIGMA etc
proc setcur
  print "Cursor position referring to :"
  print "  CENTRE (C) of image, "
  print "  BOTTOM LEFT (BL) CORNER of image, "
  print "  TOP RIGHT (TR) CORNER of image."
  askchoice (value1) "Cursor reference (C,BL,TR) \C\ : "
  if value1 = 1
    send plt2d set cursor_where 'CENTRE'
    print "Cursor referenced to CENTRE of images ..."
    cur_pos = 1
  end if
  if value1 = 2
    send plt2d set cursor_where 'BOTTOM_LEFT'
    print "Cursor referenced to BOTTOM LEFT corner of images ..."
    cur_pos = 2
  end if
  if value1 = 3
    send plt2d set cursor_where 'TOP_RIGHT'
    print "Cursor referenced to TOP RIGHT corner of images ..."
    cur_pos = 3
  end if
  send plt2d set cur_pos (cur_pos)
end proc

