{ PROCEDURE WRCOM : writes a comment on current workstation
proc wrcom
  print "Give the COMMENT to be written ? "
  askchar (comstring) "Comment \IRCAM\ : "
  get plt2d workxcen (workxcen)
  get plt2d workycen (workycen)
  print "X,Y centre of current workstation = " (workxcen) "," (workycen)
  print "Give the X,Y POSITION of the comment ? "
  asknum (comxcen) "Comment X Position (RETURN=CENTRE) \-1\ : "
  asknum (comycen) "Comment Y Position (RETURN=CENTRE) \-1\ : "
  if comxcen = -1
    comxcen = workxcen
  end if
  if comycen = -1
    comycen = workycen
  end if
  print "Give the COMMENT SIZE ? "
  asknum (comsize) "Comment Size \15\ : "
  print "Do you want to change the COMMENT ORIENTATION and/or FONT ?"
  asklog (brave_man) "Change Orientation,Font (Yes or No) \N\ : "
  if brave_man = 1
    SETCOMORI
    SETFONT
  end if
  send plt2d set cursor_cross 'NO'
  obeyw plt2d comment (comstring) (comxcen) (comycen) (comsize)
end proc

