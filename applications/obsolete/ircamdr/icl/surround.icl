{ PROCEDURE SURROUND : put border and ticks/numbers around image
proc surround
  print "O.K. writing BORDER and TICKS/NUMBERS around current image ..."
  obeyw plt2d surround
  obeyw plt2d border 1
  print "SURROUND complete ..."
end proc

