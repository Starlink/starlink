{ PROCEDURE SETRADEC : sets ra/dec for contour maps and surround }
proc setradec
  print "Give the RA and DEC coordinates of the ZERO points :"
  askra (value1) "RA Coordinate (e.g. 12,30,25.3) : "
  askdec (value2) "Dec Coordinate (e.g. 40,25,26.6) : "
  send plt2d set ra_zero (value1)
  send plt2d set dec_zero (value2)
end proc

