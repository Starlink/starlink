{ PROCEDURE CONTOFF : sets contour maps pixels offsets
proc contoff value1 value2
  yn = undefined(value1)
  if not yn
    yn = undefined(value2)
  end if
  if yn
    print "Input contour map offset in X (pixels) ?"
    asknum (value3) "X-Offset \0\ : "
    print "Input contour map offset in Y (pixels) ?"
    asknum (value4) "Y-Offset \0\ : "
  else
    value3 = value1
    value4 = value2
  end if
  send plt2d set contour_xoff (value3)
  send plt2d set contour_yoff (value4)
  print "O.K. X,Y contour offsets of " (value3) "," (value4) " set ..."
end proc

