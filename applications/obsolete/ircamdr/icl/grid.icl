{ PROCEDURE GRID : plots a grid on an image
proc grid value1 value2
  yn = undefined(value1)
  if not yn
    yn = undefined(value2)
  end if
  if yn
    print "Give the X,Y GRID increments (in arcseconds) ? "
    asknum (value3) "X Increment \5\ : "
    asknum (value4) "Y Increment \5\ : "
  else
    value3 = value1
    value4 = value2
  end if
  obeyw plt2d grid (value3) (value4)
end proc

