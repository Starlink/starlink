{ PROCEDURE CROSS : plots a cross
proc cross value1 value2 value3
  yn1 = undefined(value1)
  yn2 = undefined(value2)
  yn3 = undefined(value3)
  if yn1 or yn2 or yn3
    print "Give the CROSS X,Y POSITION and SIZE (in arcseconds) ? "
    asknum (value4) "Cross X Pixel Position \32\ : "
    asknum (value5) "Cross Y Pixel Position \32\ : "
    asknum (value6) "Cross Size \10\ : "
  else
    value4 = value1
    value5 = value2
    value6 = value3
  end if
  obeyw plt2d cross (value4) (value5) (value6)
end proc

