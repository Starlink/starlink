{ PROCEDURE SETNUMSCA : sets the numbers scale factor for around images
proc setnumsca value1
  yn = undefined(value1)
  if yn
    print "Axis numbers scale factor (0.05 to 10.0, <1 means bigger numbers) ?"
    asknum (value2) "Number scale factor \1.0\ : "
  else
    value2 = value1
  end if
  send plt2d set NUM_SCALE (value2)
end proc

