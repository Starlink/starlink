{ PROCEDURE SETNUMORI : sets the orientation of numbers in around image
proc setnumori
  print "Y axis numbers HORIZONTAL (H) or VERTICAL (V) on axis ?"
  askchoice (value1) "Number Orientation (H,V) \V\ : "
  if value1 = 1
    send plt2d set NUMBER_ORIENT 'NORMAL'
    print "OK, number will be plotted in HORIZONTAL direction ..."
  end if
  if value1 = 2
    send plt2d set NUMBER_ORIENT 'UP'
    print "OK, number will be plotted in VERTICAL direction ..."
  end if
end proc

