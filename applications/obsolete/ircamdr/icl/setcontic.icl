{ PROCEDURE SETCONTIC : sets contour maps tick mark extent
proc setcontic st1
  yn = undefined(st1)
  if yn
    print "Do you want SMALL tick marks of a FULL grid ?"
    askchoice (value1) "Small or Full Grid (S,F) \S\ : "
  else
    if st1 = "F" or st1 = "FULL"
      value1 = 2
    else
      value1 = 1
    end if
  end if
  if value1 = 1
    send plt2d set contour_ticklen 'SMALL'
  else
    send plt2d set contour_ticklen 'FULL'
  end if
end proc

