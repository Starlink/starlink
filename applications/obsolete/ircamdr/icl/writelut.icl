{ PROCEDURE WRITELUT : writes a colour table LUT to workstation
proc writelut value1
  get plt2d image_workstn (image_workstn)
  if image_workstn <> 1
    print "This workstation does NOT support colour graphics ..."
    return
  end if
  yn = undefined(value1)
  if yn
    print "Enter number of the colour table (LUT) required"
    asknum (value2) "Colour table number \19\ : "
    if value2 < 1 or value2 > 100
      print "Error, illegal colour table number entered " (value2)
      return
    end if
  else
    value2 = value1
  end if
  tochar (value2) (st1)
  concat "$LIRCAMDIR/col" (st1) (st2)
  get plt2d worknam (worknam)
  print "Writing colour table " (st2) " to " (worknam)
  send plt2d set last_lut (st2)
  obeyw plt2d coltab (st2)
end proc

