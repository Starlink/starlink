{ PROCEDURE CUT2FF : enables/disables writing of cut to ff file
proc cut2ff opt fnam
  yn1 = undefined(opt)
  yn2 = undefined(fnam)
  if yn1
    print "Do you the CUT X and Y values written to a FREE-FORMAT file ?"
    asklog (value1) "Free-Format file Output (Yes or No) \N\ : "
  else
    value1 = opt
  end if
  if yn2
    print "Give name for output ascii file to contain cut data :"
    askname (st1) "Ascii filename \plt2dcut.dat\ ? "
  else
    st1 = fnam
  end if
  if value1 = 1
    send plt2d set ff_file 'YES'
    print "Cut output option has been enabled ..."
    print "Cuts will be written to ascii file " (st1)
  else
    send plt2d set ff_file 'NO'
    print "Cut output option has been disabled ..."
  end if
end proc

