{ PROCEDURE HARDANNOT : hardcopy annotation procedure
proc hardannot value6
  yn = undefined(value6)
  if yn
    value6 = 1
  end if
  magval = value6
  print "Do you want BORDER/TICKS around image ? "
  asklog (brave_man) "Border and Ticks (Yes or No) \N\ : "
  if brave_man = 1
    SURROUND
  end if
  print "Do you want TITLE and AXES COMMENTS ? "
  asklog (brave_man) "Title and Axes Comments (Yes or No) \N\ : "
  if brave_man = 1
    magval2 = magval/2.0
    HARDCOM (magval2)
  end if
  print "Do you want to plot any SYMBOLS on your image ? "
  asklog (brave_man) "Plot Symbols (Yes or No) \N\ : "
  if brave_man = 1
    print "How many of the following do you want to plot ? "
    print "CIRCLES, CROSSES, BOXES, LABELS"
    asknum (numsym) "Number of Symbols \1\ : "
    loop for jj = 1 to (numsym)
      print "Plot CIRCLES, CROSSES, BOXES, LABELS ? "
      askchoice (value1) "Plot What (CI,CR,BO,LA) \CI\ : "
      HARDSYM (value1)
    end loop
  end if
end proc

