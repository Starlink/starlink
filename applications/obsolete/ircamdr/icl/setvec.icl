{ PROCEDURE SETVEC : sets up a polarization vector map on current workstation
proc setvec
  get plt2d magnification (value1)
  print "Current IMAGE magnification = "  (value1)
  print "Give the MAGNIFICATION for the vector plot :"
  asknum (value1) "Magnification \0\ : "
  send plt2d set polmag (value1)
  print "Give the POSITION ANGLE CORRECTION value :"
  asknum (value1) "Theta Correction \0.0\ : "
  send plt2d set polpos (value1)
  print "Give the VECTOR DENSITY FACTOR :"
  asknum (value1) "Vector Density \1\ : "
  send plt2d set polden (value1)
  print "Give the LENGTH of the 100% polarization vector :"
  asknum (value1) "100% Length \10\ : "
  send plt2d set polvec (value1)
  print "Do you want ANNOTATION around your vector plot ?"
  asklog (value1) "Annotation (Yes or No) \Y\ : "
  if value1 = 1
    send plt2d set polann 'ANNOTATE'
    print "Give a TITLE for the vector plot :"
    askchar (st1) "Title \JUNK\ : "
    send plt2d set poltitle (st1)
  else
    send plt2d set polann 'NO_ANNOTATION'
  end if
  print "Do you want to POSITION the vector plot using the CURSOR ?"
  asklog (value1) "Cursor Positioning (Yes or No) \N\ : "
  if value1 = 1
    send plt2d set pol_positioning 'CURSOR'
  else
    send plt2d set pol_positioning 'KEY'
    get plt2d workxcen (workxcen)
    get plt2d workycen (workycen)
    print "X,Y centre of current workstation is " (workxcen) "," (workycen)
    print "Give X,Y CENTRE for the vector plot (RETURN=above values) :"
    asknum (value1) "X-Centre \-1\ : "
    asknum (value2) "Y-Centre \-1\ : "
    if value1 = -1
      value1 = workxcen
      print "X-centre set to " (value1)
    end if
    if value2 = -1
      value2 = workycen
      print "Y-centre set to " (value2)
    end if
    send plt2d set pol_xcen (value1)
    send plt2d set pol_ycen (value2)
  end if
  print "Do you want the VECTORS all SAME colour or DIFFERENT colours ?"
  askchoice (value1) "Same colour of Different (S,D) \S\ ? "
  if value1 = 1
    send plt2d set pol_coltype 'SAME'
  else
    send plt2d set pol_coltype 'DIFF'
    print "Do you want to CHOOSE the COLOUR RANGES ?"
    asklog (value2) "Choose Ranges (Yes or No) \N\ : "
    if value2 = 1
      print "FIVE colours are available for polarization vectors, W,R,G,B,Y."
      print "You need to define 5 ranges of polarization, one for each colour."
      print "The ranges are defined by a START value of polarization and an"
      print "INTERVAL in polarization. Thus, a start value of 0.0 and an "
      print "interval of 15.0 would give polarizations in the range 0-15% the"
      print "colour W, polarization 15-30% a colour R, 30-45% a colour G "
      print "45-60% a colour B and 60-75% a colour Y."
      print "Polarizations below the START and above the maximum are "
      print "NOT plotted ..."
      print "Give the POLARIZATION range START value :"
      asknum (value3) "Range Start \0.0\ : "
      print "Give the POLARIZATION range INTERVAL value :"
      asknum (value4) "Range Interval \20.0\ : "
      send plt2d set pol_colch 'CHOOSE'
      send plt2d set pol_colst (value3)
      send plt2d set pol_colint (value4)
    else
      send plt2d set pol_colch 'STANDARD'
    end if
  end if
end proc

