{ PROCEDURE SETCONT : sets up the contour plot
proc setcont
  cont_flag = 1
  send plt2d set cont_flag (cont_flag)
  print "Give the type of CONTOUR level selection required "
  print "Contour Level Selection : Input BASE level and INTERVAL       = A"
  print "                          Take UNIFORM levels between MAX,MIN = B"
  print "                          Input a selection of CONTOUR LEVELS = C"
  askchoice (ctype) "Contour Level Type (A,B,C) \A\ : "
  if ctype = 1
    print "O.K. CONTOUR selection is via a BASE level and INTERVAL ..."
    print "Give the NUMBER of CONTOURS plotted ? "
    asknum (value1) "Number of Contours \5\ : "
    print "Give the BASE CONTOUR level ? "
    asknum (value2) "Base Contour \20\ : "
    print "Give the CONTOUR INTERVAL ? "
    asknum (value3) "Contour Interval \50\ : "
    send plt2d set contour_number (value1)
    send plt2d set contour_base (value2)
    send plt2d set contour_step (value3)
    print "Give CONTOURING COLOUR type :"
    print "  (ALL=all same colour,SINGLE=each different colour,TWO=two colour)"
    askchoice (value1) "Contour Type (ALL,SINGLE,TWO,A,S,T) \ALL\ : "
    if value1 = 1 or value1 = 4
      type = 'A'
    end if
    if value1 = 2 or value1 = 5
      type = 'S'
    end if
    if value1 = 3 or value1 = 6
      type = 'T'
      print "Give value for colour split level :"
      asknum (consplit) "Colour Split Level \0\ : "
      send plt2d set contour_split (consplit)
    end if
    send plt2d set contour_auto 'MANUAL'
    send plt2d set contour_type (type)
  end if
  if ctype = 2
    print "O.K. CONTOUR selection is via UNIFORM levels between MAX and MIN ..."
    print "Give the NUMBER of CONTOURS plotted ? "
    asknum (value1) "Number of Contours \5\ : "
    send plt2d set contour_number (value1)
    print "Give CONTOURING COLOUR type :"
    print "  (ALL=all same colour,SINGLE=each different colour,TWO=two colour)"
    askchoice (value1) "Contour Type (ALL,SINGLE,TWO,A,S,T) \ALL\ : "
    if value1 = 1 or value1 = 4
      type = 'A'
    end if
    if value1 = 2 or value1 = 5
      type = 'S'
    end if
    if value1 = 3 or value1 = 6
      type = 'T'
      print "Give value for colour split level :"
      asknum (consplit) "Colour Split Level \0\ : "
      send plt2d set contour_split (consplit)
    end if
    send plt2d set contour_auto 'AUTO'
    send plt2d set contour_type (type)
  end if
  if ctype = 3
    print "O.K. CONTOUR selection is via CHOSEN levels ..."
    print "Give a LIST of LEVELS for the CONTOURS "
    print "e.g. 10.0,20.0,35.0,50.0,150.0,200.0,1000.0,5000.0 "
    askchar (st1) "Contours Levels \1,2,3,4,5\ : "
    send plt2d set contour_levels (st1)
    send plt2d set contour_type 'C'
    send plt2d set contour_auto 'MANUAL'
  end if
  print "Do you want FULL ANNOTATION on the CONTOUR MAP ?"
  asklog (value1) "Annotation \Y\ : "
  if value1 = 1
    send plt2d set contour_annotat 'ANNOTATION'
    print "Give a TITLE for the top of the CONTOUR MAP : "
    askchar (st3) "Title \IRCAM Image\ : "
    send plt2d set contour_title (st3)
    SETNUM
  else
    send plt2d set contour_annotat 'NO_ANNOTATION'
  end if
  print "Give the MAGNIFICATION FACTOR for the CONTOUR MAP on the display ? "
  asknum (value1) "Contour Magnification \0\ : "
  send plt2d set contour_magnif (value1)
  print "Contour map positioned on display by CURSOR or an INPUT POSITION ?"
  askchoice (value3) "Contour Positioning (CURSOR,POSITION,C,P) \P\ : "
  if value3 = 1 or value3 = 3
    send plt2d set contour_posn 'CURSOR'
  else
    get plt2d workxcen (workxcen)
    get plt2d workycen (workycen)
    print "X,Y centre of current workstation is " (workxcen) "," (workycen)
    print "Give the CONTOUR MAP X and Y centre (RETURN=above values) ?"
    asknum (value2) "Contour X Centre \-1\ : "
    asknum (value3) "Contour Y Centre \-1\ : "
    if value2 = -1
      value2 = workxcen
      print "X-centre set to " (value2)
    end if
    if value3 = -1
      value3 = workycen
      print "Y-centre set to " (value3)
    end if
    send plt2d set contour_posn 'KEY'
    send plt2d set contour_xcen (value2)
    send plt2d set contour_ycen (value3)
  end if
end proc

