{ PROCEDURE SETCUT : sets up a cut plot
proc setcut
  cut_flag = 1
  send plt2d set cut_flag (cut_flag)
  send plt2d set cut_axisratio 1.0
  print "Give the CUT line type ? "
  askchoice (value1) "Plot Type (LINE,BLOCK,HISTOGRAM,MARKER,L,B,H,M) \LINE\ : "
  st1 = 'L'
  if value1 = 1 or value1 = 5
    st1 = 'L'
  else if value1 = 2 or value1 = 6
    st1 = 'B'
   else if value1 = 3 or value1 = 7
    st1 = 'H'
  else if value1 = 4 or value1 = 8
    st1 = 'M'
  end if
  send plt2d set cut_linetype (st1)
  if value1 = 4 or value1 = 8
    print "Give the Marker TYPE :"
    askchoice (value1) "Marker Type (*,+,.,O,X) \*\ : "
    st1 = '*'
    if value1 = 1
      st1 = '*'
    else if value1 = 2
      st1 = '+'
    else if value1 = 3
      st1 = '.'
    else if value1 = 4
      st1 = 'O'
    else if value1 = 5
      st1 = 'X'
    end if
    print "Give the Marker INCREMENT (1-100) : "
    asknum (value2) "Marker Plot Increment \1\ : "
    print "Give the Marker SIZE (1-100) : "
    asknum (value3) "Marker Size \5\ : "
    send plt2d set cut_marktype (st1)
    send plt2d set cut_markinc (value2)
    send plt2d set cut_marksiz (value3)
  end if
  print "Choose a Y-axis CUT SCALING type ? "
  askchoice (value1) "Cut Scaling (MANUAL,AUTOMATIC,M,A) \AUTOMATIC\ : "
  if value1 = 1 or value1 = 3
    send plt2d set cut_scaling 'M'
  else
    send plt2d set cut_scaling 'A'
  end if
  if value1 = 1 or value1 = 3
    print "Give the CUT MAXIMUM Y-axis value ? "
    asknum (value2) "Cut Y Maximum \255\ : "
    print "Give the CUT MINIMUM Y-axis value ? "
    asknum (value3) "Cut Y Minimum \0\ : "
    send plt2d set cut_ymax (value2)
    send plt2d set cut_ymin (value3)
  end if
  print "Give the CUT START PIXEL for X-ticks ? "
  asknum (value1) "Cut X Ticks X Start \0\ : "
  print "Give the CUT INCREMENT for X-ticks (in pixels) ? "
  asknum (value2) "Cut X Ticks Increment \25\ : "
  send plt2d set cut_xticst (value1)
  send plt2d set cut_xticint (value2)
  print "Give Cut TITLE for top of plot ? "
  askchar (st1) "Cut Title \IRCAM IMAGE\ : "
  send plt2d set cut_title (st1)
  print "Do you want the FULL cut annotation or the MINIMUM plot ?"
  askchoice (value1) "Annotation Option (FULL,MINIMUM,F,M) \FULL\ : "
  if value1 = 2 or value1 = 4
    send plt2d set cut_annotation 'MINIMUM'
  else
    send plt2d set cut_annotation 'FULL'
  end if
  print "Give the CUT MAGNIFICATION factor ?"
  asknum (value1) "Cut Magnification \10\ : "
  send plt2d set cut_magnif (value1)
  print "Cut positioned by CURSOR or INPUT POSITION ?"
  askchoice (value2) "Cut Positioning (CURSOR,POSITION,C,P) \POSITION\ : "
  if value2 = 1 or value2 = 3
    send plt2d set cut_positioning 'CURSOR'
  else
    get plt2d workxcen (workxcen)
    get plt2d workycen (workycen)
    print "X,Y centre of current workstation is " (workxcen) "," (workycen)
    print "Give the X,Y CUT centre (RETURN=above values) ?"
    asknum (value2) "Cut X Centre \-1\ : "
    asknum (value3) "Cut Y Centre \-1\ : "
    send plt2d set cut_positioning 'KEY'
    if value2 = -1
      value2 = workxcen
      print "X-centre set to " (value2)
      send plt2d set cut_xcen (value2)
    end if
    if value3 = -1
      value3 = workycen
      print "Y-centre set to " (value3)
      send plt2d set cut_ycen (value3)
    end if
  end if
  print "O.K. have set the CUT definition parameters ..."
end proc

