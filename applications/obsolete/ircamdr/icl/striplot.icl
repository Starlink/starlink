{ PROCEDURE STRIPLOT : plots the stripchart output files
proc striplot value1
  yn = undefined(value1)
  if yn
    fpplot = 1
  end if
  if fpplot = 1
    print "Enter the NAME of the FP STRIPCHART output file to be plotted :"
    askname (scfile) "FP STRIPCHART Filename : "
  else
    print "Enter the NAME of the STRIPCHART output file to be plotted :"
    askname (scfile) "STRIPCHART Filename : "
  end if
  TRACE (scfile) N
  print "Input the NUMBER of ELEMENTS plotted (see NUMBER_ELEMENTS) :"
  asknum (stnum) "Number Elements \10\ : "
  if fpplot <> 1
    print "Give the RANGE (on the MEAN of the STRIP) for plotting the MEANs :"
    asknum (stranm) "Means Plot Range \100\ : "
    print "Give the RANGE (on the MEAN of the STRIP) for plotting the STDs :"
    asknum (strans) "Stds Plot Range \100\ : "
  end if
  print "Do you want to CLEAR the display screen before plotting ?"
  asklog (brave_man) "Clear Screen \Y\ : "
  if brave_man = 1
    obeyw plt2d clear
  end if
  print "Now plotting MEAN ODD channel from " (scfile)
  get plt2d max_xsize (wmaxx)
  get plt2d max_ysize (wmaxy)
  send plt2d set cut_axisratio 0.8
  get plt2d worknum (worknum)
  if worknum = 8
    send plt2d set cut_magnif 3
  end if
  if worknum = 1
    send plt2d set cut_magnif 6
  else
    send plt2d set cut_magnif 7
  end if
  if fpplot = 1
    value5 = wmaxx/4.0
    value6 = wmaxy/2.0
  else
    value5 = wmaxx/4.0
    value6 = wmaxy*3.0/4.0
  end if
  if fpplot = 1
    send plt2d set cut_title 'FP STRIPCHART - Mean ODD'
    send plt2d set cut_scaling 'A'
  else
    send plt2d set cut_title 'STRIPCHART - Mean ODD'
    send plt2d set cut_scaling 'R'
    send plt2d set cut_range (stranm)
  end if
  send plt2d set cut_positioning 'KEY'
  send plt2d set cut_xcen (value5)
  send plt2d set cut_ycen (value6)
  obeyw plt2d cut (scfile) 1 1 (stnum) 1
  print "Now plotting MEAN EVEN channel from " (scfile)
  if fpplot = 1
    value5 = wmaxx*3.0/4.0
    value6 = wmaxy/2.0
  else
    value5 = wmaxx*3.0/4.0
    value6 = wmaxy*3.0/4.0
  end if
  if fpplot = 1
    send plt2d set cut_title 'FP STRIPCHART - Mean EVEN'
  else
    send plt2d set cut_title 'STRIPCHART - Mean EVEN'
  end if
  send plt2d set cut_positioning 'KEY'
  send plt2d set cut_xcen (value5)
  send plt2d set cut_ycen (value6)
  obeyw plt2d cut (scfile) 1 2 (stnum) 2
  if fpplot <> 1
    print "Now plotting STD ODD channel from STRIPCHART " (scfile)
    value5 = wmaxx/4.0
    value6 = wmaxy/4.0
    send plt2d set cut_positioning 'KEY'
    send plt2d set cut_xcen (value5)
    send plt2d set cut_ycen (value6)
    send plt2d set cut_title 'STRIPCHART - Std ODD'
    send plt2d set cut_range (strans)
    obeyw plt2d cut (scfile) 1 3 (stnum) 3
    print "Now plotting STD EVEN channel from STRIPCHART " (scfile)
    value5 = wmaxx*3.0/4.0
    value6 = wmaxy/4.0
    send plt2d set cut_positioning 'KEY'
    send plt2d set cut_xcen (value5)
    send plt2d set cut_ycen (value6)
    send plt2d set cut_title 'STRIPCHART - Std EVEN'
    obeyw plt2d cut (scfile) 1 4 (stnum) 4
   end if
end proc

