{ PROCEDURE VEC : plots a polarization vector plot on current workstation
proc vec
  print "Use Q,U images or P,Theta images for plot ?"
  askchoice (value1) "Input Type (QU,PT) \PT\ : "
  if value1 = 1
    send plt2d set quorpt 'QU'
    print "Give the NAMES of the Q and U images :"
    askname (st1) "Q Image Name \qimage\ : "
    askname (st2) "U Image Name \uimage\ : "
  else
    send plt2d set quorpt 'PT'
    print "Give the NAMES of the P and Theta images :"
    askname (st1) "P Image Name     \pimage\ : "
    askname (st2) "Theta Image Name \timage\ : "
  end if
  send plt2d set inpicqp (st1)
  send plt2d set inpicut (st2)
  send plt2d set cursor_image (st1)
  get plt2d pol_positioning (using_cursor)
  compare (using_cursor) 'CURSOR' (brave_man)
  print "Attempting to plot your POLARIZATION VECTOR MAP ..."
  get plt2d worknum (worknum)
  last_line = 4
  send plt2d set last_line (last_line)
  if worknum = 3 or worknum = 4
    obeyw plt2d clear
    compare tekline "LOCAL" comval
    if comval = 1
      obeyw plt2d polax
      waitcr
    else
      obeyw plt2d polax
    end if
  else
    if brave_man = 1
      obeyw plt2d polax
    else
      obeyw plt2d polax
    end if
  end if
end proc

