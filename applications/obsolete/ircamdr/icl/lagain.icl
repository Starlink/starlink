{ PROCEDURE LAGAIN : re-plots last line graphics to current workstation...
proc lagain
  get plt2d last_line (last_line)
  if last_line < 1 or last_line > 4
    print "Error, LINE PLOT is not available cause you ain''t done one yet..."
    return
  else if last_line = 1 or last_line = 3
    print "Re-plotting the last 1D CUT ..."
    obeyw plt2d cut
  else if last_line = 2
    print "Re-plotting the last CONTOUR MAP ..."
    obeyw plt2d contour
  else if last_line = 4
    print "Re-plotting the last POLARIZATION VECTOR MAP ..."
    obeyw plt2d polax
  end if
end proc

