{ PROCEDURE CLEARIT : clears a section of the image display screen
proc clearit
  send plt2d set CURSOR_CROSS 'NO'
  print "Select the BOTTOM LEFT corner of area cleared ..."
  obeyw plt2d curpos
  get plt2d x_cur_real (val1)
  get plt2d y_cur_real (val2)
  print "Select the TOP RIGHT corner of area cleared ..."
  obeyw plt2d curpos
  get plt2d x_cur_real (val3)
  get plt2d y_cur_real (val4)
  val1 = real(val1)
  val2 = real(val2)
  val3 = real(val3)
  val4 = real(val4)
  send plt2d set im_xst (val1)
  send plt2d set im_yst (val2)
  send plt2d set im_xen (val3)
  send plt2d set im_yen (val4)
  obeyw plt2d clearit
end proc

