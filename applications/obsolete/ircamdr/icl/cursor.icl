{ PROCEDURE CURSOR : displays the cursor and returns X,Y, value
proc cursor
  get plt2d cursor_workstn (cursor_workstn)
  if cursor_workstn = 1
    get plt2d name_image (im)
    print "CURSOR displayed for image " (im)
    obeyw plt2d cursor
    get plt2d x_cur_pixel (value1)
    get plt2d y_cur_pixel (value2)
    get plt2d cursor_value (value3)
    get plt2d x_cur_real (value4)
    get plt2d y_cur_real (value5)
    print "Cursor PIXEL = " (value1) "," (value2)
    print "Pixel VALUE  = " (value3)
    print "Device coord = " (value4) "," (value5)
  else
    print "You CANNOT display a CURSOR on this workstation"
  end if
end proc

