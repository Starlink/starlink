{ PROCEDURE CONTOUR : plots a contour image
proc contour num_obsele code_image
  get plt2d cont_flag (cont_flag)
  get plt2d worknam (worknam)
  get plt2d worknum (worknum)
  if cont_flag < 1 or cont_flag > 1
    print "You must run SETCONT before you can plot a contour map"
    return
  end if
  get plt2d name_image (name_image)
  oldnam = name_image
  yn = undefined(num_obsele)
  yn2 = undefined(code_image)
  if yn
    num_obsele2 = -999
    code_image2 = -999
  else
    num_obsele2 = num_obsele
    if yn2
      code_image2 = -999
    else
      code_image2 = code_image
    end if
  end if
  get plt2d name_image (last_image)
  get_imagename (num_obsele2) (code_image2) (name_out) (last_image)
  name_image = name_out
  get plt2d contour_posn (using_cursor)
  send plt2d set contour_image (name_image)
  if worknum = 3 or worknum = 4
    obeyw plt2d clear
  end if
  send plt2d set cursor_image (name_image)
  send plt2d set cursor_cross 'NO'
  get plt2d tekline (tekline)
  if worknum = 3 or worknum = 4
    compare (tekline) "LOCAL" (comval)
    if comval = 1
      obeyw plt2d contour (name_image)
      waitcr
    else
      print "CONTOUR " (name_image) " on " (worknam)
      obeyw plt2d contour (name_image)
    end if
  else
    print "CONTOUR " (name_image) " on " (worknam)
    if using_cursor = "CURSOR"
      obeyw plt2d contour (name_image)
    else
      obeyw plt2d contour (name_image)
    end if
  end if
  last_line = 2
  send plt2d set last_line (last_line)
  name_image = oldnam
  send plt2d set name_image (name_image)
end proc

