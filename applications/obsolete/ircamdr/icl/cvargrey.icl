{ PROCEDURE CVARGREY : plots an image with vargrey between max,min with cursor
proc cvargrey num_obsele code_image value1 value2
  get plt2d image_workstn (image_workstn)
  get plt2d cursor_workstn (cursor_workstn)
  get plt2d name_image (name_image)
  get plt2d worknam (worknam)
  if image_workstn = 1 and cursor_workstn = 1
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
    name_out = " "
    get plt2d name_image (last_image)
    get_imagename (num_obsele2) (code_image2) (name_out) (last_image)
    name_image = name_out
    last_plot = 10
    send plt2d set last_plot (last_plot)
    send plt2d set cursor_image (name_image)
    yn = undefined(value1)
    send plt2d set name_image (name_image)
    if not yn
      print "Setting percentage X range to " (value1)
      send plt2d set vargrey_xpc (value1)
    end if
    yn = undefined(value2)
    if not yn
      print "Setting percentage Y range to " (value2)
      send plt2d set vargrey_ypc (value2)
    end if
    print "CVARGREY " (name_image) " on " (worknam)
    get plt2d cur_pos (cur_pos)
    if cur_pos <= 1 or cur_pos > 3
      print "Selected CENTRE of image with CURSOR ..."
    end if
    if cur_pos = 2
      print "Selected BOTTOM LEFT corner of image with CURSOR ..."
    end if
    if cur_pos = 3
      print "Selected TOP RIGHT corner of image with CURSOR ..."
    end if
    obeyw plt2d curvargrey (name_image)
  else
    print "You either CANNOT plot IMAGES on this workstation - " (worknam)
    print "    or you CANNOT use the CURSOR on it ..."
  end if
end proc

