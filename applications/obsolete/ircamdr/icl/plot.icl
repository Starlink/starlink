{ PROCEDURE PLOT : plots an image between maximum, minimum values
proc plot num_obsele code_image val1 val2
  get plt2d image_workstn (image_workstn)
  get plt2d worknam (worknam)
  get plt2d filetype (filetype)
  if image_workstn <> 1
    print "You CANNOT plot IMAGES on this workstation - " (worknam)
    return
  end if
  yn = undefined(num_obsele)
  yn2 = undefined(code_image)
  yn3 = undefined(val1)
  yn4 = undefined(val2)
  if yn
    num_obsele2 = -999
    code_image2 = -999
    value1 = -999
    value2 = -999
  else
    num_obsele2 = num_obsele
    if yn2
      code_image2 = -999
      value1 = -999
      value2 = -999
    else
      code_image2 = code_image
      if yn3
        value1 = -999
        value2 = -999
      else
        value1 = val1
        if yn4
          value2 = -999
        else
          value2 = val2
        end if
      end if
    end if
  end if
  name_out = " "
  get plt2d name_image (last_image)
  get_imagename (num_obsele2) (code_image2) (name_out) (last_image)
  name_image = name_out
  last_plot = 1
  send plt2d set last_plot (last_plot)
  send plt2d set cursor_image (name_image)
  send plt2d set name_image (name_image)
  yn = undefined(val1)
  if not yn
    print "Setting MINIMUM to " (val1)
    send plt2d set minimum (val1)
  end if
  yn = undefined(val2)
  if not yn
    print "Setting MAXIMUM to " (val2)
    send plt2d set maximum (val2)
  end if
  print "PLOT " (name_image) " on " (worknam)
  obeyw plt2d plot (name_image)
end proc

