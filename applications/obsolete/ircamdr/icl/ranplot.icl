{ PROCEDURE RANPLOT : plots an image between user defined range
proc ranplot num_obsele code_image value1
  get plt2d image_workstn (image_workstn)
  get plt2d cursor_workstn (cursor_workstn)
  get plt2d name_image (name_image)
  get plt2d worknam (worknam)
  if image_workstn = 1
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
    yn = undefined(value1)
    if yn
      print "Give the RANGE VALUE of the plot (>0) :"
      asknum (range) "Plot Range \256\ : "
    else
      range = value1
    end if
    last_plot = 3
    send plt2d set last_plot (last_plot)
    send plt2d set cursor_image (name_image)
    send plt2d set name_image (name_image)
    print "RANPLOT " (name_image) " on " (worknam)
    obeyw plt2d ranplot (name_image) (range)
  else
    print "You CANNOT plot IMAGES on this workstation - " (worknam)
  end if
end proc

