{ PROCEDURE CNSIGMA : plots image with range N-sigma on mean at cursor posn
proc cnsigma num_obsele code_image value1
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
    yn = undefined(value1)
    if yn
      promptfor = 1
    else
      promptfor = 0
    end if
    if promptfor = 1
      get plt2d sigma_level (value2)
      print "Current sigma level = " (value2)
      print "Give SIGMA level (enter 0 to select upper/lower limits) ? "
      asknum (value2) "Sigma Level \10.0\ : "
    else
      value2 = value1
    end if
    if value2 = 0.0
      send plt2d set nsigma_sort 'DIFF'
      get plt2d sigma_up (value2)
      get plt2d sigma_down (value3)
      print "Current upper/lower sigma levels = " (value2) (value3)
      print "Give the UPPER SIGMA level ? "
      asknum (value2) "Upper Sigma Level \5.0\ : "
      print "Give the LOWER SIGMA level ? "
      asknum (value3) "Lower Sigma Level \1.0\ : "
      send plt2d set sigma_up (value2)
      send plt2d set sigma_down (value3)
    else
      send plt2d set nsigma_sort 'SAME'
      send plt2d set sigma_level (value2)
    end if
    last_plot = 5
    send plt2d set last_plot (last_plot)
    send plt2d set cursor_image (name_image)
    send plt2d set name_image (name_image)
    print "CNSIGMA " (name_image) " on " (worknam)
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
    obeyw plt2d curnsigma (name_image)
  else
    print "You either CANNOT plot IMAGES on this workstation - " (worknam)
    print "    or you CANNOT use the CURSOR on it ..."
  end if
end proc

