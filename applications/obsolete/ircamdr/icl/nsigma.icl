{ PROCEDURE NSIGMA : plots an image using N_sigma range on mean
proc nsigma num_obsele code_image value1
  get plt2d image_workstn (image_workstn)
  get plt2d cursor_workstn (cursor_workstn)
  get plt2d name_image (name_image)
  get plt2d worknam (worknam)
  get plt2d filetype (filetype)
  if image_workstn <> 1
    print "You CANNOT plot IMAGES on this workstation - " (worknam)
    return
  end if
  yn = undefined(num_obsele)
  yn2 = undefined(code_image)
  if yn
    num_obsele2 = -999
    code_image2 = -999
    value2 = -999
    promptfor = 1
    itype = "INTEGER"
  else
    num_obsele2 = num_obsele
    itype = type(num_obsele)
    if yn2
      code_image2 = -999
      value2 = -999
      promptfor = 1
    else
      if filetype = 1
        value2 = code_image
        code_image2 = 1
        promptfor = 0
      else
        code_image2 = code_image
        value2 = -999
        promptfor = 1
      end if
    end if
  end if
{  print (itype)
  name_out = " "
  get plt2d name_image (last_image)
  get_imagename (num_obsele2) (code_image2) (name_out) (last_image)
  name_image = name_out
  if promptfor = 1 and value2 = -999
    get plt2d sigma_level (value2)
    print "Current sigma level = " (value2)
    print "Give SIGMA level (enter 0 to select upper/lower limits) ? "
    asknum (value2) "Sigma Level \10.0\ : "
  end if
  if value2 = 0
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
  send plt2d set last_plot 2
  send plt2d set cursor_image (name_image)
  send plt2d set name_image (name_image)
  print "NSIGMA " (name_image) " on " (worknam)
  obeyw plt2d nsigma (name_image)
  get plt2d magnification (mag1)
  if mag1 = 0
    get plt2d image_calmag (mag2)
    print "Magnification=0, calculated magnification = " (mag2)
  end if
end proc
