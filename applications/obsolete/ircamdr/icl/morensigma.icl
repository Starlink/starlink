{ PROCEDURE MORENSIGMA : plots an image using N_sigma range on mean again
proc morensigma value1
  get plt2d image_workstn (image_workstn)
  get plt2d cursor_workstn (cursor_workstn)
  get plt2d name_image (name_image)
  get plt2d worknam (worknam)
  if image_workstn = 1
    yn = undefined(value1)
    if yn
      promptfor = 1
    else
      promptfor = 0
    end if
    if promptfor = 1
      get plt2d sigma_level (value5)
      print "Current sigma level = " (value5)
      print "Give new SIGMA level (enter 0 to select upper/lower limits) ? "
      asknum (value5) "Sigma Level \2.0\ : "
    else
      value5 = value1
    end if
    if value5 = 0
      send plt2d set nsigma_sort 'DIFF'
      get plt2d sigma_up (value5)
      get plt2d sigma_down (value6)
      print "Current UPPER,LOWER sigma levels = " (value5) (value6)
      print "Give the UPPER SIGMA level ? "
      asknum (value5) "Upper Sigma Level \5.0\ : "
      print "Give the LOWER SIGMA level ? "
      asknum (value6) "Lower Sigma Level \1.0\ : "
      send plt2d set sigma_up (value5)
      send plt2d set sigma_down (value6)
    else
      send plt2d set nsigma_sort 'SAME'
      send plt2d set sigma_level (value5)
    end if
    last_plot = 2
    send plt2d set last_plot (last_plot)
    print "NSIGMA " (name_image) " on " (worknam)
    obeyw plt2d nsigma
  else
    print "You CANNOT plot IMAGES on this workstation - " (worknam)
  end if
end proc

