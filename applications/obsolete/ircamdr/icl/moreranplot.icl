{ PROCEDURE MORERANPLOT : plots an image using ranplot range on mean again
proc moreranplot value1
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
      print "Give new RANGE for plot"
      asknum (value2) "New Range \100.0\ : "
    else
      value2 = value1
    end if
    send plt2d set plot_range (value2)
    last_plot = 3
    send plt2d set last_plot (last_plot)
    print "RANPLOT " (name_image) " on " (worknam)
    obeyw plt2d ranplot
  else
    print "You CANNOT plot IMAGES on this workstation - " (worknam)
  end if
end proc

