{ PROCEDURE MOREPLOT : plots an image using plot between limits again
proc moreplot value3 value4
  get plt2d image_workstn (image_workstn)
  get plt2d cursor_workstn (cursor_workstn)
  get plt2d name_image (name_image)
  get plt2d worknam (worknam)
  if image_workstn = 1
    yn = undefined(value3)
    if not yn
      yn = undefined(value4)
    end if
    if yn
      promptfor = 1
    else
      promptfor = 0
      value1 = value3
      value2 = value4
    end if
    if promptfor = 1
      get plt2d maximum (value1)
      get plt2d minimum (value2)
      print "Current MAXIMUM  = " (value1)
      print "Give NEW maximum for PLOT"
      asknum (value1) "Maximum \0.0\ : "
      print "Current MINIMUM = " (value2)
      print "Give NEW minimum for PLOT"
      asknum (value2) "Minimum \0.0\ : "
    end if
    send plt2d set maximum (value1)
    send plt2d set minimum (value2)
    last_plot = 1
    send plt2d set last_plot (last_plot)
    print "PLOT " (name_image) " on " (worknam)
    obeyw plt2d plot
  else
    print "You CANNOT plot IMAGES on this workstation - " (worknam)
  end if
end proc

