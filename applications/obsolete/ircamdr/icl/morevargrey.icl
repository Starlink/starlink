{ PROCEDURE MOREVARGREY : plots an image using vargrey CUT
proc morevargrey value1 value2
  get plt2d image_workstn (image_workstn)
  get plt2d cursor_workstn (cursor_workstn)
  get plt2d name_image (name_image)
  get plt2d worknam (worknam)
  if image_workstn = 1
    yn = undefined(value1)
    if not yn
      yn = undefined(value2)
    end if
    if yn
      promptfor = 1
    else
      promptfor = 0
    end if
    if promptfor = 1
      print "Give new percentage X CUT ? "
      asknum (value3) "Percentage X \20.0\ : "
      print "Give new percentage Y CUT ? "
      asknum (value4) "Percentage Y \80.0\ : "
    else
      value3 = value1
      value4 = value2
    end if
    send plt2d set vargrey_xpc (value3)
    send plt2d set vargrey_ypc (value4)
    last_plot = 9
    send plt2d set last_plot (last_plot)
    print "VARGREY " name_image " on " (worknam)
    obeyw plt2d vargrey
  else
    print "You CANNOT plot IMAGES on this workstation - " (worknam)
  end if
end proc

