{ PROCEDURE AGAIN : plots the last image again
proc again
  get plt2d worknam (worknam)
  get plt2d name_image (name_image)
  get plt2d last_plot (last_plot)
  send plt2d set cursor_image (name_image)
  if last_plot = 1
    print "Last image display was a PLOT ..."
    obeyw plt2d plot (name_image)
    print "PLOT " (name_image) " on " (worknam)
  end if
  if last_plot = 2
    print "Last image display was an NSIGMA ..."
    obeyw plt2d nsigma (name_image)
    print "NSIGMA " (name_image) " on " (worknam)
  end if
  if last_plot = 3
    print "Last image display was a RANPLOT ..."
    obeyw plt2d ranplot (name_image)
    print "RANPLOT " (name_image) " on " (worknam)
  end if
  if last_plot = 4
    print "Last image display was a CPLOT ..."
    obeyw plt2d curplot (name_image)
    print "CPLOT " (name_image) " on " (worknam)
  end if
  if last_plot = 5
    print "Last image display was a CNSIGMA ..."
    obeyw plt2d curnsigma (name_image)
    print "CNSIGMA " (name_image) " on " (worknam)
  end if
  if last_plot = 6
    print "Last image display was a CRANPLOT ..."
    obeyw plt2d curanplot (name_image)
    print "CRANPLOT " (name_image) " on " (worknam)
  end if
  if last_plot = 9
    print "Last image display was a VARGREY ..."
    obeyw plt2d vargrey (name_image)
    print "VARGREY " (name_image) " on " (worknam)
  end if
  if last_plot = 10
    print "Last image display was a CVARGREY ..."
    obeyw plt2d curvargrey (name_image)
    print "CVARGREY " (name_image) " on " (worknam)
  end if
end proc

