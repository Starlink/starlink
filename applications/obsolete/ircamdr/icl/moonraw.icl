{ PROCEDURE MOONRAW : plots raw saturn occultation images in movie
proc moonraw
  pltyp = 2
  ffpre = "moons"
  plnst = 1
  plnen = 100
  send plt2d set magnification 0
  obeyw plt2d clear
  loop for dummy = (plnst) to (plnen)
    if dummy = 1
      im = ffpre
    else
      im = ffpre&"G"&dummy
    end if
    name_image = im
    send plt2d set name_image (im)
    send plt2d set cursor_image (im)
    obeyw plt2d nsigma (im) 2
    print "Image " (im) " displayed using NSIGMA ..."
  end loop
end proc

