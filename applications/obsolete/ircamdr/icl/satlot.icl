{ PROCEDURE SATLOT : plots saturj occultation images in movie
proc satlot
  pltyp = 2
  ffpre = "diff"
{  plnst = 176
{  plnen = 1259
  plnst = 1000
  plnen = 1259
  send plt2d set magnification 0
  obeyw plt2d clear
  loop for dummy = (plnst) to (plnen)
    im = ffpre&dummy
    name_image = im
    send plt2d set name_image (im)
    send plt2d set cursor_image (im)
    obeyw plt2d nsigma (im) 10
    print "Image " (im) " displayed using NSIGMA ..."
  end loop
end proc

