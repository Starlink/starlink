{ PROCEDURE PLOTGLITCH : plots an image, glitchmarks it, plot result
proc plotglitch
  get plt2d disp_mag (disp_mag)
  print "Display magnification = " (disp_mag)
  delfile plotglitch.lis
  loop for value1 = 1 to 1000
    askname (img) "Image to be displayed and de-glitched (CTRL C to STOP) ? "
    img2 = img & "g"
    obeyw plt2d clear
    send plt2d set magnification (disp_mag)
    print "Displaying image " (img) " before de-glitching ..."
    send plt2d set cursor_image (img)
    obeyw plt2d nsigma (img)
    send plt2d set CURSOR_CROSS 'YES'
    print "Select bad pixels/glitches using cursor ..."
    print "Select area off image to end input of bad pixels"
    obeyw plt2d glitchmark 'plotglitch.lis'
    print "Glitching the image " (img) " ..."
    obeyw rapi2d glitch (img) (img2) 'glitched' f 'plotglitch.lis'
    delfile plotglitch.lis
    print "Deglitched image is called " (img2)
    print "Displaying image " (img2) " after de-glitching ..."
    obeyw plt2d nsigma (img2)
  end loop
end proc

