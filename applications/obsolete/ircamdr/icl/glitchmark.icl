{ PROCEDURE GLITCHMARK : marks glitches interactively for glitch a-task
proc glitchmark
  print "Give NAME for output GLITCH disk file "
  askname (st1) "Output glitch filename \g.dat\ ? "
  exists = file_exists(st1)
  if exists
    print "File " (st1) " exists ..."
    asklog (delit) "Delete it (Yes or No) \Y\ ? "
    if delit = 1
      delfile (st1)
    else
      print "quiting ..."
      return
    end if
  end if
  print "Use the CURSOR to mark bad pixels ..."
  print "  To EXIT select a pixel OFF the CURRENT image ..."
  print "  (or select pixel 1,1=bottom left)"
  send plt2d set CURSOR_CROSS 'YES'
  obeyw plt2d glitchmark (st1)
  print "File created is called " (st1)
  print "Do you want to DEGLITCH an image using this file ?"
  asklog (value1) "Deglitch Image \Y\ : "
  if value1 = 1
    get plt2d image_name (lastim)
    print "Give NAME of image to be DEGLITCHED (RETURN="(lastim)")"
    askname (st2) "Image to be DEGLITCHED ? "
    if st2 = ""
      st2 = lastim
    end if
    print "Give NAME for image after DEGLITCHING :"
    askname (st3) "Image after DEGLITCHING : "
    print "O.K. deglitching image " (st2) " with " (st1) " glitch file ..."
    obeyw rapi2d GLITCH (st2) (st3) 'Deglitched' 'F' (st1) \
  end if
end proc
