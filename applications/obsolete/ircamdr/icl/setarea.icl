{ PROCEDURE SETAREA : sets area of image plotted full or sub-area
proc setarea
  print "Plot full image (F) or sub-area (S) ? "
  askchoice (pltwht) "Full or Sub-area (F,S) \F\ ? "
  if pltwht = 1
    send plt2d set subim_option 'A'
  else
    send plt2d set subim_option 'S'
    print "Select centre of sub-area with cursor ? "
    asklog (selcur) "Cursor Select (Yes or No) \N\ ? "
    if selcur = 1
      print "Select the IMAGE CENTRE with cursor ..."
      obeyw plt2d cursor
      get plt2d x_cur_pixel (xpix)
      get plt2d y_cur_pixel (ypix)
      print "Pixel selected = " (xpix) (ypix)
      xpix = integer(xpix)
      ypix = integer(ypix)
      print "Give X,Y size of sub-area in pixels ? "
      asknum (xsz) "X-Size pixel \64\ ? "
      asknum (ysz) "Y-Size pixel \64\ ? "
      xst = xpix
      yst = ypix
      xen = xpix+xsz-1
      yen = ypix+ysz+1
print (xst) (yst) (xen) (yen)
    else
      print "Give X,Y start and end pixel for sub-area ? "
      asknum (xst) "X-Start pixel  \1\ ? "
      asknum (yst) "Y-Start pixel  \1\ ? "
      asknum (xen) "X-End   pixel \64\ ? "
      asknum (yen) "Y-End   pixel \64\ ? "
    end if
    send plt2d set subim_xst (xst)
    send plt2d set subim_yst (yst)
    send plt2d set subim_xen (xen)
    send plt2d set subim_yen (yen)
  end if
end proc
