{ PROCEDURE POLPICK2 : procedure to run PICKIM rapi2d action with cursor on
{ 8 polarization intensity images.
proc polpick2
  askname (pol0o)  "o-ray    0 deg. image  \pol0or\ ? "
  askname (pol0e)  "e-ray    0 deg. image  \pol0er\ ? "
  askname (pol45o) "o-ray   45 deg. image \pol45or\ ? "
  askname (pol45e) "e-ray   45 deg. image \pol45er\ ? "
  askname (pol22o) "o-ray 22.5 deg. image \pol22or\ ? "
  askname (pol22e) "e-ray 22.5 deg. image \pol22er\ ? "
  askname (pol67o) "o-ray 67.5 deg. image \pol67or\ ? "
  askname (pol67e) "e-ray 67.5 deg. image \pol67er\ ? "
  out0o = pol0o&"p"
  out0e = pol0e&"p"
  out45o = pol45o&"p"
  out45e = pol45e&"p"
  out22o = pol22o&"p"
  out22e = pol22e&"p"
  out67o = pol67o&"p"
  out67e = pol67e&"p"
  send plt2d set cursor_image (pol0o)
  obeyw plt2d nsigma (pol0o)
  print "Select the BOTTOM LEFT corner of the area to be picked"
  print "with the cursor ..."
  obeyw plt2d cursor
  get plt2d x_cur_pixel (xpix1)
  get plt2d y_cur_pixel (ypix1)
  print "Bottom left coordinates are " (xpix1) " " (ypix1)
  print "Select the TOP RIGHT corner of the area to be picked"
  print "with the cursor ..."
  obeyw plt2d cursor
  get plt2d x_cur_pixel (xpix2)
  get plt2d y_cur_pixel (ypix2)
  print "Top right coordinates are   " (xpix2) " " (ypix2)
  xsiz = int(xpix2-xpix1+1)
  ysiz = int(ypix2-ypix1+1)
  print "Size of picked sub-image = " (xsiz) " by " (ysiz)
  obeyw rapi2d PICKIM (pol0o) (xpix1) (ypix1) (xsiz) (ysiz) (out0o) \
  obeyw rapi2d PICKIM (pol0e) (xpix1) (ypix1) (xsiz) (ysiz) (out0e) \
  obeyw rapi2d PICKIM (pol45o) (xpix1) (ypix1) (xsiz) (ysiz) (out45o) \
  obeyw rapi2d PICKIM (pol45e) (xpix1) (ypix1) (xsiz) (ysiz) (out45e) \
  obeyw rapi2d PICKIM (pol22o) (xpix1) (ypix1) (xsiz) (ysiz) (out22o) \
  obeyw rapi2d PICKIM (pol22e) (xpix1) (ypix1) (xsiz) (ysiz) (out22e) \
  obeyw rapi2d PICKIM (pol67o) (xpix1) (ypix1) (xsiz) (ysiz) (out67o) \
  obeyw rapi2d PICKIM (pol67o) (xpix1) (ypix1) (xsiz) (ysiz) (out67e) \
  print " "
  print "Output picked image called : "
  print "                             " (out0o) (out0e)
  print "                             " (out45o) (out45e)
  print "                             " (out22o) (out22e)
  print "                             " (out67o) (out67e)
  print " "
end proc

