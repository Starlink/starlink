{ PROCEDURE DISPICK : procedure to run PICKIM rapi2d action with cursor
proc dispick num_obsele code_image
  testval2 (num_obsele) (code_image)
  get plt2d name_image (last_image)
  get_imagename (num_obsele) (code_image) (name_out) (last_image)
  name_image = name_out
  print "Select the BOTTOM LEFT corner of the area to be PICKED"
  print "with the cursor ..."
  obeyw plt2d cursor
  get plt2d x_cur_pixel (xpix1)
  get plt2d y_cur_pixel (ypix1)
  print "Bottom left coordinates are " (xpix1) " " (ypix1)
  print "Select the TOP RIGHT corner of the area to be PICKED"
  print "with the cursor ..."
  obeyw plt2d cursor
  get plt2d x_cur_pixel (xpix2)
  get plt2d y_cur_pixel (ypix2)
  print "Top right coordinates are   " (xpix2) " " (ypix2)
  xsiz = int(xpix2-xpix1+1)
  ysiz = int(ypix2-ypix1+1)
  print "Size of picked sub-image = " (xsiz) " by " (ysiz)
  obeyw rapi2d PICKIM (name_image) (xpix1) (ypix1) (xsiz) (ysiz) \
end proc

