{ PROCEDURE OCSTATS : procedure to run STATS rapi2d action with cursor
proc ocstats num_obsele code_image
  testval2 (num_obsele) (code_image)
  get plt2d name_image (last_image)
  get_imagename (num_obsele) (code_image) (name_out) (last_image)
  name_image = name_out
  print "Image being used for CSTATS = " (name_image)
  asknum (xsiz) "Enter the stats box X-size in arcsec \8\ : "
  asknum (ysiz) "Enter the stats box Y-size in arcsec \8\ : "
  obeyw plt2d clear
  obeyw plt2d nsigma (name_image)
  send plt2d set cursor_image (name_image)
  print "Select the CENTRE of the box for STATISTICS CALCULATION ..."
  print "with the cursor ..."
  obeyw plt2d cursor
  get plt2d x_cur_pixel (xpix1)
  get plt2d y_cur_pixel (ypix1)
  get plt2d platscal (platscal)
  xstb = int(xpix1-(xsiz/platscal/2.0))
  ystb = int(ypix1-(ysiz/platscal/2.0))
  if xstb < 1
    xstb = 1
  end if
  if ystb < 1
    ystb = 1
  end if
  print "O.K. cursor coordinates are " (xpix1) " " (ypix1)
  print "O.K. running HISTO on specified box ..."
  send plt2d set cursor_cross 'YES'
  obeyw plt2d BOX (xstb) (ystb) (xsiz) (ysiz) 'BOTTOM_LEFT'
  obeyw rapi2d HISTO (name_image) (xstb) (ystb) (xsiz) (ysiz) \
end proc

