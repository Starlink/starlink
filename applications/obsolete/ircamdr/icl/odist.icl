{ PROCEDURE ODIST : procedure to run DIST rapi2d action
proc odist num_obsele code_image
  testval2 (num_obsele) (code_image)
  print "Select two positions in the image with the cursor ..."
  print "If you want coordinates (RA,DEC) from the selected positions the "
  print "first point chosen should be the one that the SETNUM RA/DEC "
  print "coordinates refers to ..."
  print "Select POSITION 1 ..."
  obeyw plt2d cursor
  get plt2d x_cur_pixel (xpix1)
  get plt2d y_cur_pixel (ypix1)
  print "Pixel selected = " (xpix1) (ypix1)
  print "Select POSITION 2 ..."
  obeyw plt2d cursor
  get plt2d x_cur_pixel (xpix2)
  get plt2d y_cur_pixel (ypix2)
  print "Pixel selected = " (xpix2) (ypix2)
  get plt2d platscal (platscal)
  print "Current pixel scale = " (platscal) " arcsec/pixel"
  get plt2d ra_zero (ra_coord)
  get plt2d dec_zero (dec_coord)
  obeyw obsrap DIST (xpix1) (ypix1) (xpix2) (ypix2) ~
    (platscal) (ra_coord) (dec_coord)
end proc

