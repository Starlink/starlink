{ PROCEDURE CURHOT : removes hot pixels in user defined box
proc curhot
  get plt2d cursor_workstn (cursor_workstn)
  if cursor_workstn = 1
    get plt2d name_image (im)
    print "Current image displayed = " (im)
    askname (out) "Output image name \junk\ ? "
    print "CURSOR displayed ... "
    obeyw plt2d cursor
    get plt2d x_cur_pixel (xc)
    get plt2d y_cur_pixel (yc)
    get plt2d cursor_value (val)
    xc = integer(xc)
    yc = integer(yc)
    print "Pixel selected = " (xc) (yc) " with value " (val)
    asknum (xb) "X box size for search \15\ ? "
    asknum (yb) "Y box size for search \15\ ? "
    obeyw obsrap hotshot (im) (out) NO (xc) (yc) (xb) (yb)
  else
    print "You CANNOT display a CURSOR on this workstation"
  end if
end proc
