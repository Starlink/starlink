{ PROCEDURE SETPS : sets the plate scale for feature plotting on images
proc setps value1
  yn = undefined(value1)
  if yn
    get plt2d arcsec_pixel (platscal)
    print "Current plate scale = " (platscal)
    print "Give the number of ARCSECONDS per PIXEL in the IMAGES ?"
    asknum (platscal) "Plate Scale \1.0\ : "
  else
    platscal = value1
  end if
  send plt2d set arcsec_pixel (platscal)
  send plt2d set platscal (platscal)
  print "O.K. have set the PLATE SCALE to "  (platscal) " arcsecs/pixel ..."
end proc

