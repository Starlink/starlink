{ PROCEDURE TOMAG : converts intensity to magnitudes in images
proc tomag
  askname (st1) "Input image to be converted to magnitudes : "
  asknum (value1) "Zeropoint to be used : "
  askname (st2) "Output image (magnitudes\pixel) : "
  askname (st3) "Output image (magnitudes\sq arcsec) : "
  get plt2d platscal (platscal)
  value2 = 2.5*(log(platscal**2)/log(10.0))
  print "Pixel scale currently set is " (platscal) " arcsec/pixel"
  print "Magnitude correction factor (/pixel-to-/sq arcsec) = " ~
    (value2) " mags."
  print "Forming magnitude images ... "
  obeyw rapi2d LOG10 (st1) 10 junk1
  obeyw rapi2d CMULT junk1 -2.5 junk2
  obeyw rapi2d CADD junk2 (value1) (st2)
  obeyw rapi2d CADD (st2) (value2) (st3)
end proc

