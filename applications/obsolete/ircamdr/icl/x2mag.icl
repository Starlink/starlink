{ PROCEDURE X2MAG : sets plate scale for 2x magnifier
proc x2mag
  send plt2d set platscal 0.143
  send plt2d set arcsec_pixel 0.143
  print "Pixel scale set to 0.143 arcsec/pixel"
end proc
