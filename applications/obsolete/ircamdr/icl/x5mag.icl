{ PROCEDURE X5MAG : sets plate scale for 5x magnifier
proc x5mag
  send plt2d set platscal 0.057
  send plt2d set arcsec_pixel 0.057
  print "Pixel scale set to 0.057 arcsec/pixel"
end proc
