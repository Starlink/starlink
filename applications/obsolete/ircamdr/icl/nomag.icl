{ PROCEDURE NOMAG : sets plate scale for NO magnifier
proc nomag
  send plt2d set platscal 0.286
  send plt2d set arcsec_pixel 0.286
  print "Pixel scale set to 0.286 arcsec/pixel"
end proc
