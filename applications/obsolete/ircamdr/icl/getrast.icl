{ PROCEDURE GETRAST : gets the size in raster units of the current workstation
proc getrast
  get plt2d max_xsize (rastx)
  get plt2d max_ysize (rasty)
  rastx = rastx+1
  rasty = rasty+1
  print "Maximum workstation coordinate in X,Y are " (rastx) " , " (rasty)
end proc

