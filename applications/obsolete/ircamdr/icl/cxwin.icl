{ PROCEDURE CXWIN : changes window size for plotting
proc cxwin
  print "New window Large, Medium or Small ? "
  askchoice (nwin) "New Size (L,M,S) \L\ ? "
  pclose
  ! /star/starlink/lib/gwm/xdestroy GKS_3800
  if nwin = 1
    ! /star/starlink/lib/gwm/xmake GKS_3800 -geom 640x640 -col 64 -fg white -bg black
  else if nwin = 2
    ! /star/starlink/lib/gwm/xmake GKS_3800 -geom 512x512 -col 64 -fg white -bg black
  else
    ! /star/starlink/lib/gwm/xmake GKS_3800 -geom 380x380 -col 64 -fg white -bg black
  end if
  popen 1
  setquad 5
  get plt2d max_xsize (x)
  get plt2d max_ysize (y)
  print "New graphics window is " (x) " by " (y) " in size"
end proc
