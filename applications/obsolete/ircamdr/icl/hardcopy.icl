{ PROCEDURE HARDCOPY : closes plotting, opens it on hardcopy workstation...
proc hardcopy v1
  get plt2d hard_device (hard_device)
  if hard_device < 0
    print "Hardcopy device not defined - using Postscript Portrait"
    print "Use SETHARD to change hardcopy device."
    sethard 15
  end if
  yn = undefined(v1)
  if yn
    promptfor = 1
  else
    if v1 < 1 or v1 > 6
      promptfor = 1
    else
      promptfor = 0
      v2 = v1
    end if
  end if
  if promptfor = 1
    print "  "
    print "Hardcopy of last :"
    print "  Line graphics executed (CONTOUR,CUT,VEC)        = 1 "
    print "  CONTOUR MAP with last VECTOR MAP overlaid       = 2 "
    print "  IMAGE displayed (PLOT,NSIGMA,RANPLOT,VARGREY)   = 3 "
    print "  IMAGE displayed overlaid with last CONTOUR MAP  = 4 "
    print "  IMAGE displayed overlaid with last VECTOR MAP   = 5 "
    print "  IMAGE overlaid with last CONTOUR MAP+VECTOR MAP = 6 "
    askchoice (v2) "Choose (1,2,3,4,5,6) \1\ : "
  end if
  delfile fort.1
  delfile fort.1.1
  delfile fort.1.2
  delfile fort.1.3
  delfile fort.1.4
  delfile fort.1.5
  if v2 = 1
    HARDCOPY_LINE
  else if v2 = 2
    HARDCOPY_CV
  else if v2 = 3
    HARDCOPY_IMAGE
  else if v2 = 4
    HARDCOPY_IL
  else if v2 = 5
    HARDCOPY_IV
  else if v2 = 6
    HARDCOPY_ILV
  end if
end proc
