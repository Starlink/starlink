{ PROCEDURE CALMAG : calculates magnitude from number and zeropoint
proc calmag value1 value2
  yn1 = undefined(value1)
  yn2 = undefined(value2)
  if yn1
    print "Enter FILTER used : "
    askname (fi) "Filter \K\ : "
  else
    fi = value1
  end if
  fi = upcase(fi)
  if yn2
    print "Enter INTENSITY in DN/SECOND : "
    asknum (v2) "Intensity \0.0\ : "
  else
    v2 = value2
  end if
  get plt2d zeroj   (zeroj)
  get plt2d zeroh   (zeroh)
  get plt2d zerok   (zerok)
  get plt2d zeronbl (zeronbl)
  get plt2d zerolp  (zerolp)
  get plt2d zeronbm (zeronbm)
  get plt2d platscal (pscal)
  if pscal = 1.0
    print " "
    print "Give pixel scale of images : "
    asknum (pscal) "Arcsecond per pixel \0.286\ ? "
    send plt2d set platscal (pscal)
    send plt2d set arcsec_pixel (pscal)
    print " "
  end if
  if fi = "J"
    v1 = zeroj
  end if
  if fi = "H"
    v1 = zeroh
  end if
  if fi = "K"
    v1 = zerok
  end if
  if fi = "NBL"
    v1 = zeronbl
  end if
  if fi = "LP"
    v1 = zerolp
  end if
  if fi = "NBM"
    v1 = zeronbm
  end if
  mpp = ((v1)-(2.5*log(v2)/log(10)))
  mpsa = mpp-2.5*log(1.0/(pscal*pscal))/log(10)
  mp2 = mpp-2.5*log(3.1415926*((2.0/pscal)/2.0)**2)/log(10)
  mp4 = mpp-2.5*log(3.1415926*((4.0/pscal)/2.0)**2)/log(10)
  print "Zeropoint               = " (v1)
  print "Magnitude /pixel        = " (mpp)
  print "Magnitude /sq arcsec    = " (mpsa)
  print "Magnitude /2 arcsec ap. = " (mp2)
  print "Magnitude /4 arcsec ap. = " (mp4)
end proc
