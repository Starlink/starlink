{ PROCEDURE SETVAR : sets up system variables
proc setvar
  get plt2d platscal (def)
  print "Give PIXEL SCALE in arcsec/pixel: CURRENT DEFAULT = " (def)
  asknum (pscal) "Pixel Scale \-1\ : "
  if pscal = -1
    pscal = def
  end if
  send plt2d set platscal (pscal)
  send plt2d set arcsec_pixel (pscal)

  get plt2d disp_mag (def)
  print "Give DISP magnification: CURRENT DEFAULT = " (def)
  asknum (disp_mag) "Magnification \-1\ : "
  if disp_mag = -1
    disp_mag = def
  end if
  send plt2d set disp_mag (disp_mag)

  get plt2d disp_ns (def)
  print "Give DISP sigma level: CURRENT DEFAULT = " (def)
  asknum (disp_ns)  "Sigma Level   \-1\ : "
  if disp_ns = -1
    disp_ns = def
  end if
  send plt2d set disp_ns (disp_ns)
  print " "

  get plt2d disp_ap1 (def)
  print "Give PHO3 star aperture (arcsec): CURRENT DEFAULT = " (def)
  asknum (disp_ap1) "Star aperture \-1\ : "
  if disp_ap1 = -1
    disp_ap1 = def
  end if
  send plt2d set disp_ap1 (disp_ap1)

  get plt2d disp_ap2 (def)
  print "Give PHO3 sky inner aperture (arcsec): CURRENT DEFAULT = " (def)
  asknum (disp_ap2) "Sky Inner Aperture \-1\ : "
  if disp_ap2 = -1
    disp_ap2 = def
  end if
  send plt2d set disp_ap2 (disp_ap2)

  get plt2d disp_ap3 (def)
  print "Give PHO3 sky outer aperture (arcsec): CURRENT DEFAULT = " (def)
  asknum (disp_ap3) "Sky Outer Aperture \-1\ : "
  if disp_ap3 = -1
    disp_ap3 = def
  end if
  send plt2d set disp_ap3 (disp_ap3)
  print " "

  get plt2d zeroj (def)
  print "Give zeropoint for J: CURRENT DEFAULT = " (def)
  asknum (zj) "J Zeropoint \-1\ ? "
  if zj = -1
    zj = def
  end if
  send plt2d set zeroj (zj)

  get plt2d zeroh (def)
  print "Give zeropoint for H: CURRENT DEFAULT = " (def)
  asknum (zh) "H Zeropoint \-1\ ? "
  if zh = -1
    zh = def
  end if
  send plt2d set zeroh (zh)

  get plt2d zerok (def)
  print "Give zeropoint for K: CURRENT DEFAULT = " (def)
  asknum (zk) "K Zeropoint \-1\ ? "
  if zk = -1
    zk = def
  end if
  send plt2d set zerok (zk)

  get plt2d zeronbl (def)
  print "Give zeropoint for nbL: CURRENT DEFAULT = " (def)
  asknum (znbl) "nbL Zeropoint \-1\ ? "
  if znbl = -1
    znbl = def
  end if
  send plt2d set zeronbl (znbl)

  get plt2d zerolp (def)
  print "Give zeropoint for Lp: CURRENT DEFAULT = " (def)
  asknum (zlp) "Lp Zeropoint \-1\ ? "
  if zlp = -1
    zlp = def
  end if
  send plt2d set zerolp (zlp)

  get plt2d zeronbm (def)
  print "Give zeropoint for nbM: CURRENT DEFAULT = " (def)
  asknum (znbm) "nbM Zeropoint \-1\ ? "
  if znbm = -1
    znbm = def
  end if
  send plt2d set zeronbm (znbm)
end proc
