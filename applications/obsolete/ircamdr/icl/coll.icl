{ PROCEDURE COLL : collimation program for IRCAM3
proc COLL
  print " "
  asknum (flath) "Enter the current FLAT HORIZONTAL setting \0.0\ "
  asknum (flatv) "Enter the current FLAT VERTICAL   setting \0.0\ "
  print " "
  asknum (collh) "Enter the current COLL HORIZONTAL setting \0.0\ "
  asknum (collv) "Enter the current COLL VERTICAL   setting \0.0\ "
  print " "
  asknum (incr)  "Enter increment (in mm) for scan \0.25\ "
  print " "
  fexists = file_exists("ircam3_coll.dat")
  if fexists
    delfile ircam3_coll.dat
  end if
  create cfile ircam3_coll.dat
  dline = "                              **************************"
  write cfile (dline)
  dline = "                              *IRCAM3 COLLIMATION CHART*"
  write cfile (dline)
  dline = "                              **************************"
  write cfile (dline)
  dline = " "
  write cfile (dline)
  dline = "HORIZONTAL COLLIMATION SETTINGS - 0,0 = "&(flath)&","&(collh)
  write cfile (dline)
  dline = "-------------------------------------------------------------------------------"
  write cfile (dline)
  dline = "          HSHIFT       New FLAT HOR    New COLL HOR       VALUE"
  write cfile (dline)
  dline = "-------------------------------------------------------------------------------"
  write cfile (dline)
  loop for dummy = -5 to 5 step 1
    hshift = dummy*incr
    newflath = flath+0.0882*hshift
    newcollh = collh+0.0390*hshift
    write cfile (hshift:15:3) (newflath:15:3) (newcollh:15:3)
    dline = "-------------------------------------------------------------------------------"
    write cfile (dline)
  end loop
  dline = " "
  write cfile (dline)
  dline = " "
  write cfile (dline)
  dline = "VERTICAL COLLIMATION SETTINGS - 0,0 = "&(flatv)&","&(collv)
  write cfile (dline)
  dline = "-------------------------------------------------------------------------------"
  write cfile (dline)
  dline = "          VSHIFT       New FLAT VER    New COLL VER       VALUE"
  write cfile (dline)
  dline = "-------------------------------------------------------------------------------"
  write cfile (dline)
  loop for dummy = -5 to 5 step 1
    vshift = dummy*incr
    newflatv = flatv-0.1440*vshift
    newcollv = collv+0.0392*vshift
    cvs = REAL(vshift:10:3)
    nfl = REAL(newflatv:10:3)
    nco = REAL(newcollv:10:3)
    write cfile (vshift:15:3) (newflatv:15:3) (newcollv:15:3)
    dline = "-------------------------------------------------------------------------------"
    write cfile (dline)
  end loop
  fclose_c
  print "File ircam3_coll.dat contains user-defined collimation settings"
  print " "
  asklog (yn) "Print (Yes or No) \Y\ ? "
  if yn = 1
    sh lp ircam3_coll.dat
  end if
end proc
