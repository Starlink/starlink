{ PROCEDURE ARRAY_TESTS : reduced array_tests exec data
proc array_tests st
  get plt2d filetype (filetype)
  if filetype <> 1
    print "Error, cannot run ARRAY_TESTS on old data format"
    return
  end if
  yn = undefined(st)
  get plt2d name_prefix (pref)
  get plt2d rosuf (rosuf)
{  pref = upcase(pref)
  if pref = "UNKNOWN"
    print "Error, file prefix not defined - run SETPRE"
    return
  end if
{  rosuf = upcase(rosuf)
  if rosuf = "NONE"
    rosuf = " "
  end if
  if yn
    print "Give start observation number of ARRAY_TESTS sequence : "
    asknum (atst) "Start Obs Number \1\ ? "
  else
    atst = st
  end if
  fclose_a
  host = getenv("HOST")
  if host = "ike"
    fexist = file_exists("/ukirt_sw/logs/ircam3_array_tests.log")
    if fexist
      print "Writing results to /ukirt_sw/logs/ircam3_array_tests.log"
      append afile "/ukirt_sw/logs/ircam3_array_tests.log"
    else
      create afile "/ukirt_sw/logs/ircam3_array_tests.log"
      dline = "UT DATE      STARE_RN      ND_STARE_RN      DARK CURRENT"
      write afile (dline)
    end if
  end if
  imno = atst+1
  im1 = pref & imno & rosuf
  imno = atst+2
  im2 = pref & imno & rosuf
  obeyw rapi2d SUB (im1) (im2) junk
  obeyw rapi2d STATS junk \
  getpar glob stats_std (stare_std)
  stare_rn = (stare_std/sqrt(2.0))*6.0
  imno = atst+3
  im1 = pref & imno & rosuf
  imno = atst+4
  im2 = pref & imno & rosuf
  obeyw rapi2d SUB (im1) (im2) junk
  obeyw rapi2d STATS junk \
  getpar glob stats_std (nd_stare_std)
  nd_stare_rn = (nd_stare_std/sqrt(2.0))*6.0
  imno = atst+5
  im1 = pref & imno & rosuf
  imno = atst+7
  im2 = pref & imno & rosuf
  obeyw rapi2d SUB (im1) (im2) junk
  obeyw rapi2d HISTO junk \
  getpar glob histo_median (darkc)
  darkc = darkc*6.0/59.0
  delfile junk.sdf
  utd     = getenv("UTD")
  cstrn   = REAL(stare_rn:10:3)
  cndstrn = REAL(nd_stare_rn:10:3)
  cdark   = REAL(darkc:10:3)
  dline = utd & '       ' & cstrn & '          ' & cndstrn & '           ' & cdark
  if host = "ike"
    write afile (dline)
  end if
  fclose_a
  print " "
  print "STARE    mode readout noise (e-) = " (stare_rn)
  print "ND_STARE mode readout noise (e-) = " (nd_stare_rn)
  print "DARK CURRENT (e-/sec)            = " (darkc)
  print " "
  if stare_rn <= 30.0
    print "STARE readout noise is LOW wrt nominal of 56e-"
  else if stare_rn > 70.0
    print "STARE readout noise is HIGH low wrt nominal of 56e-"
  else
    print "** STARE readout noise is nominal **"
  end if
  if nd_stare_rn <= 20.0
    print "ND_STARE readout noise is LOW wrt nominal of 38e-"
  else if nd_stare_rn > 50.0
    print "ND_STARE readout noise is HIGH wrt nominal of 38e-"
  else
    print "** ND_STARE readout noise is nominal **"
  end if
  if darkc <= 0.0
    print "Dark current is NEGATIVE, wierd - nominal = 0-5e-/sec"
  else if darkc > 10.0
    print "Dark current is HIGH wrt nominal = 0-5e-/sec"
  else
    print "** Dark current is nominal **"
  end if
  print " "
end proc
