{ PROCEDURE ROPAR : gets observational parameters for an RO
proc ropar ronum
  set precision 6
  get plt2d filetype (filetype)
  get plt2d name_prefix (name_prefix)
  get plt2d rosuf (rosuf)
  rosuf = upcase(rosuf)
  if rosuf = "NONE"
    rosuf = " "
  end if
  yn = undefined(ronum)
  if yn
    asknum (ronum2) "Observation number \1\ ? "
  else
    ronum2 = ronum
  end if
  if filetype = 1
    im = name_prefix & ronum2 & rosuf
    obeyw plt2d ropars (im)
  else
    obeyw plt2d contpars (ronum2)
  end if
  get plt2d object_name   (object)
  get plt2d filter        (filter)
  get plt2d exposure_time (dexptime)
  get plt2d number_exp    (nexp)
  get plt2d airmass_start (amst)
  get plt2d airmass_end   (amen)
  get plt2d ra_off        (raoff)
  get plt2d dec_off       (decoff)
  get plt2d utstart       (rutst)
  get plt2d utend         (rutend)
  get plt2d mode          (mode)
  dexptime = real(dexptime)
  amst = real(amst)
  amen = real(amen)
  meanam = (amst+amen)/2.0
  raoff = real(integer(raoff*100.0)/100.0)
  decoff = real(integer(decoff*100.0)/100.0)
  meanut = (rutst+rutend)/2.0
  print "Object            = " (object)
  print "Filter            = " (filter)
  print "Exposure time (s) = " (dexptime)
  print "Exposures/Integ.  = " (nexp)
  print "Mean airmass      = " (meanam)
  print "RA,DEC offsets    = " (raoff) (decoff)
  set precision 5
  print "Mean Obs UT (hrs) = " (meanut)
  print "Obs. mode         = " (mode)
  set precision 6
end proc
