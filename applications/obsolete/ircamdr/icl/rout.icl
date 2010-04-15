{ PROCEDURE ROUT : gets observational parameters UT for an RO
proc rout ronum1 ronum2
  set precision 3
  get plt2d filetype (filetype)
  get plt2d name_prefix (name_prefix)
  get plt2d rosuf (rosuf)
  rosuf = upcase(rosuf)
  if rosuf = "NONE"
    rosuf = " "
  end if
  yn1 = undefined(ronum1)
  yn2 = undefined(ronum2)
  if yn1
    asknum (ronum1a) "Start observation number \1\ ? "
  else
    ronum1a = ronum1
  end if
  if yn2
    asknum (ronum2a) "End observation number   \1\ ? "
  else
    ronum2a = ronum2
  end if
  fclose_c
  delfile rout.dat
  create cfile "rout.dat"
  loop for jj = ronum1a to ronum2a
    if filetype = 1
      im = name_prefix & jj & rosuf
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
    print " "
    dline = jj&"     "&meanut
    write cfile (dline)
    set precision 3
  end loop
  close cfile
  print " "
  print "File rout.out contains UT information"
  print " "
end proc
