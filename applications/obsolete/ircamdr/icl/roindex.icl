{ PROCEDURE ROINDEX : index RO files
proc roindex rost roen
  set precision 3
  get plt2d filetype (filetype)
  if filetype <> 1
    print " ERROR, cannot run with old file type"
    return
  end if
  get plt2d name_prefix (name_prefix)
  if name_prefix = "NONE"
    print "RO prefix not set, use SETFILE and repeat ROINDEX"
    return
  end if
  get plt2d rosuf (rosuf)
  rosuf = upcase(rosuf)
  if rosuf = "NONE"
    rosuf = " "
  end if
  yn1 = undefined(rost)
  yn2 = undefined(roen)
  if yn1
    asknum (st) "Start observation number \1\ ? "
  else
    st = rost
  end if
  if yn2
    asknum (en) "End   observation number \1\ ? "
  else
    en = roen
  end if
  print "Accessing info, please wait....."
  fclose_b
  delfile roindex.dat
  create bfile "roindex.dat"
  dline = "       Obs.  Object             Filter    Scale    Expo.   Nexp    Airmass"
  write bfile (dline)
  loop for j = (st) to (en)
    im = name_prefix & j & rosuf
    obeyw plt2d ropars (im)
    get plt2d object_name   (object)
    get plt2d filter        (filter)
    get plt2d exposure_time (dexptime)
    get plt2d number_exp    (nexp)
    get plt2d airmass_start (amst)
    get plt2d airmass_end   (amen)
    get plt2d pix_size      (scale)
    nc = len(object)
    if nc < 15
      na = 15-nc
      loop for k = 1 to na
        object = object & "."
      end loop
    end if
    filter = substr(filter,1,6)
    nf = len(filter)
    if nf < 6
      na = 6-nf
      loop for k = 1 to na
        filter = filter & "."
      end loop
    end if
    exp = real(dexptime)
    am = real((amst+amen)/2.0)
    ne = len(nexp)
    nexp = integer(nexp)
    scale = real(scale)
    dline = "        "&(j)&"   "&(object)&"    "&(filter)&"    "&~
(scale)&"    "&(exp)&"    "&(nexp)&"       "&(am)
    write bfile (dline)
  end loop
  dline = "       Obs.  Object             Filter    Scale    Expo.   Nexp    Airmass"
  write bfile (dline)
  fclose_b
  $ more roindex.dat
  print " "
  print "Output index file = roindex.dat"
  print " "
end proc
