{ PROCEDURE ROMED : median filters a number of RO files
proc romed st no da ou
  get plt2d filetype (filetype)
  get plt2d rosuf (rosuf)
  rosuf2 = upcase(rosuf)
  if rosuf2 = "NONE"
    rosuf = ""
  end if
  yn1 = undefined(st)
  yn2 = undefined(no)
  yn3 = undefined(da)
  yn4 = undefined(ou)
  get plt2d name_prefix (name_prefix)
  if name_prefix = "UNKNOWN"
    print "You must define IRCAM data file before running ROMED"
    print "Use command SETPRE."
    return
  else
    get plt2d contname (contname)
    print "Current IRCAM file prefix = " (name_prefix)
  end if
  get plt2d platscal (platscal)
  if yn1
    print "Enter start observation number ? "
    asknum (value2) "Start \1\ ? "
  else
    value2 = st
  end if
  if yn2
    print "Enter the number of observation to be median filtered ? "
    asknum (value3) "Number \1\ ? "
  else
    value3 = no
  end if
  if yn3
    print "Enter the DARK image number : "
    asknum (obs) "Dark number \1\ ? "
  else
    obs = da
  end if
  en = value2+value3-1
  value3 = integer(en)
  dark = name_prefix & obs & rosuf
  obeyw plt2d ropars (dark)
  get plt2d object_name   (dobject)
  get plt2d filter        (dfilter)
  get plt2d exposure_time (exp)
  dobject = upcase(dobject)
  dfilter = upcase(dfilter)
  dobject = substr(dobject,1,4)
  dfilter = substr(dfilter,1,6)
  if dobject = "DARK" and dfilter = "BLANKS"
    print "    Observation specified as DARK is truly a DARK"
    print "    Exposure time (sec) = " (exp)
  else
    print "Observation specified as DARK may not be a DARK"
    print "  Object name in supposed DARK image = " (dobject)
    print "  Filter name in supposed DARK image = " (dfilter)
    print "Do you want to continue anyway ?"
    asklog (docont) "Continue (Yes or No) \N\ ? "
    if docont = 0
      return
    end if
  end if
  if platscal = 1.0
    print "Enter pixel scale in arcsec/pixel : "
    asknum (platscal) "Pixel Scale \0.286\ ? "
    send plt2d set platscal (platscal)
  end if
  if yn4
    print "Give name for output median filtered image : "
    askname (final) "Output Image \skyflat\ ? "
  else
    final = ou
  end if
  fclose_b
  delfile darklot.list
  create bfile "darklot.list"
  print " "
  print "DARK SUBTRACTION SECTION"
  print "========================"
  loop for dummy = (value2) to (value3)
    im = name_prefix & dummy & rosuf
    out = name_prefix & dummy & rosuf & "d"
    obeyw plt2d ropars (im)
    get plt2d object_name   (object)
    get plt2d filter        (filter)
    get plt2d exposure_time (oexp)
    print "Object name = " (object)
    print "Filter      = " (filter)
    print "Exposure time (sec) = " (oexp)
    write bfile (out)
    print "Dark subtracting " (im) " ..."
    obeyw rapi2d SUB (im) (dark) darklot_junk2
    if exp <> 0
      invexp = 1.0/exp
      print "Scaling SOURCE-DARK to DN/S, scaling factor   = x" (invexp)
    else
      print "Error, scaling factor to DN/S = 0"
      return
    end if
    obeyw rapi2d CDIV darklot_junk2 (exp) (out)
    print "Dark subtracted image = " (out)
  end loop
  delfile darklot_junk2.sdf
  fclose_b
  print "MEDIAN FILTERING SECTION"
  print "========================"
  obeyw obsrap MED3D "darklot.list" (final) \
  delfile med3d_work.sdf
  delfile med3d_lwork.sdf
  loop for dummy = (value2) to (value3)
    rbfile = name_prefix & dummy & rosuf & "d.sdf"
    delfile (rbfile)
  end loop
  print "Final median filtered image = " (final)
end proc
