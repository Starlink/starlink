{ PROCEDURE GETOFF : gets RA,DEC offsets from RO file headers
proc getoff v1 v2
  set precision 6
  get plt2d filetype (filetype)
  get plt2d rosuf (rosuf)
  get plt2d name_prefix (name_prefix)
  name_prefix2 = upcase(name_prefix)
  if filetype <> 1
    print "Sorry, you cannot run GETOFF on old format container files"
    return
  end if
  if name_prefix2 = "UNKNOWN"
    print "You must define IRCAM data file before running GETOFF"
    print "Use command SETFILE."
    return
  else
    get plt2d contname (contname)
    print "Current IRCAM file prefix = " (name_prefix)
    print "Current IRCAM file suffix = " (rosuf)
  end if
  get plt2d platscal (platscal)
  pref = name_prefix
  if rosuf = "NONE"
    suff = ""
  else
    suff = rosuf
  end if
  yn1 = undefined(v1)
  yn2 = undefined(v2)
  if yn1
    print "Enter start observation number ? "
    asknum (value2) "Start           \1\ ? "
  else
    value2 = v1
  end if
  if yn2
    print "Enter number of images to be inspected ? "
    asknum (numin) "Number of Images \1\ ? "
  else
    numin = v2
  end if
  value3 = value2+numin-1
  print "Getting RA,DEC offsets from input images..."
  fclose_c
  delfile ro.off
  create cfile "ro.off"
  iflag = 0
  loop for dummy = (value2) to (value3)
    tim = pref & dummy & suff
    print "  Image " (tim)
    obeyw plt2d ropars (tim)
    get plt2d ra_off        (raoff)
    get plt2d dec_off       (decoff)
    raoff = real(integer(raoff*100.0)/100.0)
    decoff = real(integer(decoff*100.0)/100.0)
    if iflag = 0
      iflag = 1
      ra0 = raoff
      dec0 = decoff
{      raoff = 0.0
{      decoff = 0.0
    else
{      raoff = raoff-ra0
{      decoff = decoff-dec0
      raoff = raoff
      decoff = decoff
    end if
    cra = raoff:10:3
    cdec = decoff:10:3
    dline = cra & cdec
    write cfile (dline)
  end loop
  fclose_c
  print " "
  ! more ro.off
  print " "
  print "Offset file from data headers called ro.off"
  print " "
end proc
