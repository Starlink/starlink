{ PROCEDURE RODARKLOT : dark subtracts a number of images
proc rodarklot
  get plt2d name_prefix (pref)
  print "Data file prefix = " (pref)
  print "Enter prefix for out files : "
  askname (outpre) "Output Prefix \im_\ ? "
  print "Enter NUMBER RANGE of input image(s) to be dark subtracted :"
  asknum (nost) "Range Start  \1\ ? "
  asknum (noen) "Range End    \1\ ? "
  get plt2d rosuf (rosuf)
  rosuf = upcase(rosuf)
  if rosuf = "NONE"
    suff = " "
  else
    suff = rosuf
  end if
  tim = pref & nost & rosuf
  print "Enter number of DARK image :"
  asknum (numdark) "Dark Number \1\ ? "
  dark = pref & numdark & suff
  print "Dark image         = " (dark)
  obeyw plt2d ropars (dark)
  get plt2d object_name   (dobject)
  get plt2d filter        (dfilter)
  get plt2d exposure_time (dexp)
  dexp = real(dexp)
  dobject = upcase(dobject)
  dfilter = upcase(dfilter)
  dobject = substr(dobject,1,4)
  dfilter = substr(dfilter,1,6)
  if dobject = "DARK" and dfilter = "BLANKS"
    print "    Observation specified as DARK is truly a DARK"
  else
    print "Observation specified as DARK may not be a DARK, quiting"
    print "  Object name in supposed DARK image = " (dobject)
    print "  Filter name in supposed DARK image = " (dfilter)
    return
  end if
  value2 = nost
  value3 = noen
  print "Do you want to create an ASCII list of dark subtracted images ?"
  asklog (brave_man) "Create ASCII List (Yes or No) \NO\ ? "
  if brave_man = 1
    fclose_b
    delfile rodarklot.list
    create bfile "rodarklot.list"
    print "File rodarklot.list opened..."
  end if
  loop for dummy = (value2) to (value3)
    tim = pref & dummy & suff
    obeyw plt2d ropars (tim)
{    get plt2d object_name   (object)
{    get plt2d filter        (filter)
    get plt2d exposure_time (exp)
    exp = real(exp)
{    get plt2d number_exp    (nexp)
{    get plt2d airmass_start (amst)
{    get plt2d airmass_end   (amen)
{    get plt2d ra_off        (raoff)
{    get plt2d dec_off       (decoff)
{    object = upcase(object)
{    filter = upcase(filter)
{    airmass = (amst+amen)/2.0
    out = outpre & dummy & "d"
    if exp = dexp
      if brave_man = 1
        write bfile (out)
      end if
      print "Dark subtracting image " (tim) " ..."
      obeyw rapi2d SUB (tim) (dark) darklot_junk
      obeyw rapi2d CDIV darklot_junk (exp) (out)
      print "Dark subtracted image = " (out)
      print "+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++"
    else
      print "Image " (tim) " exp-time <> DARK exp-time"
      print "Not processing " (tim)
    end if
  end loop
  delfile darklot_junk.sdf
  if brave_man = 1
    fclose_b
  end if
end proc
