{ PROCEDURE FORMNAME2 : forms the name of an IRCAM observation element
proc formname2 obsnum name_out
  if obsnum < 1 or obsnum > 10000
    print "Error, Illegal OBSERVATION NUMBER entered ..." (obsnum)
    print "       should be in the range 1 to 10000 ..."
    return
  end if
  get plt2d rosuf (rosuf)
  rosuf = upcase(rosuf)
  if rosuf = "NONE"
    rosuf = ""
  end if
  get plt2d name_prefix (name_prefix)
  name_out = name_prefix & obsnum & rosuf
end proc
