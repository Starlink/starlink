{ PROCEDURE FORMNAME : forms the name of an IRCAM observation element
proc formname obsnum image_type name_out
  if obsnum < 1 or obsnum > 10000
    print "Error, Illegal OBSERVATION NUMBER entered ..." (obsnum)
    print "       should be in the range 1 to 10000 ..."
    return
  end if
  if image_type < 1 or image_type > 4
    print "Error, Illegal IMAGE TYPE entered ..." (image_type)
    return
  end if
  tochar (obsnum) (obs_char)
  get plt2d name_prefix (name_prefix)
  concat (name_prefix) (obs_char) (st1)
  if image_type = 1
    concat (st1) ").PHASEA" (name_out)
  else if image_type = 2
    concat (st1) ").PHASEB" (name_out)
  else if image_type = 3
    concat (st1) ").KTCA" (name_out)
  else if image_type = 4
    concat (st1) ").KTCB" (name_out)
  end if
end proc
