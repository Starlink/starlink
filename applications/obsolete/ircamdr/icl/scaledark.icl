{ PROCEDURE SCALEDARK : scales dark to specific EXP-TIME/COADDS
proc scaledark
  get plt2d name_prefix (name_prefix)
  compare name_prefix "UNKNOWN" (brave_man)
  if brave_man = 1
    print "You must define IRCAM data file before running SCALEDARK"
    print "Use command SETFILE"
    return
  else
    get plt2d name_image (name_image)
    old_name = name_image
    print "Current IRCAM container file selected = " (name_prefix)
  end if
  print "Input observation number of DARK exposure to be scaled"
  asknum (value1) "Dark Number \1\ ? "
  print "Is the data NDR readout mode ? "
  asklog (brave_man) "NDR Mode (Yes or No) \Y\ ? "
  if brave_man = 0
    print "Input observation number of BIAS exposure"
    asknum (value2) "Bias Number \1\ ? "
  end if
  v2 = 1
  get_imagename (value1) (v2) (dark)
  obeyw plt2d contpars (value1)
  get plt2d number_coadds (nd)
  get plt2d exposure_time (expd)
  if brave_man = 0
    get_imagename (value2) 1 (bias)
    obeyw plt2d contpars (value2)
    get plt2d number_coadds (nb)
    get plt2d exposure_time (expb)
  end if
  print "Dark image         = " (dark)
  print "Exposure time used = " (expd) "mS"
  print "Number of coadds   = " (nd)
  if brave_man = 0
    print "Bias image         = " (bias)
    print "Exposure time used = " (expb) "mS"
    print "Number of coadds   = " (nb)
  end if
  print "Input exposure time dark is to be scaled to : "
  asknum (expn) "New Exposure Time (in seconds) \10\ ? "
  print "Input name for output scaled dark image "
  askname (out) "Output Image \SCALEDARK\ ? "
  expn = expn*1000
  fac = expn/expd
  if brave_man = 0
    print "  Scaling DARK+BIAS to /coadd..."
    obeyw rapi2d CDIV (dark) (nd) scaledark_junk1
    print "  Scaling BIAS to /coadd..."
    obeyw rapi2d CDIV (bias) (nb) scaledark_junk2
    print "  Subtracting BIAS from DARK+BIAS..."
    obeyw rapi2d SUB scaledark_junk1 scaledark_junk2 scaledark_junk3
    print "  Scaling DARK to new exposure time..."
    obeyw rapi2d CMULT scaledark_junk3 (fac) scaledark_junk4
    print "  Adding on BIAS to scaled DARK..."
    obeyw rapi2d ADD scaledark_junk4 scaledark_junk2 scaledark_junk5
    print "  Re-scaling DARK+BIAS to original DARK coadds..."
    obeyw rapi2d CMULT scaledark_junk5 (nd) (out)
    delfile scaledark_junk1.sdf
    delfile scaledark_junk2.sdf
    delfile scaledark_junk3.sdf
    delfile scaledark_junk4.sdf
    delfile scaledark_junk5.sdf
  else
    print "  Scaling DARK to new exposure time..."
    obeyw rapi2d CMULT (dark) (fac) (out)
  end if
  print "Scaled dark image called " (out)
end proc

