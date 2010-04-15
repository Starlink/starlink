{ Procedure DRB_STDPROC : auto scans RO lis file and creates batch file
proc drb_stdproc
  get plt2d name_prefix (ropref)
  print "Using RO prefix " (ropref)
  utd = substr(ropref,3,6)
  rbfile = "ro"&utd&".lis"
  fexist = file_exists(rbfile)
  if fexist
    print "RO index file " (rbfile) " exists"
  else
    print "RO index file " (rbfile) " does not exist, quiting"
    return
  end if
  copfile (rbfile) crestd.tmp
  delfile crestd.icl
  delfile header.txt
  ! $LIRCAMDIR/drb_crestd
  delfile crestd.tmp
  typfile crestd.icl
  asklog (yn) "Execute the above batch file (Yes or No) \N\ ? "
  if yn = 1
    load crestd.icl
  else
    print "OK. to execute batch file later type:"
    print "  load crestd.icl <cr>"
    print "from the IrcamDR > prompt"
  end if
end proc
