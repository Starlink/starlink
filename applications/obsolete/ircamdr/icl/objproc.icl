{ Procedure OBJPROC : auto scans RO lis file and creates batch file
proc objproc
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
  copfile $LIRCAMDIR/bad_objects.list objects.list
  copfile (rbfile) creobj.tmp
  delfile creobj.icl
  delfile header.txt
  ! $LIRCAMDIR/creobj
  delfile creobj.tmp
  typfile creobj.icl
  asklog (yn) "Execute the above batch file (Yes or No) \N\ ? "
  if yn = 1
    load creobj.icl
  else
    print "OK. to execute batch file later type:"
    print "  load creobj.icl <cr>"
    print "from the IrcamDR > prompt"
  end if
end proc
