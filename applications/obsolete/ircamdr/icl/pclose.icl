{ PROCEDURE PCLOSE : closes plotting
proc pclose
  print "Closing plotting on current workstation ..."
  obeyw plt2d close
  print "O.K. plotting CLOSED ..."
  worknum = -999
  worknam = "''UNKNOWN''"
  send plt2d set worknam (worknam)
  send plt2d set worknum (worknum)
end proc

