{ PROCEDURE HARDPRINT : prints hardcopy to right queue
proc hardprint hardy delit
  yn = undefined(hardy)
  copfile fort.1 ircamdr.ps
  delfile fort.1
  if not yn
    if hardy = 14 or hardy = 15
      print " "
      print "Postscript hardcopy output to disk file ircamdr.ps"
      print " "
      print "Issue command :"
      print "                IrcamDR > lp_ircamdr"
      print " to spool file to printer using Unix command lp"
      print " "
    end if
  end if
end proc

