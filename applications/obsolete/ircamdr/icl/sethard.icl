{ PROCEDURE SETHARD : defines hardcopy device to be used in HARDCOPY
proc sethard value1
  yn = undefined(value1)
  if yn
    promptfor = 1
  else
    max_workstn = variable(max_workstn)
    if value1 < 1 or value1 > max_workstn
      promptfor = 1
    else
      promptfor = 0
    end if
  end if
  if promptfor = 1
    print "Plotting workstations currently supported : "
    print "Canon laser printer (P or L)            = 10 or 11"
    print "Canon TeX laser printer (P or L)        = 12 or 13"
    print "Postscript printer (P or L)             = 14 or 15"
    print "Encapsulated Postscript (P or L)        = 16 or 17"
    print "Colour Postscript (P or L)              = 18 or 19"
    print "Encapsulated Colour Postscript (P or L) = 20 or 21"
    print "Give workstation number for plotting ? "
    asknum (hard_device) "Workstation Number (10-21) \15\ : "
  else
    hard_device = value1
  end if
  send plt2d set hard_device (hard_device)
  print "Hardcopy device is selected = " (hard_device)
end proc

