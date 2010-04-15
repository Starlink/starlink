{ PROCEDURE SETFILE : sets up variables needed by system
proc setfile newold newutd newsuf
  yn1 = undefined(newold)
  yn2 = undefined(newutd)
  yn3 = undefined(newsuf)
  if yn1
    print "New (RO940422) or Old (IRCAM_22APR94_1C) data format : "
    askchoice (neworold) "New or Old Format (N,O) \N\ ? "
  else
    newold2 = upcase(newold)
    if newold2 = "N"
      neworold = 1
    else
      neworold = 2
    end if
  end if
  if neworold = 1
    send plt2d set filetype 1
    if yn2
      defut = getenv("UTD")
      print "Default UT date = " (defut)
      print "UT date of files to work on (e.g. 940422) [RETURN=DEFAULT] : "
      askname (utd) "UT Date \-1\ ? "
      if utd = "-1"
        utd = defut
      end if
      utd = upcase(utd)
    else
      utd = upcase(newutd)
    end if
    setenv utd (utd)
    if utd = "NONE"
      npre = "NONE"
      print "O.K. no UT date set at present, use SETFILE to define later"
    else
      npre = "ro"&utd&"_"
    end if
    if yn3
{      print "Give SUFFIX for data file (if any, NONE=no suffix) : "
{      askname (fsuf) "File Suffix \NONE\ ? "
      fsuf = "NONE"
    else
      fsuf = newsuf
    end if
    fsuf2 = upcase(fsuf)
    if fsuf2 = "NONE"
      fsuf = "NONE"
    end if
    send plt2d set contname (npre)
    send plt2d set container (npre)
    send plt2d set name_prefix (npre)
    send plt2d set rosuf (fsuf)
  else
    send plt2d set filetype 0
    send plt2d set rosuf ' '
    if yn2
      SETPRE
    else
      SETPRE (newutd)
    end if
  end if
end proc
