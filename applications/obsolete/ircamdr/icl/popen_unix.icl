{ PROCEDURE POPEN : opens plotting on a workstation
proc popen value1
  max_workstn = variable(max_workstn)
  yn = undefined(value1)
  if yn
    promptfor = 1
  else
    worknum = value1
    max_workstn = variable(max_workstn)
    if worknum < 0 or worknum > max_workstn
      promptfor = 1
    else
      promptfor = 0
    end if
  end if
  if promptfor = 1
    print "Plotting workstations currently supported : "
    print "X-Windows base device                   =  1"
    print "X-Windows 2 device                      =  2"
    print "X-Windows 3 device                      =  3"
    print "X-Windows 4 device                      =  4"
    print "Canon laser printer (P or L)            = 10 or 11"
    print "Canon TeX laser printer (P or L)        = 12 or 13"
    print "Postscript printer (P or L)             = 14 or 15"
    print "Encapsulated Postscript (P or L)        = 16 or 17"
    print "Colour Postscript (P or L)              = 18 or 19"
    print "Encapsulated Colour Postscript (P or L) = 20 or 21"
    print "Give workstation number for plotting ? "
    asknum (worknum) "Workstation Number (0=NONE AT THE MOMENT) \0\ : "
  end if
  loadw $LIRCAMDIR/plt2d plt2d
{  obeyw plt2d welcome
  host = variable(host)
  host = upcase(host)
  send plt2d set host (host)
  if worknum = 0
    print "O.K. no workstation selected at the moment"
    send plt2d set worknum (worknum)
  else
    if worknum < 0 or worknum > max_workstn
      print "Error, Invalid workstation selected, number = " (worknum)
      return
    end if
    worknam = "UNKNOWN"
    if worknum = 1
      worknam = "X-WINDOWS"
      image_workstn = 1
      cursor_workstn = 1
      hard_workstn = -999
    else if worknum = 2
      worknam = "X-WINDOWS2"
      image_workstn = 1
      cursor_workstn = 1
      hard_workstn = -999
    else if worknum = 3
      worknam = "X-WINDOWS3"
      image_workstn = 1
      cursor_workstn = 1
      hard_workstn = -999
    else if worknum = 4
      worknam = "X-WINDOWS4"
      image_workstn = 1
      cursor_workstn = 1
      hard_workstn = -999
    else if worknum = 5
      worknam = "UNKNOWN"
      image_workstn = -999
      cursor_workstn = -999
      hard_workstn = -999
    else if worknum = 6
      worknam = "UNKNOWN"
      image_workstn = -999
      cursor_workstn = -999
      hard_workstn = -999
    else if worknum = 7
      worknam = "UNKNOWN"
      image_workstn = -999
      cursor_workstn = -999
      hard_workstn = -999
    else if worknum = 8
      worknam = "UNKNOWN"
      image_workstn = -999
      cursor_workstn = -999
      hard_workstn = -999
    else if worknum = 9
      worknam = "UNKNOWN"
      image_workstn = -999
      cursor_workstn = -999
      hard_workstn = -999
    else if worknum = 10
      worknam = "CANON_P"
      image_workstn = 1
      cursor_workstn = -999
      hard_workstn = 1
    else if worknum = 11
      worknam = "CANON_L"
      image_workstn = 1
      cursor_workstn = -999
      hard_workstn = 1
    else if worknum = 12
      worknam = "CANON_PTEX"
      image_workstn = 1
      cursor_workstn = -999
      hard_workstn = 1
    else if worknum = 13
      worknam = "CANON_LTEX"
      image_workstn = 1
      cursor_workstn = -999
      hard_workstn = 1
    else if worknum = 14
      worknam = "PS_PORTRAIT"
      image_workstn = 1
      cursor_workstn = -999
      hard_workstn = 1
    else if worknum = 15
      worknam = "PS_LANDSCAPE"
      image_workstn = 1
      cursor_workstn = -999
      hard_workstn = 1
    else if worknum = 16
      worknam = "EPSP"
      image_workstn = 1
      cursor_workstn = -999
      hard_workstn = 1
    else if worknum = 17
      worknam = "EPSL"
      image_workstn = 1
      cursor_workstn = -999
      hard_workstn = 1
    else if worknum = 18
      worknam = "CPSP"
      image_workstn = 1
      cursor_workstn = -999
      hard_workstn = 1
    else if worknum = 19
      worknam = "CPSL"
      image_workstn = 1
      cursor_workstn = -999
      hard_workstn = 1
    else if worknum = 20
      worknam = "ECPSP"
      image_workstn = 1
      cursor_workstn = -999
      hard_workstn = 1
    else if worknum = 21
      worknam = "ECPSL"
      image_workstn = 1
      cursor_workstn = -999
      hard_workstn = 1
    else if worknum > 21
      worknam = "UNKNOWN"
      image_workstn = -999
      cursor_workstn = -999
      hard_workstn = -999
    end if
    send plt2d set worknam (worknam)
    send plt2d set worknum (worknum)
    send plt2d set image_workstn (image_workstn)
    send plt2d set cursor_workstn (cursor_workstn)
    send plt2d set hard_workstn (hard_workstn)
    if worknum <> 0
      obeyw plt2d open (worknam)
      get plt2d max_xsize (workxcen)
      get plt2d max_ysize (workycen)
      workxcen = (workxcen+1)/2
      workycen = (workycen+1)/2
      send plt2d set workxcen (workxcen)
      send plt2d set workycen (workycen)
      print "  Workstation centre coordinates are " (workxcen) ","(workycen)
      if cursor_workstn = 1
        print "  Workstation supports CURSORING ..."
      else
        print "  Workstation does not supports CURSORING ..."
      end if
      if image_workstn = 1
        print "  Workstation supports IMAGE DISPLAY ..."
        if worknum = 1 or worknum = 2 or worknum = 3 or worknum = 4
          get plt2d last_lut (last_lut)
          print "    Loading colour table " (last_lut) " please wait"
          obeyw plt2d coltab (last_lut)
          print "    Colour table loaded ..."
        end if
        send plt2d set magnification 0
        send plt2d set disp_mag 0
        send plt2d set contour_magnif 0
        send plt2d set cut_magnif 0
      else
        print "  Workstation does not supports IMAGE DISPLAY ..."
      end if
      print "O.K. Plotting OPEN on workstation " (worknam)
    end if
  end if
end proc

