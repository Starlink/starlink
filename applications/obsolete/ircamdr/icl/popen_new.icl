{ PROCEDURE POPEN : opens plotting on a workstation
proc popen value1
{  max_workstn = variable(max_workstn)
  max_workstn = 30
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
    print "Args or Args Overlay            =   1 or  2"
    print "T4010 or T4014                  =   3 or  4"
    print "Ikon or Ikon Overlay            =   5 or  6"
    print "Sigmex T6134 or T5688           =   7 or  8"
    print "LN03 laser printer Lo or Hi     =   9 or 10"
    print "QMS laser printer Port or Land  =  11 or 12"
    print "Canon laser printer             =  13"
    print "Vaxstation 8-plane or 2-plane   =  14 or 15"
    print "Postscript printer P/L          =  16 or 17"
    print "X-Windows device                =  18"
    print "Encapsulated Postscript P/L     =  19 or 20"
    print "Colour Postscript P/L           =  21 or 22"
    print "Give workstation number for plotting ? "
    asknum (worknum) "Workstation Number (1-22, 0=NONE) |0| : "
  end if
  loadw $LIRCAMDIR/plt2d plt2d
  if worknum = 0
    print "O.K. No workstation selected at the moment"
    send plt2d set worknum (worknum)
  else
    if worknum < 0 or worknum > max_workstn
      print "Error, Invalid workstation selected, number = " (worknum)
      return
    end if
    worknam = "''UNKNOWN''"
    if worknum = 1
      worknam = "''ARGS''"
      image_workstn = 1
      cursor_workstn = 1
      hard_workstn = -999
    else if worknum = 2
      worknam = "''ARGS_OVERLAY''"
      image_workstn = 1
      cursor_workstn = 1
      hard_workstn = -999
    else if worknum = 3
      worknam = "''T4010''"
      image_workstn = -999
      cursor_workstn = 1
      hard_workstn = -999
    else if worknum = 4
      worknam = "''T4014''"
      image_workstn = -999
      cursor_workstn = 1
      hard_workstn = -999
    else if worknum = 5
      worknam = "''IKON''"
      image_workstn = 1
      cursor_workstn = 1
      hard_workstn = -999
    else if worknum = 6
      worknam = "''IKON_OVERLAY''"
      image_workstn = 1
      cursor_workstn = 1
      hard_workstn = -999
    else if worknum = 7
      worknam = "''T6134''"
      image_workstn = 1
      cursor_workstn = 1
      hard_workstn = -999
    else if worknum = 8
      worknam = "''T5688''"
      image_workstn = 1
      cursor_workstn = 1
      hard_workstn = -999
    else if worknum = 9
      worknam = "''LN03_LO''"
      image_workstn = -999
      cursor_workstn = -999
      hard_workstn = 1
    else if worknum = 10
      worknam = "''LN03_HI''"
      image_workstn = -999
      cursor_workstn = -999
      hard_workstn = 1
    else if worknum = 11
      worknam = "''QMS_PORTRAIT''"
      image_workstn = 1
      cursor_workstn = -999
      hard_workstn = 1
    else if worknum = 12
      worknam = "''QMS_LANDSCAPE''"
      image_workstn = 1
      cursor_workstn = -999
      hard_workstn = 1
    else if worknum = 13
      worknam = "''CANON''"
      image_workstn = -999
      cursor_workstn = -999
      hard_workstn = 1
    else if worknum = 14
      worknam = "''VAXSTATION8''"
      image_workstn = 1
      cursor_workstn = 1
      hard_workstn = -999
    else if worknum = 15
      worknam = "''VAXSTATION2''"
      image_workstn = -999
      cursor_workstn = 1
      hard_workstn = -999
    else if worknum = 16
      worknam = "''PS_PORTRAIT''"
      image_workstn = 1
      cursor_workstn = -999
      hard_workstn = 1
    else if worknum = 17
      worknam = "''PS_LANDSCAPE''"
      image_workstn = 1
      cursor_workstn = -999
      hard_workstn = 1
    else if worknum = 18
      worknam = "X-WINDOWS"
      image_workstn = 1
      cursor_workstn = 1
      hard_workstn = -999
    else if worknum = 19
      worknam = "''EPSP''"
      image_workstn = 1
      cursor_workstn = -999
      hard_workstn = 1
    else if worknum = 20
      worknam = "''EPSL''"
      image_workstn = 1
      cursor_workstn = -999
      hard_workstn = 1
    else if worknum = 21
      worknam = "''CPSP''"
      image_workstn = 1
      cursor_workstn = -999
      hard_workstn = 1
    else if worknum = 22
      worknam = "''CPSL''"
      image_workstn = 1
      cursor_workstn = -999
      hard_workstn = 1
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
      workxcen = int((workxcen+1)*0.5)
      workycen = int((workycen+1)*0.5)
      send plt2d set workxcen (workxcen)
      send plt2d set workycen (workycen)
      print "  Workstation centre coordinates are " (workxcen) "," (workycen)
      if cursor_workstn = 1
        print "  Workstation supports CURSORING ..."
      else
        print "  Workstation does not supports CURSORING ..."
      end if
      if image_workstn = 1
        print "  Workstation supports IMAGE DISPLAY ..."
        if worknum = 1 or worknum = 2 or worknum = 7 or worknum = 14 ~
        or worknum = 18
          print "    Loading colour table grey please wait"
          obeyw plt2d coltab $ADAM_EXE/grey
          print "    Colour table loaded ..."
        end if
      else
        print "  Workstation does not supports IMAGE DISPLAY ..."
      end if
      print "O.K. Plotting OPEN on workstation " (worknam)
    end if
  end if
end proc

