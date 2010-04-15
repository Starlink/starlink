{ PROCEDURE STDPHO : Standard star photometry on image - concentric apertures
proc stdpho objim skyim exp filt std am pn
  numfs = 102
  numuk = 80
  get plt2d filetype (filetype)
  get plt2d image_workstn (image_workstn)
  if image_workstn <> 1
    print "STDPHO is not available from this terminal"
    return
  end if
  get plt2d filetype (filetype)
  get plt2d rosuf (rosuf)
  rosuf2 = upcase(rosuf)
  if rosuf2 = "NONE"
    rosuf = ""
  end if
  if filetype <> 1
    print "Can only run STDPHO with NEW IRCAM3 data format"
    return
  end if
  get plt2d name_prefix (name_prefix)
  if name_prefix = "UNKNOWN"
    print "You must run SETPRE to define image prefix before running"
    print "this procedure"
    return
  end if
  fexists = file_exists("$LIRCAMDIR/ukirt.dat")
  if fexists
    print "Found $LIRCAMDIR/ukirt.dat"
  else
    print "File $LIRCAMDIR/ukirt.dat does not exist - quiting"
    return
  end if
  fexists = file_exists("$LIRCAMDIR/fs.dat")
  if fexists
    print "Found $LIRCAMDIR/fs.dat"
  else
    print "File $LIRCAMDIR/fs.dat does not exist - quiting"
    return
  end if
  get plt2d disp_ap1 (disp_ap1)
  get plt2d disp_ap2 (disp_ap2)
  get plt2d disp_ap3 (disp_ap3)
  aper = disp_ap1
  aper2 = disp_ap2
  aper3 = disp_ap3
  get plt2d disp_mag (disp_mag)
  get plt2d disp_ns (disp_ns)
  yn1 = undefined(objim)
  yn2 = undefined(skyim)
  yn3 = undefined(exp)
  yn4 = undefined(filt)
  yn5 = undefined(std)
  yn6 = undefined(am)
  yn7 = undefined(pn)
  if yn1
    print "Give OBJECT image number (0 for full image name) : "
    asknum (objim2) "Object Number \0\ ? "
  else
    objim2 = objim
  end if
  if objim2 = 0
    if yn2
      print "Give full IMAGE NAME : "
      askname (fullim) "Image Name \junk\ ? "
    else
      fullim = skyim
    end if
  else
    objnam = name_prefix & objim2 & rosuf
  end if
  if objim2 <> 0
    if yn2
      print "Give SKY image number : "
      asknum (skyim2) "Sky Number ? "
    else
      skyim2 = skyim
    end if
    obeyw plt2d ropars (objnam)
    get plt2d object_name   (object)
    get plt2d filter        (filter)
    get plt2d exposure_time (expo)
    get plt2d airmass_start (amst)
    get plt2d airmass_end   (amen)
    object = upcase(object)
    filter = upcase(filter)
    airmass = (amst+amen)/2.0
    print "Standard star name  = " (object)
    print "Filter used         = " (filter)
    print "Exposure time (sec) = " (expo)
    print "Airmass of Obs.     = " (airmass)
    skynam = name_prefix & skyim2 & rosuf
    roout = "f" & objim2 & "m" & skyim2
    print "Subtracting images " (objnam) " and " (skynam)
    obeyw rapi2d sub (objnam) (skynam) (roout)
    print "Difference image = " (roout)
  else
    skyim2 = 0
    roout = fullim
  end if
  send plt2d set name_image (roout)
  send plt2d set cursor_image (roout)
  if yn3
    if objim2 = 0
      print "Exposure time of image " (roout) " (in seconds) :"
      asknum (exp2) "Exposure time \1.0\ ? "
    else
      exp2 = expo
    end if
  else
    exp2 = exp
  end if
  if yn4
    if objim2 = 0
      print "Filter used (J,H,K,nbL,Lp,nbM) : "
      askname (filtn) "Filter Used \K\ ? "
    else
      filtn = substr(filter,1,3)
    end if
  else
    filtn = filt
  end if
  filtn = upcase(filtn)
  if filtn = "J"
    amc = 0.1
  else if filtn = "H"
    amc = 0.06
  else if filtn = "K"
    amc = 0.08
  else if filtn = "NBL"
    amc = 0.09
  else if filtn = "LP"
    amc = 0.09
  else if filtn = "NBM"
    amc = 0.0
  else
    amc = 0.0
  end if
  if yn5
    if objim2 = 0
      print "Give name of STANDARD STAR observed (e.g. FS33,HD18881) : "
      askname (s2) "Standard Star ? "
    else
      s2 = object
    end if
  else
    s2 = std
  end if
  s2 = upcase(s2)
  if yn6
    if objim2 = 0
      print "Give airmass of standard star observation :"
      asknum (am2) "Airmass \1.0\ ? "
    else
      am2 = airmass
    end if
  else
    am2 = am
  end if
  if yn7
    posneg = "P"
  else
    posneg = upcase(pn)
    posneg = substr(posneg,1,1)
  end if
  fclose_c
  st = getenv("LIRCAMDIR")
  st = st & "/fs.dat"
  open cfile (st)
  read cfile (dline)
  read cfile (dline)
  actmag2 = -99.99
  loop for dummy = 1 to numfs
    read cfile (dline)
    sname = dline
    readr cfile (jmag) (hmag) (kmag) (jmh) (jmk)
    if s2 = sname
      if filtn = "J"
        actmag2 = jmag
      else if filtn = "H"
        actmag2 = hmag
      else if filtn = "K"
        actmag2 = kmag
      else if filtn = "NBL"
        print "ERROR, this magnitude is not defined for standard " (s2)
        return
      else if filtn = "LP"
        print "ERROR, this magnitude is not defined for standard " (s2)
        return
      else if filtn = "NBM"
        print "ERROR, this magnitude is not defined for standard " (s2)
        return
      else
        print "ERROR, unknown filter x" (filtn) "x"
        return
      end if
      sname=substr(sname,1,5)
      print "Magnitude of standard " (sname) " : " (filtn) " = " (actmag2)
      break
    end if
  end loop
  fclose_c
  if actmag2 = -99.99
    print "Not a UKIRT FAINT STANDARD, checking BRIGHT STANDARD catalogue"
    fclose_c
    st = getenv("LIRCAMDIR")
    st = st & "/ukirt.dat"
    open cfile (st)
    read cfile (dline)
    read cfile (dline)
    actmag2 = -99.99
    loop for dummy = 1 to numuk
      read cfile (dline)
      sname = dline
      readr cfile (jmag) (hmag) (kmag) (lmag) (mmag)
      if s2 = sname
        if filtn = "J"
          actmag2 = jmag
        else if filtn = "H"
          actmag2 = hmag
        else if filtn = "K"
          actmag2 = kmag
        else if filtn = "NBL"
          actmag2 = lmag
        else if filtn = "LP"
          actmag2 = lmag
        else if filtn = "NBM"
          actmag2 = mmag
        else
          print "ERROR, unknown filter " (filtn)
          return
        end if
        print "Magnitude of standard " (sname) " : " (filtn) " = " (actmag2)
        break
      end if
    end loop
    fclose_c
    if actmag2 = -99.99
      print "Not a UKIRT BRIGHT STANDARD either, enter magnitude for star : "
      asknum (actmag2) "Actual Magnitude \0.0\ ? "
      sname = "UNKNOWN"
    end if
  end if
  get plt2d im_xcen (workxcen)
  get plt2d im_ycen (workycen)
  if workxcen = 0
    get plt2d max_xsize (workxcen)
    workxcen = workxcen/2.0
  end if
  if workycen = 0
    get plt2d max_ysize (workycen)
    workycen = workycen/2.0
  end if
  obeyw plt2d clear
  print "Plotting image " (roout)
  send plt2d set magnification (disp_mag)
  obeyw plt2d nsigma (roout)
  get plt2d im_yen (value1)
  value1 = value1+30
  obeyw plt2d comment (roout) (workxcen) (value1) 20
  get plt2d platscal (scale)
  if scale = 1.0
    print "Give image pixel scale (arcsec/pixel) : "
    asknum (scale) "Pixel Scale \0.286\ ? "
    send plt2d set platscal (scale)
    send plt2d set arcsec_pixel (scale)
  else
    print "Plate scale = " (scale) " arcsec/pixel"
  end if
  diam = int(aper/scale+0.5)
  zp = 0
  if filtn = "J"
    zpfile = "zpj.icl"
  end if
  if filtn = "H"
    zpfile = "zph.icl"
  end if
  if filtn = "K"
    zpfile = "zpk.icl"
  end if
  if filtn = "NBL"
    zpfile = "zpnbl.icl"
  end if
  if filtn = "LP"
    zpfile = "zplp.icl"
  end if
  if filtn = "NBM"
    zpfile = "zpnbm.icl"
  end if
  send plt2d set cursor_image (roout)
  print "Select the STAR with cursor ..."
  obeyw plt2d cursor
  get plt2d x_cur_pixel (xpix)
  get plt2d y_cur_pixel (ypix)
  xpix = integer(xpix)
  ypix = integer(ypix)
  xst = xpix-10
  yst = ypix-10
  xst = integer(xst)
  yst = integer(yst)
  fclose_b
  delfile (zpfile)
  create bfile (zpfile)
  obeyw rapi2d HISTO (roout) (xst) (yst) 20 20 \
  getpar glob histo_max (maxval)
  getpar glob histo_min (minval)
  if posneg <> "N"
    getpar glob histo_xmax (xobj)
    getpar glob histo_ymax (yobj)
    print "Maximum signal = " (maxval) " at pixel " (xobj) "," (yobj)
  else
    getpar glob histo_xmin (xobj)
    getpar glob histo_ymin (yobj)
    print "Minimum signal = " (minval) " at pixel " (xobj) "," (yobj)
  end if
  obeyw plt2d circle (xobj) (yobj) (aper)
  obeyw plt2d circle (xobj) (yobj) (aper2)
  obeyw plt2d circle (xobj) (yobj) (aper3)
  obeyw rapi2d APERADD (roout) (xobj) (yobj) (aper) (scale) "N" "Y"
  getpar glob aperadd_total (objval)
  getpar glob aperadd_numpix (nob)
  obeyw rapi2d APERADD (roout) (xobj) (yobj) (aper2) (scale) "N" "Y"
  getpar glob aperadd_total (skyval)
  getpar glob aperadd_numpix (nos)
  obeyw rapi2d APERADD (roout) (xobj) (yobj) (aper3) (scale) "N" "Y"
  getpar glob aperadd_total (skyval2)
  getpar glob aperadd_numpix (nos2)
  skypp = ((skyval2-skyval)/(nos2-nos))/exp2
  objpp = objval/exp2
  objval = (objval-((skyval2-skyval)*(nob/(nos2-nos))))/exp2
  mag = zp - 2.5*(log(abs(objval))/log(10))
  if objval < 0
    print "WARNING : OBJECT-SKY has NEGATIVE value, absolute value used"
  end if
  zerop = abs(mag)+actmag2
  zeropc = zerop-amc*(am2-1.0)
  print "  Object counts/sec in aperture   = " (objpp)
  print "  Sky/pixel/sec in sky annulus    = " (skypp)
  print "  Star-Sky                        = " (objval)
  print " "
  print "  Filter                          = " (filtn)
  print "  Standard star                   = " (sname)
  if sname <> "UNKNOWN"
    print "  Actual magnitude in filter      = " (actmag2)
  end if
  print " "
  print "  Zeropoint (=1DN/sec) at airmass " (am2) " = " (zerop)
  print "  Zeropoint (=1DN/sec) at unit airmass = " (zeropc)
  print " "
  zpj = -99.99
  zph = -99.99
  zpk = -99.99
  zpnbl = -99.99
  zplp = -99.99
  zpnbm = -99.99
  if filtn = "J"
    dline = "send plt2d set zeroj " & (zerop)
    send plt2d set zeroj (zerop)
    zpj = zerop
  end if
  if filtn = "H"
    dline = "send plt2d set zeroh " & (zerop)
    send plt2d set zeroh (zerop)
    zph = zerop
  end if
  if filtn = "K"
    dline = "send plt2d set zerok " & (zerop)
    send plt2d set zerok (zerop)
    zpk = zerop
  end if
  if filtn = "NBL"
    dline = "send plt2d set zeronbl " & (zerop)
    send plt2d set zeronbl (zerop)
    zpnbl = zerop
  end if
  if filtn = "LP"
    dline = "send plt2d set zerolp " & (zerop)
    send plt2d set zerolp (zerop)
    zplp = zerop
  end if
  if filtn = "NBM"
    dline = "send plt2d set zeronbm " & (zerop)
    send plt2d set zeronbm (zerop)
    zpnbm = zerop
  end if
  write bfile (dline)
  fclose_b
  node = getenv("SYSNODE")
  node = upcase(node)
  if node = "IRTDR::"
    zpfile = file_exists("IRCAM_ENG:IRCAM3_ZEROPOINTS.RESULTS")
    fclose_a
    if zpfile
      append afile "IRCAM_ENG:IRCAM3_ZEROPOINTS.RESULTS"
    else
      create afile "IRCAM_ENG:IRCAM3_ZEROPOINTS.RESULTS"
      dline = "UTD     STAR           AM     AP1    AP2    AP3   " ~
        & "PSCAL     ZP(J)     ZP(H)     ZP(K)   ZP(NBL)    ZP(LP)   ZP(NBM)"
      write afile (dline)
    end if
    utd = get_symbol("utd")
    czpj = zpj:10:2
    czph = zph:10:2
    czpk = zpk:10:2
    czpnbl = zpnbl:10:2
    czplp = zplp:10:2
    czpnbm = zpnbm:10:2
    am2 = real(am2)
    cam = am2:8:2
    disp_ap1 = real(disp_ap1)
    disp_ap2 = real(disp_ap2)
    disp_ap3 = real(disp_ap3)
    scale = real(scale)
    cd1 = disp_ap1:7:1
    cd2 = disp_ap2:7:1
    cd3 = disp_ap3:7:1
    carc = scale:8:3
    dline = utd&"  "&s2&"      "&cam&cd1&cd2&cd3&carc&czpj&czph&czpk&~
      czpnbl&czplp&czpnbm
    write afile (dline)
    fclose_a
  end if
end proc
