{ PROCEDURE STDRED : reduces a number of STARE/ND_STARE images + pho
proc drb_stred var1 var2 var3 var4 var5
  print "This is DRB's version."
  set precision 6
  yn1 = undefined(var1)
  yn2 = undefined(var2)
  yn3 = undefined(var3)
  yn4 = undefined(var4)
  yn5 = undefined(var5)
  numfs = 102
  numuk = 80
  extj = 0.100
  exth = 0.060
  extk = 0.080
  extnbl = 0.090
  extlp = 0.090
  extnbm = 0.0
  porn = 1
  get plt2d filetype (filetype)
  get plt2d image_workstn (imwork)
  get plt2d rosuf (rosuf)
  rosuf2 = upcase(rosuf)
  if rosuf2 = "NONE"
    suff = ""
  else
    suff = rosuf
  end if
  get plt2d name_prefix (name_prefix)
  if filetype <> 1
    print "Sorry, you cannot run STDRED on old format container files"
    return
  end if
  if name_prefix = "UNKNOWN"
    print "You must define IRCAM data file prefix before running STDRED"
    print "Use command SETFILE"
    return
  else
    get plt2d contname (contname)
  end if
  if imwork = 1
    obeyw plt2d line_width 2
  end if
  get plt2d std_select (std_sel)
  get plt2d std_red (std_red)
  std_red = upcase(std_red)
  get plt2d std_sky (std_sky)
  std_sky = upcase(std_sky)
  get plt2d std_see (std_see)
  std_see = upcase(std_see)
  get plt2d std_phowhat (std_phowhat)
  std_phowhat = upcase(std_phowhat)
  if std_red = "Y"
    print "Current IRCAM file prefix = " (name_prefix)
    print "Current IRCAM file suffix = " (rosuf)
    pref = name_prefix
    yn = undefined(var2)
    if yn
      get plt2d std_noseq (numin)
    else
      numin = var2
    end if
    if yn1
      print "Enter start observation no. of sequence of " (numin) " ? "
      asknum (value2) "Start           \1\ ? "
    else
      value2 = var1
    end if
    value3 = value2+numin-1
    tim = pref & value2 & suff
    obeyw plt2d ropars (tim)
    get plt2d object_name   (object)
    get plt2d filter        (zfiltn)
    print "Object name in RO file " (tim) " = " (object)
    print "Filter = " (zfiltn)
    zfiltn = upcase(zfiltn)
    zfiltn2 = substr(zfiltn,1,1)
    if zfiltn2 = "N"
      zfiltn2 = substr(zfiltn,1,3)
    end if
    if zfiltn2 = "L"
      zfiltn2 = substr(zfiltn,1,2)
    end if
    obj2 = " "
    delfile $ADAM_USER/GLOBAL.sdf
    valstr (object) (obj2)
    fclose_c
    st = getenv("LIRCAMDIR")
    st1 = st & "/fs.dat"
    open cfile (st1)
    read cfile (dline)
    read cfile (dline)
    actmag2 = -99.99
    loop for dummy = 1 to numfs
      read cfile (dline)
      sname = dline
      readr cfile (jmag) (hmag) (kmag) (jmh) (jmk)
      if upcase(obj2) = upcase(sname)
        if zfiltn2 = "J"
          actmag2 = jmag
        else if zfiltn2 = "H"
          actmag2 = hmag
        else if zfiltn2 = "K"
          actmag2 = kmag
        else if zfiltn2 = "NBL"
          print "This magnitude is not defined for standard " (obj2)
        else if zfiltn2 = "LP"
          print "This magnitude is not defined for standard " (obj2)
        else if zfiltn2 = "NBM"
          print "This magnitude is not defined for standard " (obj2)
        else
          print "Unknown filter x" (zfiltn2) "x"
        end if
        sname=substr(sname,1,5)
        print "Magnitude of standard " (sname) " : " (zfiltn2) " = " (actmag2)
        break
      end if
    end loop
    fclose_c
    if actmag2 = -99.99
      print "Not a UKIRT FAINT STANDARD, checking BRIGHT STANDARD catalogue"
      fclose_c
      st = getenv("LIRCAMDIR")
      st1 = st & "/ukirt.dat"
      open cfile (st1)
      read cfile (dline)
      read cfile (dline)
      actmag2 = -99.99
      loop for dummy = 1 to numuk
        read cfile (dline)
        sname = dline
        readr cfile (jmag) (hmag) (kmag) (lmag) (mmag)
        if upcase(obj2) = upcase(sname)
          if zfiltn2 = "J"
            actmag2 = jmag
          else if zfiltn2 = "H"
            actmag2 = hmag
          else if zfiltn2 = "K"
            actmag2 = kmag
          else if zfiltn2 = "NBL"
            actmag2 = lmag
          else if zfiltn2 = "LP"
            actmag2 = lmag
          else if zfiltn2 = "NBM"
            actmag2 = mmag
          else
            print "Unknown filter " (zfiltn2)
          end if
          print "Magnitude of standard " (sname) " : " (zfiltn2) " = " (actmag2)
          break
        end if
      end loop
      fclose_c
      if actmag2 = -99.99
        print "Not a UKIRT BRIGHT STANDARD either... "
        sname = "UNKNOWN"
      else
        actmag2 = real(actmag2)
      end if
    end if
    init = obj2 & "_"
    ynd = undefined(var3)
    if ynd
      if zfiltn2 = "J"
        obs = integer(value2-1)
      else if zfiltn2 = "H"
        obs = integer(value2-1-numin)
      else if zfiltn2 = "K"
        obs = integer(value2-1-numin-numin)
      else if zfiltn2 = "NBL"
        obs = integer(value2-1)
      else if zfiltn2 = "NBM"
        obs = integer(value2-1)
      else
        obs = 0
      end if
      if obs = 0
        print "No idea what obs no. the DARK is, sorry, please enter it below"
        asknum (obs) "Dark No. ? "
      end if
      print "Assuming DARK observation is no. " (obs) " - checking"
    else
      obs = var3
      print "Checking DARK observation no. " (obs)
    end if
    dark = pref & obs & suff
    obeyw plt2d ropars (dark)
    get plt2d object_name   (dobject)
    get plt2d filter        (dfilter)
    get plt2d exposure_time (dexp)
    dobject = upcase(dobject)
    dfilter = upcase(dfilter)
    dobject = substr(dobject,1,4)
    dfilter = substr(dfilter,1,6)
    if dobject = "DARK" and dfilter = "BLANKS"
      print "Observation specified as DARK is truly a DARK"
    else
      print "Observation specified as DARK may not be a DARK, quiting"
      print "  Object name in supposed DARK image = " (dobject)
      print "  Filter name in supposed DARK image = " (dfilter)
      print " "
      print "Enter DARK observation number manually : "
      asknum (obs) "Dark Observation Number ? "
      dark = pref & obs & suff
      obeyw plt2d ropars (dark)
      get plt2d object_name   (dobject)
      get plt2d filter        (dfilter)
      get plt2d exposure_time (dexp)
      dobject = upcase(dobject)
      dfilter = upcase(dfilter)
      dobject = substr(dobject,1,4)
      dfilter = substr(dfilter,1,6)
      if dobject = "DARK" and dfilter = "BLANKS"
        print "Observation specified as DARK is truly a DARK"
      else
        print "Error, that is not a DARK either..."
        print "Quiting"
        return
      end if
    end if
    nd = 1
    tim = pref & value2 & suff
    obeyw plt2d ropars (tim)
    get plt2d object_name   (object)
    get plt2d filter        (filter)
    get plt2d exposure_time (exp)
    get plt2d number_exp    (nexp)
    get plt2d airmass_start (amst)
    get plt2d airmass_end   (amen)
    get plt2d ra_off        (raoff)
    get plt2d dec_off       (decoff)
    get plt2d utstart       (utst1)
    get plt2d utend         (uten1)
    get plt2d pix_size      (platscal1)
    object = upcase(object)
    filter = upcase(filter)
    phofilt = filter
    airmass1 = real((amst+amen)/2.0)
    ut1 = real((utst1+uten1/2.0))
    platscal = real(platscal1)
    print "Image pixel scale = " (platscal)
    send plt2d set arcsec_pixel (platscal)
    send plt2d set platscal (platscal)
    if zfiltn2 = "J"
      get plt2d ffj (ff)
      if ff = "@NONE"
        print "No flat-field defined for filter J, using median option"
        medorsep = 1
      else
        medorsep = 0
        print "Using flat-field image " (ff)
      end if
    else if zfiltn2 = "H"
      get plt2d ffh (ff)
      if ff = "@NONE"
        print "No flat-field defined for filter H, using median option"
        medorsep = 1
      else
        medorsep = 0
        print "Using flat-field image " (ff)
      end if
    else if zfiltn2 = "K"
      get plt2d ffk (ff)
      if ff = "@NONE"
        print "No flat-field defined for filter K, using median option"
        medorsep = 1
      else
        medorsep = 0
        print "Using flat-field image " (ff)
      end if
    else if zfiltn2 = "NBL"
      get plt2d ffnbl (ff)
      if ff = "@NONE"
        print "No flat-field defined for filter nbL, using median option"
        medorsep = 1
      else
        medorsep = 0
        print "Using flat-field image " (ff)
      end if
    else if zfiltn2 = "LP"
      get plt2d fflp (ff)
      if ff = "@NONE"
        print "No flat-field defined for filter Lp, using median option"
        medorsep = 1
      else
        medorsep = 0
        print "Using flat-field image " (ff)
      end if
    else if zfiltn2 = "NBM"
      get plt2d ffnbm (ff)
      if ff = "@NONE"
        print "No flat-field defined for filter nbM, using median option"
        medorsep = 1
      else
        medorsep = 0
        print "Using flat-field image " (ff)
      end if
    else
      print "No flat-field defined for this filter, using median option"
      medorsep = 1
    end if
    if medorsep = 0
      obeyw rapi2d shsize (ff)
      getpar glob shsize_xdim (xdimff)
      getpar glob shsize_ydim (ydimff)
      print "Flat-field image is of size " (xdimff) (ydimff)
    end if
    wantair = 0
    wantmos = 1
    tim = pref & value3 & suff
    obeyw plt2d ropars (tim)
    get plt2d object_name   (object2)
    get plt2d filter        (filter2)
    get plt2d exposure_time (exp2)
    get plt2d number_exp    (nexp2)
    get plt2d airmass_start (amst)
    get plt2d airmass_end   (amen)
    get plt2d ra_off        (raoff)
    get plt2d dec_off       (decoff)
    get plt2d utstart       (utst2)
    get plt2d utend         (uten2)
    get plt2d pix_size      (platscal1)
    airmass2 = real((amst+amen)/2.0)
    ut2 = real((utst2+uten2)/2.0)
    meanam = real((airmass1+airmass2)/2.0)
    meanut = real((ut1+ut2)/2.0)
    print "Getting RA,DEC offsets from input images..."
    fclose_c
    delfile stdred.off
    create cfile "stdred.off"
    print "RA,DEC offsets in image sequence are : "
    loop for dummy = (value2) to (value3)
      tim = pref & dummy & suff
      obeyw plt2d ropars (tim)
      get plt2d ra_off        (raoff)
      get plt2d dec_off       (decoff)
      raoff = real(integer(raoff*100.0)/100.0)
      decoff = real(integer(decoff*100.0)/100.0)
      cra = raoff:10:3
      cdec = decoff:10:3
      dline = cra & cdec
      write cfile (dline)
      print "  Image " (tim) " : " (dline)
    end loop
    fclose_c
    offwhat = 1
    final = init & value2 & "_mos"
    print "Final mosaiced image will be called " (final)
    fclose_b
    delfile darklot.list
    create bfile "darklot.list"
    print " "
    print "DARK SUBTRACTION SECTION"
    print "========================"
    loop for dummy = (value2) to (value3)
      im = pref & dummy & suff
      out = init & dummy & "d"
      write bfile (out)
      print "Dark subtracting " (im) " ..."
      nf = 1
      expf = exp
      obeyw rapi2d SUB (im) (dark) darklot_junk2
      fact = nf*expf
      invfact = 1.0/fact
      if fact <> 0
        print "Scaling SOURCE-DARK to DN/S, scaling factor   = x" (invfact)
      else
        print "Error, scaling factor to DN/S = 0"
        return
      end if
      obeyw rapi2d CDIV darklot_junk2 (fact) (out)
      print "Dark subtracted image = " (out)
    end loop
    delfile darklot_junk.sdf
    delfile darklot_junk2.sdf
    fclose_b
    if medorsep = 1
      print " "
      print "MEDIAN FILTERING FLAT-FIELD CREATION SECTION"
      print "============================================"
      flatn = "flat"
      obeyw obsrap MED3D "darklot.list" (flatn) \
      delfile med3d_work.sdf
      delfile med3d_lwork.sdf
    else
      flatn = ff
    end if
    delfile darklot.list
    print " "
    print "FLAT-FIELDING SECTION"
    print "====================="
    if medorsep = 0
      ffflag = 0
      im = init & value2 & "d"
      obeyw rapi2d shsize (im)
      getpar glob shsize_xdim (xdim)
      getpar glob shsize_ydim (ydim)
      if xdim = xdimff and ydim = ydimff
        ffflag = 0
      else
        ffflag = 1
        print "Error:   flat-field image is of size " (xdimff) (ydimff)
        print "and dark subtracted image is of size " (xdim) (ydim)
        print " "
        if xdimff = 256 and ydimff = 256
          if xdim = 128 and ydim = 128
            obeyw rapi2d PICKIM (flatn) 65 65 128 128 junkff \
            oldff = flatn
            flatn = "junkff"
          else if xdim = 64 and ydim = 64
            obeyw rapi2d PICKIM (flatn) 97 97 64 64 junkff \
            oldff = flatn
            flatn = "junkff"
          else
            print "Enter name of flat-field image of size " (xdim) (ydim)
            oldff = flatn
            askname (flatn) "Flat-field image \ff\ ? "
          end if
        else
          print "Enter name of flat-field image of size " (xdim) (ydim)
          oldff = flatn
          askname (flatn) "Flat-field image \ff\ ? "
        end if
      end if
    else
      im = init & value2 & "d"
      obeyw rapi2d shsize (im)
      getpar glob shsize_xdim (xdim)
      getpar glob shsize_ydim (ydim)
    end if
    loop for dummy = (value2) to (value3)
      im = init & dummy & "d"
      out = init & dummy & "df"
      print "Flat-fielding image " (im) " with ff " (flatn) " ..."
      obeyw rapi2d DIV (im) (flatn) (out)
      print "Flat-fielded image output to : " (out)
    end loop
    if medorsep = 1
      delfile flat.sdf
    else
      if ffflag = 1
        flatn = oldff
        delfile junkff.sdf
      end if
    end if
    print " "
    if wantair = 1
      print " "
      print "AIRMASS CORRECTION (TO AIRMASS=1) SECTION"
      print "========================================="
      loop for dummy = (value2) to (value3)
        imro = pref & dummy & suff
        im = init & dummy & "df"
        out = init & dummy & "dfa"
        obeyw plt2d ropars (imro)
        get plt2d airmass_start (amst)
        get plt2d airmass_end   (amen)
        object = upcase(object)
        filter = upcase(filter)
        filter = substr(filter,1,3)
        airm = (amst+amen)/2.0
        airm = real(airm)
        print "Airmass correcting image " (im) " using : "
        print "  filter  = " (filter)
        print "  airmass = " (airm) "..."
        obeyw obsrap AMCORR (im) (out) (filter) (airm) \
        print "Airmass corrected image output to : " (out)
      end loop
    end if
    toff = "stdred.off"
    if numin > 1
      if wantmos = 1
        print " "
        print "AUTOMOS SKY CORRECTION SECTION"
        print "=============================="
        fclose_b
        delfile images.list
        create bfile "images.list"
        loop for dummy = (value2) to (value3)
          if wantair = 1
            im = init & dummy & "dfa"
          else
            im = init & dummy & "df"
          end if
          write bfile (im)
        end loop
        fclose_b
        delfile junk
        obeyw obsrap AUTOMOS "images.list" (toff) "junk" (platscal) \
        delfile junk
      end if
    end if
    print " "
    print "BAD PIXEL MASKING SECTION"
    print "========================="
    reload_obsrap
    if xdim = 256 and ydim = 256
      glitchf = "$LIRCAMDIR/bpm_fpa42_256x256"
    else if xdim = 128 and ydim = 128
      glitchf = "$LIRCAMDIR/bpm_fpa42_128x128"
    else if xdim = 64 and ydim = 64
      glitchf = "$LIRCAMDIR/bpm_fpa42_64x64"
    else
      print "Image size = " (xdim) "x" (ydim)
      print "Input name of (matching) BAD PIXEL MASK to be used :"
      askname (glitchf) "Bad Pixel Mask \BPM_USER\ ? "
    end if
    gf = glitchf & ".sdf"
    fexist = file_exists(gf)
    if fexist
      print "File " (gf) " found"
    else
      print "File " (gf) " not found, try again"
      print "Input name of BAD PIXEL MASK to be used :"
      askname (glitchf) "Bad Pixel Mask \$LIRCAMDIR/bpm_fpa42_256x256\ ? "
      gf = glitchf & ".sdf"
      fexist = file_exists(gf)
      if fexist
        print "File " (gf) " found"
      else
        print "File " (gf) " not found, EXITING"
        return
      end if
    end if
    fclose_a
    delfile mask.list
    create afile "mask.list"
    count = 0
    loop for dummy = (value2) to (value3)
      if wantmos = 1
        if wantair = 1
          im = init & dummy & "dfaz"
        else
          im = init & dummy & "dfz"
        end if
      else
        if wantair = 1
          im = init & dummy & "dfa"
        else
          im = init & dummy & "df"
        end if
      end if
      out = im & "m"
      write afile (out)
      print "Bad pixel masking image " (im) " using " (glitchf) " ..."
      count = count + 1
      if count > 20
        count = 1
        reload_obsrap
      end if
      obeyw obsrap APPLYMASK (im) (glitchf) (out) -1.0E-20
      print "Bad pixel masked image output to : " (out)
    end loop
    fclose_a
    if numin > 1
      if wantmos = 1
        print " "
        print "QUILT MOSAICING SECTION"
        print "======================="
        obeyw obsrap CREQUILT (toff) "mask.list" (platscal) ~
          "quilt" "images.quilt"
        obeyw rapi2d QUILT F "images.quilt" (final) YES YES ~
          V -1.0E-20 \
        delfile images.list
        delfile images.list2
        delfile images.quilt
        print "O.K. final QUILTED image = " (final)
      end if
    else
      out2 = out & ".sdf"
      final2 = final & ".sdf"
      obeyw rapi2d CMULT (out2) 1.0 (final2)
      delfile (out2)
    end if
  else
    medorsep = 1
    if yn1
      print "Enter Image name for auto-photometry : "
      askname (final) "Image Name \junk\ ? "
    else
      final = var1
    end if
    if yn2
      print "Enter filter used for reduced observation : "
      askname (phofilt) "Filter (J,H,K,NBL,NBM) ? "
    else
      phofilt = var2
    end if
    phofilt = upcase(phofilt)
    if yn3
      print "Enter pixel scale of image in arcsec/pixel : "
      asknum (platscal) "Pixel Scale \0.286\ ? "
    else
      platscal = var3
    end if
    if yn4
      print "Enter mean airmass of observation : "
      asknum (meanam) "Mean Airmass \1.000\ ? "
    else
      meanam = var4
    end if
    meanam = real(meanam)
    actmag2 = -99.99
    if yn5
      print "Object +ve or -ve wrt sky ? "
      askchoice (porn) "+ve or -ve (+,-) \+\ ? "
    else
      porn = var5
    end if
  end if

{ Auto Photometry
  print " "
  print "AUTO-PHOTOMETRY SECTION"
  print "======================="
  peak2 = "Y"
  print " "
  print "Optimization of aperture position on PEAK PIXEL."
  print " "

{ Auto-photometry on INDIVIDUAL images
  if std_phowhat = "BOTH"
    reload_obsrap
    fclose_d
    open dfile "mask.list"
    iflago = 0
    calzpsq = 0
    calzpsum = 0
    loop for jj = 1 to numin
      read dfile (dline)
      ll = len(dline)
      name_image = substr(dline,1,ll)
      out = name_image&"b"
      count = count + 1
      if count > 20
        count = 1
        reload_obsrap
      end if
      print "Deglitching image " (name_image) " using magic number -1.0E-20"
      delfile $ADAM_USER/GLOBAL.sdf
      obeyw rapi2d GLITCH (name_image) (out) 'DEGLITCHED' A junk.dat -1.0E-20
      name_image = name_image&"b"
      print "Image after bad pixel removal = " (name_image)
      send plt2d set name_image (name_image)
      send plt2d set cursor_image (name_image)
      exp = 1.0
      ncoadds = 1
      filtn = phofilt
      filtn = upcase(filtn)
      send plt2d set magnification 0
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
      if imwork = 1
        obeyw plt2d clear
        print "Plotting image " (name_image)
        obeyw plt2d nsigma (name_image)
        send plt2d set cursor_cross 'NO'
        get plt2d im_yen (value1)
        value1 = value1+15
        obeyw plt2d comment (name_image) (workxcen) (value1) 20
      end if
      diffim = name_image
      scale = platscal
      get plt2d zeroj   (zeroj)
      get plt2d zeroh   (zeroh)
      get plt2d zerok   (zerok)
      get plt2d zeronbl (zeronbl)
      get plt2d zerolp  (zerolp)
      get plt2d zeronbm (zeronbm)
      total=ncoadds*exp
      zp = 0
      xfiltn = substr(filtn,1,1)
      if xfiltn = "J"
        zp = zeroj
        extval = extj
        filtn = "J"
      end if
      if xfiltn = "H"
        zp = zeroh
        extval = exth
        filtn = "H"
      end if
      if xfiltn = "K"
        zp = zerok
        extval = extk
        filtn = "K"
      end if
      xfiltn = substr(filtn,1,2)
      if xfiltn = "LP"
        zp = zerolp
        extval = extlp
        filtn = "LP"
      end if
      xfiltn = substr(filtn,1,3)
      if xfiltn = "NBL"
        zp = zeronbl
        extval = extnbl
        filtn = "NBL"
      end if
      if xfiltn = "NBM"
        zp = zeronbm
        extval = extnbm
        filtn = "NBM"
      end if
      zp = real(zp)
      print "  Zeropoint to be used       = " (zp) " for " (filtn)
      get plt2d disp_ap1 (aper1)
      get plt2d disp_ap2 (aper2)
      get plt2d disp_ap3 (aper3)
      obeyw rapi2d shsize (diffim)
      getpar glob shsize_xdim (xdim)
      getpar glob shsize_ydim (ydim)
      xobj0 = integer(xdim/2.0)
      yobj0 = integer(ydim/2.0)
      xst = integer(xobj0-32.0)
      yst = integer(yobj0-32.0)
      if std_sel = "C"
        print "Select STAR with cursor"
        send plt2d set cursor_image (diffim)
        obeyw plt2d cursor
        get plt2d x_cur_pixel (xobj)
        get plt2d y_cur_pixel (yobj)
        print "Pixel selected = " (xobj) (yobj)
        xst = integer(xobj-11.0)
        yst = integer(yobj-11.0)
        obeyw rapi2d HISTO (diffim) (xst) (yst) 20 20 \
        getpar glob histo_max (maxval)
        getpar glob histo_xmax (xobj)
        getpar glob histo_ymax (yobj)
      else
        if imwork = 1
          if iflago = 0
            xobj00 = xobj0
            yobj00 = yobj0
            xst0 = xst
            yst0 = yst
            fclose_b
            open bfile (toff)
            iflago = 1
          end if
          readr bfile (xoff) (yoff)
          print "Offsets read from file = " (xoff) (yoff)
          xobj0 = xobj00+(xoff/platscal)
          yobj0 = yobj00-(yoff/platscal)
          xst = xst0+(xoff/platscal)
          yst = yst0-(yoff/platscal)
          obeyw plt2d box (xobj0) (yobj0) 20.0 20.0 'CENTRE'
          print "Box (20x20 arcsec) for peak pixel search displayed on image"
        end if
        obeyw rapi2d HISTO (diffim) (xst) (yst) 64 64 \
        getpar glob histo_max (maxval)
        getpar glob histo_min (minval)
        getpar glob histo_xmax (xobj)
        getpar glob histo_ymax (yobj)
      end if
      print "Maximum signal of " (maxval) " found at " (xobj) (yobj)
      if imwork = 1
        obeyw plt2d circle (xobj) (yobj) (aper1)
        obeyw plt2d circle (xobj) (yobj) (aper2)
        obeyw plt2d circle (xobj) (yobj) (aper3)
      end if
      if std_see = "Y"
        print "Fitting PSF for seeing information in image " (diffim)
        fclose_e
        delfile starpos.dat
        create efile "starpos.dat"
        stc = (xobj)&" "&(yobj)
        write efile (stc)
        fclose_e
        seefile = file_exists("stdred_seeing.results")
        if seefile
          append efile "stdred_seeing.results"
        else
          create efile "stdred_seeing.results"
         a1 = "Image                        Filter    UT         "
         a2 = "FWHM    Ax.Rat    Orient     Gamma"
         dline = a1&a2
         write efile (dline)
        end if
        seeim = "@"&diffim
        psf in=(seeim) cofile=starpos.dat device=! clear=yes ~
            fwhm=(seeing) axisr=(rat) orient=(ori) gamma=(gam) \
        afwhm = real(seeing*platscal)
        rat = real(rat)
        ori = real(ori)
        gam = real(gam)
        print "PSF FWHM        = " (afwhm) "arcsec"
        print "Mean axis ratio = " (rat)
        print "Orientation     = " (ori)
        print "Gamma           = " (gam)
        nc = len(diffim)
        if nc < 24
          na = 24-nc
          diffim2 = diffim
          loop for k = 1 to na
            diffim2 = diffim2 & "."
          end loop
        else
          diffim2 = substr(diffim,1,24)
        end if
        nc = len(filtn)
        if nc < 5
          na = 5-nc
          filtn2 = filtn
          loop for k = 1 to na
            filtn2 = filtn2 & "."
          end loop
        else
          filtn2 = substr(filtn,1,5)
        end if
        cut = meanut:10:4
        cafwhm = afwhm:10:3
        crat = rat:10:2
        cori = ori:10:2
        cgam = gam:10:2
        dline = diffim2&"     "&filtn2&cut&cafwhm&crat&cori&cgam
        write efile (dline)
        fclose_e
        delfile starpos.dat
      end if
      print "Photometry apertures displayed on image"
      print "Analysing image " (diffim)
      sap1 = aper1/scale
      sap2 = aper2/scale
      sap3 = aper3/scale
      sscale = 1.0
      obeyw obsrap APERPHOT (diffim) T x T Y (xobj) (yobj) (sap1) ~
         (sap2) (sap3) (sscale) N \
      getpar glob aperphot_med2 (skypp)
      getpar glob aperphot_objsky (objval)
      getpar glob aperphot_mag (mag)
      skypp = real(skypp)
      objval = real(objval)
      mag = real(mag)
      if objval < 0
        print "WARNING : OBJECT-SKY gives negative value, absolute value used"
      end if
      print "  Zeropoint                                = " (zp)
{  print "  Object counts in aperture                = " (objpp)
      print "  Sky/pixel in sky annulus                 = " (skypp)
      print "  Star-Sky                                 = " (objval)
      print "  Inst. Magnitude                          = " (mag)
      if zp <> 0.0
        amag = zp+mag
        print "  Actual Magnitude                         = " (amag)
      end if
      print " "
      phbfile = file_exists("stdred_photometry.results")
      fclose_a
      fexist = file_exists("header.txt")
      if fexist
          fclose_c
          open cfile "header.txt"
          read cfile (dline)
          fclose_c
          lhead = len(dline)
          header = substr(dline,1,lhead)
          pheader = header
      end if
      if phbfile
        append afile "stdred_photometry.results"
      else
        create afile "stdred_photometry.results"
        fexist = file_exists("header.txt")
        if fexist
          fclose_c
          open cfile "header.txt"
          read cfile (dline)
          fclose_c
          delfile header.txt
          lhead = len(dline)
          header = substr(dline,1,lhead)
          pheader = header
          write afile (header)
          header = " "
          write afile (header)
        end if
        ast = "FILENAME                      UT      FILTER   AP1  "
        bst = "AP2   AP3    PSCAL    ZP      AIRM  ACT.MAG    OBJ-SKY  "
        cst = "  I3.MAG  CAL.ZP CAL.ZPX"
        dline = ast&bst&cst
        write afile (dline)
      end if
      czp = zp:8:3
      if zp = 0.0 and actmag2 <> -99.99
        calzp = abs(mag)+actmag2
        if meanam <> -9.99
          calzpx = calzp + (meanam-1.0)*extval
        else
          calzpx = -99.99
        end if
      else
        calzp = -99.99
        calzpx = -99.99
      end if
      if calzp <> -99.99
         calzpsq = calzpsq + calzp**2
         calzpsum = calzpsum + calzp
      else
         calzpsq = calzpsq + mag**2
         calzpsum = calzpsum + mag
      end if
      ccalzp = calzp:8:3
      ccalzpx = calzpx:8:3
      disp_ap1 = real(aper1)
      disp_ap2 = real(aper2)
      disp_ap3 = real(aper3)
      scale = real(scale)
      cd1 = disp_ap1:6:2
      cd2 = disp_ap2:6:2
      cd3 = disp_ap3:6:2
      carc = scale:8:3
      cobjval = objval:13:5
      cmag = mag:8:3
      cam = meanam:8:3
      cac = actmag2:9:3
      cut = meanut:10:4
      nc = len(diffim)
      if nc < 24
        na = 24-nc
        loop for k = 1 to na
          diffim = diffim & "."
        end loop
      else
        diffim = substr(diffim,1,24)
      end if
      if jj=1
         filena = diffim
      end if
      nc = len(filtn)
      if nc < 5
        na = 5-nc
        loop for k = 1 to na
          filtn = filtn & "."
        end loop
      else
        filtn = substr(filtn,1,5)
      end if
      if medorsep = 1
        filtn = filtn&"M"
      else
        filtn = filtn&"S"
      end if
      dline = diffim&"  "&cut&"  "&~
        filtn&cd1&cd2&cd3&carc&czp&cam&cac&cobjval&cmag&ccalzp&ccalzpx
      write afile (dline)
      fclose_a
      outd = out&".sdf"
      print "Deleting file " (outd)
      delfile (outd)
    end loop
    meancalzp = calzpsum/numin
    variance = ( calzpsq - numin*meancalzp**2)
    if ( numin = 1 or variance < 0)
       variance = 0
    else
       variance = variance/(numin - 1)
    end if
    standdev = sqrt(variance)
    if standdev > 0.1
       probfile = file_exists("photometry.problems")
       if probfile
          append afile "photometry.problems"
       else
          create afile "photometry.problems"
          if fexist
             write afile (pheader)
             write afile " "
          end if
       end if
       numstr = (numin-1):2
       standevstr = standdev:8:3
       write afile "The following files gave zero-points or instr. magnitudes~
 which had a standard deviation greater than 0.1."
       write afile (filena) "and the following " (numstr) "images have a~
 standard deviation of " (standevstr)
       if medorsep = 1
          write afile "The above images were median filtered."
       else
          write afile "The above images were seperately flat fielded with~
 the file " (flatn)
       end if
       fclose_a
    end if
    fclose_d
    fclose_b
  end if

{ Auto-photometry on MOSAIC image
  name_image = final
  send plt2d set name_image (name_image)
  send plt2d set cursor_image (name_image)
  exp = 1.0
  ncoadds = 1
  filtn = phofilt
  filtn = upcase(filtn)
  get plt2d disp_ns (phons)
  send plt2d set magnification 0
  send plt2d set sigma_level (phons)
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
  if imwork = 1
    obeyw plt2d clear
    print "Plotting image " (name_image)
    obeyw plt2d nsigma (name_image)
    send plt2d set cursor_cross 'NO'
    get plt2d im_yen (value1)
    value1 = value1+15
    obeyw plt2d comment (name_image) (workxcen) (value1) 20
  end if
  diffim = name_image
  scale = platscal
  get plt2d zeroj   (zeroj)
  get plt2d zeroh   (zeroh)
  get plt2d zerok   (zerok)
  get plt2d zeronbl (zeronbl)
  get plt2d zerolp  (zerolp)
  get plt2d zeronbm (zeronbm)
  total=ncoadds*exp
  zp = 0
  xfiltn = substr(filtn,1,1)
  if xfiltn = "J"
    zp = zeroj
    extval = extj
    filtn = "J"
  end if
  if xfiltn = "H"
    zp = zeroh
    extval = exth
    filtn = "H"
  end if
  if xfiltn = "K"
    zp = zerok
    extval = extk
    filtn = "K"
  end if
  xfiltn = substr(filtn,1,2)
  if filtn = "LP"
    zp = zerolp
    extval = extlp
    filtn = "LP"
  end if
  xfiltn = substr(filtn,1,3)
  if filtn = "NBL"
    zp = zeronbl
    extval = extnbl
    filtn = "NBL"
  end if
  if filtn = "NBM"
    zp = zeronbm
    extval = extnbm
    filtn = "NBM"
  end if
  zp = real(zp)
  print "Zeropoint to be used       = " (zp) " for " (filtn)
  get plt2d disp_ap1 (aper1)
  get plt2d disp_ap2 (aper2)
  get plt2d disp_ap3 (aper3)
  obeyw rapi2d shsize (diffim)
  getpar glob shsize_xdim (xdim)
  getpar glob shsize_ydim (ydim)
  xobj0 = integer(xdim/2.0)
  yobj0 = integer(ydim/2.0)
  xst = integer(xobj0-32.0)
  yst = integer(yobj0-32.0)
  if std_sel = "C"
    print "Select STAR with cursor"
    send plt2d set cursor_image (diffim)
    obeyw plt2d cursor
    get plt2d x_cur_pixel (xobj)
    get plt2d y_cur_pixel (yobj)
    print "Pixel selected = " (xobj) (yobj)
    xst = integer(xobj-11.0)
    yst = integer(yobj-11.0)
    obeyw rapi2d HISTO (diffim) (xst) (yst) 20 20 \
    getpar glob histo_max (maxval)
    getpar glob histo_min (minval)
    if porn = 1
      getpar glob histo_xmax (xobj)
      getpar glob histo_ymax (yobj)
      print "Maximum signal of " (maxval) " found at " (xobj) (yobj)
    else
      getpar glob histo_xmin (xobj)
      getpar glob histo_ymin (yobj)
      print "Minimum signal of " (minval) " found at " (xobj) (yobj)
    end if
  else
    if imwork = 1
      obeyw plt2d box (xobj0) (yobj0) 18.0 18.0 'CENTRE'
      print "Box (18x18 arcsec) for peak pixel search displayed on image"
    end if
    obeyw rapi2d HISTO (diffim) (xst) (yst) 64 64 \
    getpar glob histo_max (maxval)
    getpar glob histo_min (minval)
    if porn = 1
      getpar glob histo_xmax (xobj)
      getpar glob histo_ymax (yobj)
      print "Maximum signal of " (maxval) " found at " (xobj) (yobj)
    else
      getpar glob histo_xmin (xobj)
      getpar glob histo_ymin (yobj)
      print "Minimum signal of " (minval) " found at " (xobj) (yobj)
    end if
  end if
  if imwork = 1
    obeyw plt2d circle (xobj) (yobj) (aper1)
    obeyw plt2d circle (xobj) (yobj) (aper2)
    obeyw plt2d circle (xobj) (yobj) (aper3)
  end if
  print "Photometry apertures displayed on image"
  print "Analysing image " (diffim)
  scale = platscal
  sap1 = aper1/scale
  sap2 = aper2/scale
  sap3 = aper3/scale
  sscale = 1.0
  obeyw obsrap APERPHOT (diffim) T x T Y (xobj) (yobj) (sap1) ~
    (sap2) (sap3) (sscale) N \
  getpar glob aperphot_med2 (skypp)
  getpar glob aperphot_objsky (objval)
  getpar glob aperphot_mag (mag)
  skypp = real(skypp)
  objval = real(objval)
  mag = real(mag)
  if objval < 0
    print "WARNING : OBJECT-SKY gives negative value, absolute value used"
  end if
  print "  Zeropoint                                = " (zp)
{  print "  Object counts in aperture                = " (objpp)
  print "  Sky/pixel in sky annulus                 = " (skypp)
  print "  Star-Sky                                 = " (objval)
  print "  Inst. Magnitude                          = " (mag)
  if zp <> 0.0
    amag = zp+mag
    print "  Actual Magnitude                         = " (amag)
  end if
  print " "
  phbfile = file_exists("stdred_photometry.results")
  fclose_a
  if phbfile
    append afile "stdred_photometry.results"
  else
    create afile "stdred_photometry.results"
    fexist = file_exists("header.txt")
    if fexist
      fclose_c
      open cfile "header.txt"
      read cfile (dline)
      fclose_c
      delfile header.txt
      lhead = len(dline)
      header = substr(dline,1,lhead)
      write afile (header)
      header = " "
      write afile (header)
    end if
    ast = "FILENAME                      UT      FILTER   AP1  "
    bst = "AP2   AP3    PSCAL    ZP      AIRM  ACT.MAG    OBJ-SKY  "
    cst = "  I3.MAG  CAL.ZP CAL.ZPX"
    dline = ast&bst&cst
    write afile (dline)
  end if
  czp = zp:8:3
  if zp = 0.0 and actmag2 <> -99.99
    calzp = abs(mag)+actmag2
    if meanam <> -9.99
      calzpx = calzp + (meanam-1.0)*extval
    else
      calzpx = -99.99
    end if
  else
    calzp = -99.99
    calzpx = -99.99
  end if
  ccalzp = calzp:8:3
  ccalzpx = calzpx:8:3
  disp_ap1 = real(aper1)
  disp_ap2 = real(aper2)
  disp_ap3 = real(aper3)
  scale = real(scale)
  cd1 = disp_ap1:6:2
  cd2 = disp_ap2:6:2
  cd3 = disp_ap3:6:2
  carc = scale:8:3
  cobjval = objval:13:5
  cmag = mag:8:3
  cam = meanam:8:3
  cac = actmag2:9:3
  meanut = -99.99
  cut = meanut:10:4
  nc = len(diffim)
  if nc < 24
    na = 24-nc
    loop for k = 1 to na
      diffim = diffim & "."
    end loop
  else
    diffim = substr(diffim,1,24)
  end if
  nc = len(filtn)
  if nc < 5
    na = 5-nc
    loop for k = 1 to na
      filtn = filtn & "."
    end loop
  else
    filtn = substr(filtn,1,5)
  end if
  if medorsep = 1
    filtn = filtn&"M"
  else
    filtn = filtn&"S"
  end if
  dline = diffim&"  "&cut&"  "&~
    filtn&cd1&cd2&cd3&carc&czp&cam&cac&cobjval&cmag&ccalzp&ccalzpx
  write afile (dline)
  fclose_a

{ Cleanup
  if std_red = "Y"
    print " "
    print "Please wait ... deleting intermediate reduction files ..."
    delfile mask.list
    delfile stdred.off
    loop for dummy = (value2) to (value3)
      im = init & dummy & "d.sdf"
      delfile (im)
      im = init & dummy & "df.sdf"
      delfile (im)
      if wantair = 1
        im = init & dummy & "dfa.sdf"
        delfile (im)
      end if
      if wantmos = 1
        if wantair = 1
          im = init & dummy & "dfaz.sdf"
          delfile (im)
          im = init & dummy & "dfazm.sdf"
          delfile (im)
        else
          im = init & dummy & "dfz.sdf"
          delfile (im)
          im = init & dummy & "dfzm.sdf"
          delfile (im)
        end if
      end if
    end loop
  end if
  print " "
  print "Photometry results appended to file : stdred_photometry.results"
  print " "
  print "Any photometry problems are appended to file : photometry.problems"
  print " "
  print "END of procedure DRB_STRED"
  print " "
end proc
