{ PROCEDURE CHRED : reduces a number of CHOP images
proc chred v1 v2 v3 v4 v5 v6 v7 v8
  yn1 = undefined(v1)
  yn2 = undefined(v2)
  yn3 = undefined(v3)
  yn4 = undefined(v4)
  yn5 = undefined(v5)
  yn6 = undefined(v6)
  yn7 = undefined(v7)
  yn8 = undefined(v8)
  get plt2d filetype (filetype)
  if filetype <> 1
    print "ERROR, can only run CHRED on new format CHOP data files"
    return
  end if
  get plt2d name_prefix (name_prefix)
  if name_prefix = "UNKNOWN"
    print "You must define IRCAM data file before running CHRED"
    print "Use command SETPRE."
    return
  end if
  get plt2d rosuf (rosuf)
  rosuf2 = upcase(rosuf)
  if rosuf2 = "NONE"
    rosuf = ""
  end if
  get plt2d contname (contname)
  print "Current IRCAM file prefix = " (name_prefix)
  print "Current IRCAM file suffix = " (rosuf)
  if name_prefix = "NONE"
    print "Error, input file prefix (e.g. RO940422_) not defined."
    print "use SETFILE to define then run again."
    return
  else
    utd = substr(name_prefix,3,6)
  end if
  if rosuf = "NONE"
    rosuf = ""
  end if
  pref = name_prefix
  suffa = rosuf & "a"
  suffb = rosuf & "b"
  if yn1
    print "Enter a PREFIX for the output DIFFERENCE image(s)"
    askname (init) "Prefix         \ch_\ ? "
  else
    init = v1
    print "Prefix = " (init)
  end if
  if yn2
    print "Enter start OBJECT observation number ? "
    asknum (value2) "Start           \1\ ? "
  else
    value2 = v2
    print "Start OBJECT number = " (value2)
  end if
  if yn3
    print "Enter number of images to be reduced ? "
    asknum (numin) "Number of Images \1\ ? "
  else
    numin = v3
    print "Number of image = " (numin)
  end if
  value3 = value2+numin-1
  testim = pref & value2 & suffa
  obeyw rapi2d shsize (testim) \
  getpar glob shsize_xdim (xdim)
  getpar glob shsize_ydim (ydim)
  if xdim = 256
    glitchf = "$LIRCAMDIR/bpm_fpa42_256x256"
  else if xdim = 128
    glitchf = "$LIRCAMDIR/bpm_fpa42_128x128"
  else if xdim = 64
    glitchf = "$LIRCAMDIR/bpm_fpa42_64x64"
  else
    print "ERROR, wierd array size " (xdim) " by " (ydim) " - stopping"
    return
  end if
  gf = glitchf&".sdf"
  fexist = file_exists(gf)
  tim = pref & value2 & suffa
  obeyw plt2d ropars (tim)
  get plt2d object_name   (object)
  get plt2d filter        (filter)
  get plt2d exposure_time (exp)
  get plt2d number_exp    (nexp)
  get plt2d airmass_start (amst)
  get plt2d airmass_end   (amen)
  get plt2d ra_off        (raoff)
  get plt2d dec_off       (decoff)
  object = upcase(object)
  valstr (object) (obj2)
  filter = upcase(filter)
  airmass = (amst+amen)/2.0
  print "On-chip exposure time (sec) = " (exp)
  if yn4
    print "Was the CHOP observation NODDED as well ? "
    asklog (nod) "Chop and Nod (Yes or No) \Y\ ? "
  else
    nod = v4
    print "Chop observation nodded = " (nod)
  end if
  get plt2d platscal (platscal)
  if platscal = 1.0
    print "Enter pixel scale in arcsec/pixel : "
    asknum (platscal) "Pixel Scale \0.286\ ? "
    send plt2d set platscal (platscal)
    send plt2d set arcsec_pixel (platscal)
  end if
  if yn5
    print "Do you want the images airmass corrected ? "
    asklog (wantair) "Airmass Correction (Yes or No) \N\ : "
  else
    wantair = v5
    print "Airmass correction = " (wantair)
  end if
  if wantair = 1
    filt = filter
    if yn6
      print "Input average airmass of observations :"
      asknum (airm) "Airmass \1.0\ ? "
    else
      airm = v6
      print "Average airmass = " (airm)
    end if
  end if
  if nod = 1
    fin2 = obj2&"_"&value2&"_"&"2"
    if yn7
      print "Give names for FINAL MAIN beam NOD image : "
      print "DEFAULTS = " (fin1)
      askname (final1) "Final NOD Image 1 \-1\ ? "
    else
      final1 = v7
      print "Final image 1 = " (final1)
    end if
    if yn8
      print "Give names for FINAL MAIN beam NOD image : "
      askname (final2) "Final NOD Image 2 \-1\ ? "
    else
      final2 = v8
      print "Final image 2 = " (final2)
    end if
    if final1 = "-1"
      final1 = fin1
    end if
    if final2 = "-1"
      final2 = fin2
    end if
  else
    fin = obj2&"_"&value2
    if yn7
      print "Give name for FINAL image (DEFAULT="(fin)")"
      askname (final) "Final Image \-1\ ? "
    else
      final = v7
      print "Final image = " (final)
    end if
    if final = "-1"
      final = fin
    end if
  end if
  if wantair = 1
    filt = upcase(filt)
    if filt = 1
      filter = "J"
    end if
    if filt = 2
      filter = "H"
    end if
    if filt = 3
      filter = "K"
    end if
    if filt = 4
      filter = "NBL"
    end if
    if filt = 5
      filter = "LP"
    end if
    if filt = 6
      filter = "NBM"
    end if
  end if
  print " "
  print "PAIR SUBTRACTION SECTION"
  print "========================"
  loop for dummy = (value2) to (value3)
    im1 = pref & dummy & suffa
    im2 = pref & dummy & suffb
    out = init & dummy & "s"
    print "Subtracting images " (im1) " and " (im2) " and scaling ..."
    obeyw rapi2d SUB (im1) (im2) chred_junk
    obeyw rapi2d CDIV chred_junk (exp) (out)
    delfile chred_junk.sdf
    print "  Subtracted/scaled image output to : " (out)
  end loop
  if wantair = 1
    print " "
    print "AIRMASS CORRECTION SECTION"
    print "=========================="
    loop for dummy = (value2) to (value3)
      im = init & dummy & "s"
      out = init & dummy & "sa"
      print "Airmass correcting image " (im) " using : "
      print "  filter  = " (filter)
      print "  airmass = " (airm) "..."
      obeyw obsrap AMCORR (im) (out) (filter) (airm) \
      print "Airmass corrected image output to : " (out)
    end loop
  end if
  print " "
  print "COADDITION SECTION"
  print "=================="
  if nod = 1
    nstep = 2
  else
    nstep = 1
  end if
  nim = 0
  iflag = 0
  loop for dummy = (value2) to (value3) step (nstep)
    if iflag = 0
      iflag = 1
      if nod = 1
        dummy2 = dummy + 1
        if wantair = 1
          im1 = init & dummy & "sa"
          im2 = init & dummy2 & "sa"
        else
          im1 = init & dummy & "s"
          im2 = init & dummy2 & "s"
        end if
        obeyw rapi2d CMULT (im1) 1 junk1
        obeyw rapi2d CMULT (im2) 1 junk2
        if wantair = 1
          im1 = init & dummy & "sa"
          im2 = init & dummy2 & "sa"
        else
          im1 = init & dummy & "s"
          im2 = init & dummy2 & "s"
        end if
        nim = nim + 1
        print "Coadding image " (im1)
        print "Coadding image " (im2)
      else
        if wantair = 1
          im = init & dummy & "sa"
        else
          im = init & dummy & "s"
        end if
        obeyw rapi2d CMULT (im) 1 junk
        if wantair = 1
          im = init & dummy & "sa"
        else
          im = init & dummy & "s"
        end if
        nim = nim + 1
        print "Coadding image " (im)
      end if
    else
      if nod = 1
        dummy2 = dummy + 1
        if wantair = 1
          im1 = init & dummy & "sa"
          im2 = init & dummy2 & "sa"
        else
          im1 = init & dummy & "s"
          im2 = init & dummy2 & "s"
        end if
        nim = nim + 1
        print "Coadding image " (im1)
        print "Coadding image " (im2)
        obeyw rapi2d ADD junk1 (im1) junk1b \
        obeyw rapi2d ADD junk2 (im2) junk2b \
        ! mv junk1b.sdf junk1.sdf
        ! mv junk2b.sdf junk2.sdf
      else
        if wantair = 1
          im = init & dummy & "sa"
        else
          im = init & dummy & "s"
        end if
        nim = nim + 1
        print "Coadding image " (im)
        obeyw rapi2d ADD junk (im) junkb \
        ! mv junkb.sdf junk.sdf
      end if
    end if
  end loop
  if nod = 1
    obeyw rapi2d CDIV junk1 (nim) (final1)
    delfile junk1.sdf
    send plt2d set name_image (final1)
    obeyw rapi2d CDIV junk2 (nim) (final2)
    delfile junk2.sdf
  else
    obeyw rapi2d CDIV junk (nim) (final)
    send plt2d set name_image (final)
    delfile junk.sdf
  end if
  print " "
  count = 0
  print "BAD PIXEL MASKING SECTION"
  print "========================="
  count = count + 1
  if count > 20
    reload_obsrap
    count = 0
  end if
  if nod = 1
    print "Bad pixel masking image " (final1) " using " (glitchf) " ..."
    obeyw obsrap APPLYMASK (final1) (glitchf) junk1 -1.0E-20
    print "Bad pixel masking image " (final2) " using " (glitchf) " ..."
    obeyw obsrap APPLYMASK (final2) (glitchf) junk2 -1.0E-20
    ! mv junk1.sdf (final1).sdf
    ! mv junk2.sdf (final2).sdf
    print " "
    print "Final reduced NOD image output to : " (final1) (final2)
    print " "
  else
    print "Bad pixel masking image " (final) " using " (glitchf) " ..."
    obeyw obsrap APPLYMASK (final) (glitchf) junk -1.0E-20
    ! mv junk.sdf (final).sdf
    print " "
    print "Final reduced NOD image output to : " (final)
    print " "
  end if

{ Cleanup
  print "Please wait ... deleting intermediate reduction files ..."
  loop for dummy = (value2) to (value3)
    if wantair = 1
      im = init & dummy & "s.sdf"
      delfile (im)
    end if
  end loop
  if nod = 1
    obeyw rapi2d shsize (final1)
  else
    obeyw rapi2d shsize (final)
  end if
  print "END of procedure CHRED"
  print " "
  print "R E M E M B E R - images reduced by CHRED are normalized to"
  print "                  1 SECOND EXPOSURE TIME!!"
  print " "
end proc
