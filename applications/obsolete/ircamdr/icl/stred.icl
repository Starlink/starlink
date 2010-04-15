{ PROCEDURE STRED : reduces a number of STARE/ND_STARE images
proc stred
  set precision 6
  get plt2d filetype (filetype)
  get plt2d rosuf (rosuf)
  get plt2d name_prefix (name_prefix)
  if filetype <> 1
    print "Sorry, you cannot run STRED on old format container files"
    return
  end if
  if name_prefix = "UNKNOWN"
    print "You must define IRCAM data file before running STRED"
    print "Use command SETPRE."
    return
  else
    get plt2d contname (contname)
    print "Current IRCAM file prefix = " (name_prefix)
    print "Current IRCAM file suffix = " (rosuf)
  end if
  get plt2d platscal (platscal)
  pref = name_prefix
  if upcase(rosuf) = "NONE"
    suff = ""
  else
    suff = rosuf
  end if
  print "Enter a PREFIX for the output image(s)"
  askname (init) "Prefix         \im_\ ? "
  print "Enter start observation number ? "
  asknum (value2) "Start           \1\ ? "
  print "Enter number of images to be reduced ? "
  asknum (numin) "Number of Images \1\ ? "
  value3 = value2+numin-1
  print "Enter the DARK image number : "
  asknum (obs) "Dark number    \1\ ? "
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
    print "    Observation specified as DARK is truly a DARK"
  else
    print "Observation specified as DARK may not be a DARK, quiting"
    print "  Object name in supposed DARK image = " (dobject)
    print "  Filter name in supposed DARK image = " (dfilter)
    return
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
  object = upcase(object)
  filter = upcase(filter)
  airmass = (amst+amen)/2.0
  if platscal = 1.0
    print "Enter pixel scale in arcsec/pixel : "
    asknum (platscal) "Pixel Scale \0.286\ ? "
    send plt2d set platscal (platscal)
    send plt2d set arcsec_pixel (platscal)
  end if
  print "Do you want to MEDIAN FILTER the objects for a flat or "
  print "use a SEPARATE pre-prepared (normalized) SKY image ? "
  askchoice (medorsep) "Median or Separate Sky (M,S) \M\ ? "
  if medorsep = 2
    print "Give name of sky flat image : "
    askname (skyflat) "Sky Flat Name \skyflat\ ? "
    skyflat2 = skyflat & ".sdf"
    fexist = file_exists(skyflat2)
    if fexist
      print "File " (skyflat) " found"
    else
      print "File " (skyflat) " not found, try again"
      print "Give name of SKY FLAT image : "
      askname (skyflat) "Sky Flat Name \skyflat\ ? "
      skyflat2 = skyflat & ".sdf"
      fexist = file_exists(skyflat2)
      if fexist
        print "File " (skyflat) " found"
      else
        print "File " (skyflat) " not found, EXITING"
        return
      end if
    end if
  end if
  print "Do you want the images airmass corrected ? "
  asklog (wantair) "Airmass Correction (Yes or No) \N\ : "
  if numin > 1
    print "Do you want the images auto-mosaiced ? "
    asklog (wantmos) "Mosaicing (Yes or No) \Y\ : "
    if wantmos = 1
      print "Getting RA,DEC offsets from input images..."
      fclose_c
      delfile stred.off
      create cfile "stred.off"
      iflag = 0
      print "RA,DEC (modified) offsets in image sequence are : "
      loop for dummy = (value2) to (value3)
        tim = pref & dummy & suff
        obeyw plt2d ropars (tim)
        get plt2d ra_off        (raoff)
        get plt2d dec_off       (decoff)
        raoff = real(integer(raoff*100.0)/100.0)
        decoff = real(integer(decoff*100.0)/100.0)
        if iflag = 0
          iflag = 1
          ra0 = raoff
          dec0 = decoff
{          raoff = 0.0
{          decoff = 0.0
        else
{          raoff = raoff-ra0
{          decoff = decoff-dec0
        end if
        cra = raoff:10:3
        cdec = decoff:10:3
        dline = cra & cdec
        write cfile (dline)
        print "  Image " (tim) " : " (dline)
      end loop
      fclose_c
      print "Use these offsets or use user-defined telescope " ~
        "offset (ASCII) file ? "
      askchoice (offwhat) "File Offsets or User-defined (F,U) \F\ ? "
      if offwhat = 1
        toff = "stred.off"
        print "  Offset file created from image sequence = " (toff)
      else
        askname (toff) "Offset file \offsets.list\ ? "
        fexist = file_exists(toff)
        if fexist
          print "File " (toff) " found"
        else
          print "File " (toff) " not found, try again"
          print "Give name of telescope offset (ASCII) file for mosaic : "
          askname (toff) "Offset file \offsets.list\ ? "
          fexist = file_exists(toff)
          if fexist
            print "File " (toff) " found"
          else
           print "File " (toff) " not found, EXITING"
            return
          end if
        end if
      end if
      print "Give name for FINAL mosaiced image : "
      askname (final) "Final Image \x\ ? "
    end if
  else
    wantmos = -999
    print "Give name for FINAL image : "
    askname (final) "Final Image \x\ ? "
  end if
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
    print "MEDIAN FILTERING FLATFIELD CREATION SECTION"
    print "==========================================="
    flatn = "flat"
    obeyw obsrap MED3D "darklot.list" (flatn) \
    delfile med3d_work.sdf
    delfile med3d_lwork.sdf
  else
    flatn = skyflat
  end if
  delfile darklot.list
  print " "
  print "FLATFIELDING SECTION"
  print "===================="
  loop for dummy = (value2) to (value3)
    im = init & dummy & "d"
    out = init & dummy & "df"
    print "Flatfielding image " (im) " ..."
    obeyw rapi2d DIV (im) (flatn) (out)
    print "Flatfielded image output to : " (out)
  end loop
  if medorsep = 1
    delfile flat.sdf
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
  if numin > 1
    if wantmos = 1
      print " "
      print "AUTOMOS SKY CORRECTION SECTION"
      print "=============================="
      fclose_b
      delfile images.list
      delfile images.list2
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
      delfile junk.dat
      obeyw obsrap AUTOMOS "images.list" (toff) "junk.dat" (platscal) \
      delfile junk.dat
    end if
  end if
  print " "
  print "BAD PIXEL MASKING SECTION"
  print "========================="
  reload_obsrap
  tim = pref & value2 & suff
  obeyw rapi2d shsize (tim)
  getpar glob shsize_xdim (xdim)
  getpar glob shsize_ydim (ydim)
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
    delfile mask.list
  else
    out2 = out & ".sdf"
    final2 = final & ".sdf"
    ! mv (out2) (final2)
    delfile mask.list
  end if

{ Cleanup
  print " "
  print "Please wait ... deleting intermediate reduction files ..."
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
      else
        im = init & dummy & "dfz.sdf"
        delfile (im)
      end if
    end if
  end loop
  if wantmos = 1
    obeyw rapi2d shsize (final)
  end if
  print " "
  print "END of procedure STRED"
  print " "
  print "==========================================================="
  print "R E M E M B E R - images reduced by STRED are normalized to"
  print "                  1 SECOND EXPOSURE TIME"
  print "==========================================================="
  print " "
end proc
