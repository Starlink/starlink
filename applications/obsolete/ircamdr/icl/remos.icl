{ PROCEDURE REMOS : re-mosaics a set of reduce images
proc remos
  get plt2d filetype (filetype)
  get plt2d rosuf (rosuf)
  rosuf2 = upcase(rosuf)
  if rosuf2 = "NONE"
    suff = ""
  else
    suff = rosuf
  end if
  get plt2d platscal (platscal)
  print "Enter a PREFIX for the input image(s) :"
  askname (pref) "Prefix        \im_\  ? "
  print "Enter start observation number : "
  asknum (value2) "Start          \1\ ? "
  print "Enter number of images to re-mosaic : "
  asknum (numin)  "Number         \5\ ? "
  value3 = value2+numin-1
  value3 = integer(value3)
  print "Enter a SUFFIX for the input image(s) (NONE = no suffix) :"
  askname (suff) "Suffix       \NONE\ ? "
  suff2 = upcase(suff)
  if suff2 = "NONE"
    suff = ""
    print "No suffix specified"
  end if
  delfile imagelist
  delfile imagelist2
  create bfile "imagelist"
  loop for dummy = (value2) to (value3)
    dline = pref&dummy&suff
    write bfile (dline)
  end loop
  fclose_b
  if platscal = 1.0
    print "Enter pixel scale in arcsec/pixel : "
    asknum (platscal) "Pixel Scale \0.286\ ? "
    send plt2d set platscal (platscal)
  end if
  if numin > 1
    print "Pixel scale = " (platscal)
    print "Give name of telescope offset (ASCII) file for mosaic : "
    askname (toff) "Offset file \jitreg.dat\ ? "
    fexist = file_exists(toff)
    if fexist
      print "File " (toff) " found"
    else
      print "File " (toff) " not found"
      return
    end if
  end if
  print "Give name for FINAL mosaiced image : "
  askname (final) "Final Image \x\ ? "
  if numin > 1
    print " "
    print "AUTOMOS SKY CORRECTION SECTION"
    print "=============================="
    delfile junk.dat
    obeyw obsrap AUTOMOS "imagelist" (toff) "junk.dat" (platscal) \
    delfile junk.dat
    print " "
    print "BAD PIXEL MASKING SECTION"
    print "========================="
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
      askname (glitchf) "Bad Pixel Mask \bpm_user\ ? "
    end if
    gf = glitchf & ".sdf"
    fexist = file_exists(gf)
    if fexist
      print "File " (gf) " found"
    else
      print "File " (gf) " not found, try again!"
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
    delfile imagelist2
    create afile "imagelist2"
    count = 0
    loop for dummy = (value2) to (value3)
      im = pref & dummy & suff & "z"
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
    print " "
    print "QUILT MOSAICING SECTION"
    print "======================="
    delfile imagequilt
    obeyw obsrap CREQUILT (toff) "imagelist2" ~
      (platscal) "quilt" "imagequilt"
    obeyw rapi2d QUILT F "imagequilt" (final) YES YES V -1.0E-20 \
    delfile imagelist2
    delfile imagequilt
    print "O.K. final QUILTED image = " (final)
  else
    out2 = out & ".sdf"
    final2 = final & ".sdf"
    ! mv (out2) (final2)
  end if
{ Cleanup
  print "Please wait ... deleting intermediate reduction images ..."
  loop for dummy = (value2) to (value3)
    if numin > 1
      im = pref&dummy&suff&"z.sdf"
      delfile (im)
      im = pref&dummy&suff&"zm.sdf"
      delfile (im)
    end if
  end loop
  obeyw rapi2d shsize (final)
  print "END of procedure REMOS"
end proc
