{ PROCEDURE APPLYMASKLOT : applies bad pixel mask to lots of images
proc applymasklot
  get plt2d filetype (filetype)
  get plt2d rosuf (rosuf)
  print "Enter a PREFIX for the image(s) : "
  askname (init) "Prefix         \im_\ ? "
  print "Enter start observation number ? "
  asknum (value2) "Start           \1\ ? "
  print "Enter number of observation to be processed ? "
  asknum (numin) "Number Images    \5\ ? "
  value3 = integer(value2+numin-1)
  print "Give a SUFFIX for the image(s) (NONE = no suffix) : "
  askname (suff) "Suffix        \NONE\ ? "
  suff2 = upcase(suff)
  if suff2 = "NONE"
    suff = ""
  end if
  print "Input name of BAD PIXEL MASK to be used :"
  askname (glitchf) "Bad Pixel Mask \$LIRCAMDIR/bpm_fpa42_256x256\ ? "
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
  count = 0
  reload_obsrap
  loop for dummy = (value2) to (value3)
    count = count + 1
    if count > 20
      count = 1
      reload_obsrap
    end if
    im = init & dummy & suff
    out = im & "m"
    print "Bad pixel masking image " (im) " using " (glitchf) " ..."
    obeyw obsrap APPLYMASK (im) (glitchf) (out) -1.0E-20
    print "Bad pixel masked image output to : " (out)
  end loop
end proc
