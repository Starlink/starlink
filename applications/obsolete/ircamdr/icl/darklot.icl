{ PROCEDURE DARKLOT : dark subtracts a number of images
proc darklot
  get plt2d filetype (filetype)
  if filetype = 1
    print "Procedure DARKLOT does not run on new format data."
    print "Use procedure RODARKLOT instead."
    return
  end if
  get plt2d name_prefix (name_prefix)
  compare name_prefix "UNKNOWN" (brave_man)
  if brave_man = 1
    print "You must define IRCAM data file before running DARKLOT"
    print "Use command SETFILE."
    return
  else
    get plt2d contname (contname)
    print "Current IRCAM container file selected = " (name_prefix)
  end if
  print "Do you want to LINEARIZE the images before dark subtraction"
  asklog (linit) "Linearize (Yes or No) \N\ ? "
  if linit = 1
    print "Enter name of linearization coefficient file to be used :"
    askchar (coeffile) " \$LIRCAMDIR/lincoeff_fpa118_postjul88_ndr.list\ ? "
  end if
  pref = name_prefix
  suff = ").phasea"
  print "Enter a PREFIX for the output image(s)"
  askname (init) "Prefix       \im_\ ? "
  print "Enter the DARK image number (0 for full name)"
  asknum (obs) "Dark number      \0\ ? "
  if obs = 0
    if linit = 1
      print "ERROR, you cannot linearize using processed DARK image!"
      return
    end if
    print "Enter the DARK image name"
    askname (dark) "Dark name   \DARK\ ? "
    print "What is the ON-CHIP EXPOSURE TIME of the dark (Seconds) ? "
    asknum (exp) "On-chip exposure time \10\ ? "
    exp = exp*1000.0
    print "How many COADDS in this dark ? "
    asknum (nd) "Dark Coadds            \6\ ? "
  else
    obeyw plt2d contpars (obs)
    get plt2d number_coadds (nd)
    get plt2d exposure_time (exp)
    get_imagename (obs) 1 (dark)
  end if
  print "Dark image         = " (dark)
  print "Exposure time used = " (exp) "mS"
  print "Number of coadds   = " (nd)
  if linit = 1
    print "Linearizing DARK observation" (obs)
    obeyw obsrap LINIMAG_NDR (contname) (coeffile) ~
      lindark_junk (obs) \
  end if
  print "Enter start observation number ? "
  asknum (value2) "Start          \1\ ? "
  print "Enter end   observation number ? "
  asknum (value3) "End            \1\ ? "
  print "Do you want to create an ASCII list of dark subtracted images ?"
  asklog (brave_man) "Create ASCII List (Yes or No) \NO\ ? "
  if brave_man = 1
    fclose_b
    delfile darklot.list
    create bfile "darklot.list"
    print "File darklot.list opened..."
  end if
  loop for dummy = (value2) to (value3)
    tochar (dummy) (st1)
    concat (pref) (st1) (st2)
    concat (st2) (suff) (im)
    concat (init) (st1) (st3)
    concat (st3) "d" (out)
    if brave_man = 1
      write bfile (out)
    end if
    if linit = 1
      print "Linearizing image " (dummy) " using coefficient file :"
      print "  " (coeffile)
      obeyw obsrap LINIMAG_NDR (contname) (coeffile) ~
        linobs_junk (dummy) \
    end if
    print "Dark subtracting " (im) " ..."
    obeyw plt2d contpars (dummy)
    get plt2d number_coadds (nf)
    get plt2d exposure_time (expf)
    print (im) " was " (nf) " coadds each of " (expf) "mS"
    if expf <> exp
      print "WARNING, exposure time of OBJECT image is not same as DARK!!"
    end if
    fact = nf/nd
    if fact <> 1
      print "Source coadds = " (nf)
      print "Dark   coadds = " (nd)
      print "Scaling dark to source coadds, scaling factor = x" (fact)
      if linit = 1
        obeyw rapi2d CMULT lindark_junk (fact) darklot_junk
        obeyw rapi2d SUB linobs_junk darklot_junk darklot_junk2
      else
        obeyw rapi2d CMULT (dark) (fact) darklot_junk
        obeyw rapi2d SUB (im) darklot_junk darklot_junk2
      end if
    else
      print "Dark/source have same number of coadds, no scaling required"
      if linit = 1
        obeyw rapi2d SUB linobs_junk lindark_junk darklot_junk2
      else
        obeyw rapi2d SUB (im) (dark) darklot_junk2
      end if
    end if
    fact = nf*(expf/1000.0)
    invfact = 1.0/fact
    if fact <> 0
      print "Scaling SOURCE-DARK to DN/S, scaling factor   = x" (invfact)
    else
      print "Error, scaling factor to DN/S = 0!!!!"
      return
    end if
    obeyw rapi2d CDIV darklot_junk2 (fact) (out)
    print "Dark subtracted image = " (out)
    print "+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++"
  end loop
  delfile darklot_junk.sdf
  delfile darklot_junk2.sdf
  if linit = 1
    delfile lindark_junk.sdf
    delfile linobs_junk.sdf
  end if
  if brave_man = 1
    fclose_b
  end if
end proc

