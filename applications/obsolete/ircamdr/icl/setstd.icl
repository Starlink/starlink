proc setstd a b c d e f g h i
  yna = undefined(a)
  ynb = undefined(b)
  ync = undefined(c)
  ynd = undefined(d)
  yne = undefined(e)
  ynf = undefined(f)
  yng = undefined(g)
  ynh = undefined(h)
  yni = undefined(i)
  if ynb
    asknum (ap1) "Diameter of STAR aperture used (arcsec)    \8\ ? "
  else
    ap1 = b
    print "Diameter of STAR aperture used (arcsec) = " (ap1)
  end if
  send plt2d set disp_ap1 (ap1)
  if ync
    asknum (ap2) "Inner SKY aperture diameter (arcsec)      \15\ ? "
  else
    ap2 = c
    print "Inner SKY aperture diameter (arcsec)    = " (ap2)
  end if
  send plt2d set disp_ap2 (ap2)
  if ynd
    asknum (ap3) "Outer SKY aperture diameter (arcsec)      \20\ ? "
  else
    ap3 = d
    print "Outer SKY aperture diameter (arcsec)    = " (ap3)
  end if
  send plt2d set disp_ap3 (ap3)
  if yne
    asknum (sig) "Sigma level for image display              \3\ ? "
  else
    sig = e
    print "Sigma level for image display           = " (sig)
  end if
  send plt2d set disp_ns (sig)
  if ynf
    askchoice (yn) "Reduce RO images or PHO on a REDUCED image (R,P) \R\ ? "
  else
    yn = f
  end if
  if yn = 1
    print "Reduce RO images + auto-PHO selected"
  else
    send plt2d set std_red "N"
    send plt2d set std_phowhat "MOS"
    print "Just auto-PHO selected"
  end if
  if yn = 1
    if yna
      asknum (noseq) "Number of images in photometry sequence \3\ ? "
    else
      noseq = a
{      print "Number of images in photometry sequence = " (noseq)
    end if
    send plt2d set std_noseq (noseq)
    if yng
      print "Photometry on INDIVIDUAL+MOSAIC images or just MOSAIC image ?"
      askchoice (alon) "Photometry on A=IND+MOS or just B=MOS (A,B) \A\ ? "
    else
      yn = g
      alon = 2
    end if
    if yn = 1
      send plt2d set std_red "Y"
      if alon = 1
        send plt2d set std_phowhat "BOTH"
      else
        send plt2d set std_phowhat "MOS"
      end if
    else
      send plt2d set std_red "N"
      send plt2d set std_phowhat "MOS"
    end if
    if yni
      print "Use median filtered sky as flat-field or a separate sky image ?"
      askchoice (medval) "Median filtered FF or separate sky (M,S) \M\ ? "
      if medval = 1
        send plt2d set std_sky "M"
      else
        send plt2d set std_sky "S"
        print "Name of J flat-field image ? "
        askname (jff) "J Flat-field \NONE\ ? "
        jff2 = upcase(jff)
        if jff2 = "NONE"
          jff = "NONE"
        end if
        send plt2d set ffj (jff)
        print "Name of H flat-field image ? "
        askname (hff) "H Flat-field \NONE\ ? "
        hff2 = upcase(hff)
        if hff2 = "NONE"
          hff = "NONE"
        end if
        send plt2d set ffh (hff)
        print "Name of K flat-field image ? "
        askname (kff) "K Flat-field \NONE\ ? "
        kff2 = upcase(kff)
        if kff2 = "NONE"
          kff = "NONE"
        end if
        send plt2d set ffk (kff)
        print "Name of nbL flat-field image ? "
        askname (nblff) "nbL Flat-field \NONE\ ? "
        nblff2 = upcase(nblff)
        if nblff2 = "NONE"
          nblff = "NONE"
        end if
        send plt2d set ffnbl (nblff)
        print "Name of Lp flat-field image ? "
        askname (lpff) "Lp Flat-field \NONE\ ? "
        lpff2 = upcase(lpff)
        if lpff2 = "NONE"
          lpff = "NONE"
        end if
        send plt2d set fflp (lpff)
        print "Name of nbM flat-field image ? "
        askname (nbmff) "nbM Flat-field \NONE\ ? "
        nbmff2 = upcase(nbmff)
        if nbmff2 = "NONE"
          nbmff = "NONE"
        end if
        send plt2d set ffnbm (nbmff)
      end if
    else
      if i = 1
        send plt2d set std_sky "M"
      else
        send plt2d set std_sky "S"
        print "Name of J flat-field image ? "
        askname (jff) "J Flat-field \NONE\ ? "
        jff2 = upcase(jff)
        if jff2 = "NONE"
          jff = "NONE"
        end if
        send plt2d set ffj (jff)
        print "Name of H flat-field image ? "
        askname (hff) "H Flat-field \NONE\ ? "
        hff2 = upcase(hff)
        if hff2 = "NONE"
          hff = "NONE"
        end if
        send plt2d set ffh (hff)
        print "Name of K flat-field image ? "
        askname (kff) "K Flat-field \NONE\ ? "
        kff2 = upcase(kff)
        if kff2 = "NONE"
          kff = "NONE"
        end if
        send plt2d set ffk (kff)
        print "Name of nbL flat-field image ? "
        askname (nblff) "nbL Flat-field \NONE\ ? "
        nblff2 = upcase(nblff)
        if nblff2 = "NONE"
          nblff = "NONE"
        end if
        send plt2d set ffnbl (nblff)
        print "Name of Lp flat-field image ? "
        askname (lpff) "Lp Flat-field \NONE\ ? "
        lpff2 = upcase(lpff)
        if lpff2 = "NONE"
          lpff = "NONE"
        end if
        send plt2d set fflp (lpff)
        print "Name of nbM flat-field image ? "
        askname (nbmff) "nbM Flat-field \NONE\ ? "
        nbmff2 = upcase(nbmff)
        if nbmff2 = "NONE"
          nbmff = "NONE"
        end if
        send plt2d set ffnbm (nbmff)
      end if
    end if
  end if
  if ynh
    print "Auto-select star near image centre or select with Cursor ? "
    askchoice (chow) "Auto-select or Cursor (A,C) \A\ ? "
    if chow = 1
      send plt2d set std_select "A"
    else
      send plt2d set std_select "C"
    end if
  else
    if h = 1
      send plt2d set std_select "A"
    else
      send plt2d set std_select "C"
    end if
  end if
end proc
