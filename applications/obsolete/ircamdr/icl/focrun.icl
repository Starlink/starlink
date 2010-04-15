{ PROCEDURE FOCRUN : focus run analysis
proc focrun ost oen oda
  get plt2d name_prefix (name_prefix)
  if name_prefix = 'UNKNOWN'
    print "File prefix is undefined: define it using SETPRE"
    return
  end if
  get plt2d image_workstn (image_workstn)
  get plt2d cursor_workstn (cursor_workstn)
  get plt2d name_image (name_image)
  get plt2d worknam (worknam)
  if image_workstn = 1
    yn1 = undefined(ost)
    yn2 = undefined(oen)
    yn3 = undefined(oda)
    if yn1
      print "Current default file prefix = " (name_prefix)
      print "Enter PREFIX of input image(s) [RETURN = above default] :"
      askname (ipref) "Prefix \-1\ ? "
    else
      ipref = "-1"
    end if
    if ipref = "-1"
      pref = name_prefix
    else
      pref = ipref
    end if
    if yn1
      print "Enter START observation no. of focus run :"
      asknum (nost) "Range Start  \1\ ? "
    else
      nost = ost
    end if
    if yn2
      print "Enter END observation no. of focus run :"
      asknum (noen) "Range End    \1\ ? "
    else
      noen = oen
    end if
    if yn1
      print "Enter SUFFIX of input image(s) (NONE for no suffix) :"
      askname (suff) "Suffix   \NONE\ ? "
      if upcase(suff) = "NONE"
        suff = ""
      end if
    else
      suff = ""
    end if
    value2 = nost
    value3 = noen
    if yn3
      print "Enter DARK/SKY number :"
      asknum (noda) "Dark/Sky Number \1\ ? "
    else
      noda = oda
    end if
    stda = pref & noda & suff
    minf = 9999.9999
    realmax = -1.0e20
    iflag = 0
    fclose_c
    delfile focrun.dat
    create cfile focrun.dat
    loop for dummy = (value2) to (value3)
      st2 = pref & dummy & suff
      st2mda = "f"&dummy&"m"&noda
      obeyw rapi2d sub (st2) (stda) (st2mda)
      send plt2d set last_plot 2
      send plt2d set cursor_image (st2mda)
      send plt2d set name_image (st2mda)
      print "NSIGMA plot of " (st2mda) " on " (worknam)
      obeyw plt2d nsigma (st2mda)
      if iflag = 0
        iflag = 1
        send plt2d set cursor_cross 'YES'
        get plt2d platscal (platscal)
        fclose_b
        delfile starpos.dat
        create bfile "starpos.dat"
        loop for numstars = 1 to 1000
          print "Select position of STAR using cursor"
          print "  click off image to terminal star selection"
          obeyw plt2d cursor
          get plt2d x_cur_pixel (xobj)
          get plt2d y_cur_pixel (yobj)
          get plt2d cursor_value (val)
          if val <> -9999
            print "Star position (pixels) = " (xobj) (yobj) " Value = " (val)
            st1 = '""'&(xobj)&" "&(yobj)&'""'
            write bfile (st1)
            xbc = xobj - 10
            ybc = yobj - 10
            x2 = 20
            y2 = 20
          else
            print "Star selection terminated"
            break
          end if
        end loop
        fclose_b
      end if
      iname = "@"&st2mda
      psf in=(iname) cbfile=starpos.dat fwhm=(seeing) \
      send plt2d set gun_spec 'COLOUR'
      send plt2d set pen_colour "W"
      obeyw plt2d setcol 1
      afwhm = seeing*platscal
      print "PSF FWHM image " (st2mda) " =" (afwhm) "arcsec"
      print " "
      if minf > afwhm
        minf = afwhm
        namef = st2mda
      end if
      print "Statistics box bottom-left = " (xbc) (ybc)
      xb = integer(xbc)
      yb = integer(ybc)
      obeyw rapi2d histo (st2mda) (xb) (yb) (x2) (y2) \
      getpar glob histo_max (maxval)
      getpar glob histo_xmax (xmx)
      getpar glob histo_ymax (ymx)
      print "Peak signal = " (maxval) " at pixel " (xmx) (ymx)
      if maxval > realmax
        realmax = maxval
        namem = st2mda
      end if
      cafwhm = afwhm:10:2
      cmaxval = maxval:10:2
      dline = st2mda&"   "&cmaxval&cafwhm
      write cfile (dline)
    end loop
    print "Minimum PSF FWHM =" (minf) "arcsec in image " (namef)
    print "Peak signal      =" (realmax) " in image " (namem)
    dline = " "
    write cfile (dline)
    cmaxval = realmax:10:2
    dline = namem&"   "&cmaxval
    write cfile (dline)
    cafwhm = minf:10:2
    dline = namef&"   "&cafwhm
    write cfile (dline)
    fclose_c
    print " "
    print "Results"
    $ more focrun.dat
    print " "
  else
    print "Cannot display images on workstation " (worknam)
  end if
end proc
