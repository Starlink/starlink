{ PROCEDURE PLOTLOT : plots and does stuff on a number of images
proc plotlot wantret
  yn = undefined(wantret)
  print "Procedure to plot and do other stuff on a number of images"
  print "Do you want to enter IMAGE NAMES or PREFIX/RANGE OF NUMBER ?"
  askchoice (pltyp) "Names or Prefix,Numbers,Suffix (N,P) \P\ ? "
  if pltyp = 2
    print "Enter image names PREFIX ? "
    askname (ffpre) "Prefix    \im_\ ? "
    print "Enter start number ? "
    asknum  (plnst) "Start       \1\ ? "
    print "Enter end number ? "
    asknum  (plnen) "End        \20\ ? "
    print "Enter image names SUFFIX (NONE = no suffix) ? "
    askname (ffpost) "Suffix   \NONE\ ? "
    compare (ffpost) "NONE" (nonny)
  else
    plnst = 1
    plnen = 1000
  end if
  get plt2d magnification (plmag)
  print "Enter magnification for image plot, current value = " (plmag)
  asknum (plmag) "Magnification \0\ ? "
  send plt2d set magnification (plmag)
  print "Use PLOT or NSIGMA to display images ? "
  askchoice (plwt) "Plot How (P,N) \N\ ? "
  if plwt = 1
    get plt2d maximum (plmx)
    get plt2d minimum (plmn)
    print "Enter plot MAXIMUM,MINIMUM, current values = " (plmx) (plmn)
    asknum (plmx) "Maximum \100\ ? "
    asknum (plmn) "Minimum   \0\ ? "
    send plt2d set maximum (plmx)
    send plt2d set minimum (plmn)
  end if
  if plwt = 2
    get plt2d sigma_level (plmn)
    print "Enter NSIGMA sigma-level for plot, current value = " (plmn)
    asknum (plmn) "Sigma Level \2\ ? "
    send plt2d set sigma_level (plmn)
  end if
  print "Select action to be taken after image display :"
  print "Actions currently available are : "
  print "                                   CURSORING             = 1"
  print "                                   STATISTICS at CURSOR  = 2"
  print "                                   APERADD at CURSOR     = 3"
  print "                                   CENTROID at CURSOR    = 4"
  print "                                   NOTHING               = N"
  askchoice (plonk) "Enter Choice (1,2,3,4,N) \N\ ? "
  if plonk = 1
    print "O.K. you have selected CURSORING on every image to be displayed"
    print "How many cursor positions/image do you want (1-10) ?"
    asknum (plcur) "Cursor Number \1\ ? "
    if plcur < 1 or plcur > 10
      print "Error, illegal number of cursor positions chosen " (plcur)
      return
    end if
  end if
  if plonk = 2
    print "O.K. you have selected STATISTICS at CURSOR position on every image"
    get pt2d platscal (platscal)
    print "Current arcsec/pixel = " (platscal)
    print "Enter the X and Y STATISTICS box size (in arcsec) :"
    asknum (plxsz) "Stats X-Size \5\ ? "
    asknum (plysz) "Stats Y-Size \5\ ? "
  end if
  if plonk = 3
    print "O.K. you have selected APERADD at CURSOR position"
    get plt2d platscal (platscal)
    print "Current arcsec/pixel = " (platscal)
    print "Enter size of aperture (in arcseconds) to be used :"
    asknum (plap) "Aperture Size \10\ ? "
  end if
  obeyw plt2d clear
  loop for dummy = (plnst) to (plnen)
    if pltyp = 1
      print "Enter SOURCE image to be plotted/stuff done to (RETURN to stop)"
      askname (im) "Source image \-1\ ? "
      compare (im) "-1" (brave_man)
      if brave_man = 1
        print "O.K. finished using PLOTLOT ..."
        return
      end if
    else
      tochar (dummy) (st1)
      concat (ffpre) (st1) (st2)
      if nonny = 0
        concat (st2) (ffpost) (im)
      else
        im = st2
      end if
    end if
    name_image = im
    send plt2d set name_image (im)
    send plt2d set cursor_image (im)
    if plwt = 1
      obeyw plt2d plot (im)
      print "Image " (im) " displayed using PLOT ..."
    end if
    if plwt = 2
      obeyw plt2d nsigma (im)
      print "Image " (im) " displayed using NSIGMA ..."
    end if
    if plonk = 1
      print "Displaying " (plcur) " cursor(s) ..."
      MC (plcur)
    end if
    if plonk = 2
      print "Select position for STATISTICS box center using CURSOR "
      MC 1
      get plt2d x_cur_pixel (plxst)
      get plt2d y_cur_pixel (plyst)
      print "Stats box centre at " (plxst) "," (plyst) " and is " ~
        (plxsz) "x" (plysz)
      obeyw plt2d box (plxst) (plyst) (plxsz) (plysz) 'CENTRE'
      obeyw rapi2d STATS (im) (plxst) (plyst) (plxsz) (plysz) \
    end if
    if plonk = 3
      print "Select position for APERADD aperture center using CURSOR "
      MC 1
      get plt2d x_cur_pixel (plxst)
      get plt2d y_cur_pixel (plyst)
      obeyw plt2d circle (plxst) (plyst) (plap)
      get plt2d platscal (platscal)
      obeyw rapi2d APERADD (im) (plxst) (plyst) (plap) (platscal) \
    end if
    if plonk = 4
      print "Select position for CENTROID using CURSOR "
      MC 1
      get plt2d x_cur_pixel (plxst)
      get plt2d y_cur_pixel (plyst)
      obeyw rapi2d CENTROID (im) (plxst) (plyst) \
    end if
    print "+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++"
    if yn
      print "Hit RETURN to continue and plot next image"
      asknum (value1) "Hit RETURN or CTRL C \-1\ : "
    end if
  end loop
end proc

