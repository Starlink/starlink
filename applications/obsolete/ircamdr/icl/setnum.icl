{ PROCEDURE SETNUM : sets offset or ra/dec mode for contour maps and surround
proc setnum
  print "Do you want numbers to be OFFSETS or RA and DEC values ?"
  askchoice (value1) "Number Choice (OFFSETS,RADEC,O,R) \OFFSETS\ : "
  send plt2d set contour_annotat 'ANNOTATION'
  if value1 = 1 or value1 = 3
    send plt2d set number_type 'OFFSETS'
  else
    send plt2d set number_type 'RA_DEC'
  end if
  print "Do you wish to change the axis tick marks ZERO POINT and INTERVALS ?"
  asklog (value2) "Change Zero and Interval \Y\ : "
  if value2 = 1
    get plt2d im_xsize (xsz)
    get plt2d im_ysize (ysz)
    print "Input the X and Y axis TICK MARKS ZERO POINT (pixel number) "
    print "Centre of last image plotted = " (xsz) (ysz)
    asknum (value3) "X-Axis Tick Mark ZERO (RETURN=CENTRE) \-1\ : "
    asknum (value4) "Y-Axis Tick Mark ZERO (RETURN=CENTRE) \-1\ : "
    if value3 = -1
      value3 = integer(xsz/2.0)
    end if
    if value4 = -1
      value4 = integer(ysz/2.0)
    end if
    value3 = value3-1
    value4 = value4-1
    send plt2d set x_zero (value3)
    send plt2d set y_zero (value4)
    if value1 = 1 or value1 = 3
      print "Input the X and Y axis TICK MARK INTERVALS (in arcseconds) "
      asknum (value3) "X-Axis Tick Mark INTERVAL \30.0\ : "
      asknum (value4) "Y-Axis Tick Mark INTERVAL \30.0\ : "
      get plt2d platscal (platscal)
      if platscal = 1.0
        print "Enter pixel scale in arcsec/pixel : "
        asknum (platscal) "Pixel Scale \0.286\ ? "
        send plt2d set platscal (platscal)
        send plt2d set arcsec_pixel (platscal)
      end if
      if platscal > 0.0
        value3 = value3/platscal
        value4 = value4/platscal
      end if
    else
      print "Input the RA axis TICK MARK INTERVALS (in seconds of time) "
      asknum (value3) "RA-Axis Tick Mark INTERVAL \1.0\ : "
      get plt2d platscal (platscal)
      if platscal > 0
        value3 = value3*15/platscal
      end if
      print "Input the DEC axis TICK MARK INTERVALS (in arcseconds) "
      asknum (value4) "DEC-Axis Tick Mark INTERVAL \15.0\ : "
      if platscal > 0
        value4 = value4/platscal
      end if
    end if
    send plt2d set contour_tickint (value3)
    send plt2d set contour_ytickin (value4)
    send plt2d set pol_xtickint (value3)
    send plt2d set pol_ytickint (value4)
  end if
  if value1 = 2 or value1 = 4
    print "Give the RA and DEC coordinates of the ZERO points :"
    askname (value1) "RA Coordinate (e.g. 12 30 25.3) : "
    ra_coord = ra(value1)
    ra_coord = ra_coord*180.0/3.141592654/15.0
    send plt2d set ra_zero (ra_coord)
    askname (value2) "Dec Coordinate (e.g. 40 25 26.6) : "
    dec_coord = decl(value2)
    dec_coord = dec_coord*180.0/3.141592654
    send plt2d set dec_zero (dec_coord)
    cosdelta = cosd( dec_coord)
    print "Declination          = " (dec_coord) " degrees"
    print "Cosine( Declination) = " (cosdelta)
  end if
  print "Do you want the DEC numbers HORIZONTAL or VERTICAL ? "
  askchoice (value5) "Number Orientation (H,V) \V\ : "
  if value5 = 2
    send plt2d set number_orient 'UP'
   else
    send plt2d set number_orient 'NORMAL'
  end if
end proc

