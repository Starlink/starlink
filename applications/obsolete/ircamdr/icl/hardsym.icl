{ PROCEDURE HARDSYM : plots symbols on an image in hardcopy
proc hardsym value1
  yn = undefined(value1)
  if yn
    value1 = 0
  end if
  if value1 = 1
    print "Number of CIRCLES plotted ? "
    asknum (symnum) "Number of Circles \1\ : "
    loop for value6 = 1 to (symnum)
      print "Circle " value6 " X,Y pixel for centre ?"
      asknum (symx) "X-Pixel \31\ : "
      asknum (symy) "Y-Pixel \29\ : "
      print "Circle Diameter in Arcseconds ?"
      asknum (symsiz) "Diameter \10.0\ : "
      obeyw plt2d circle (symx) (symy) (symsiz)
    end loop
  else if value1 = 2
    print "Number of CROSSES plotted ? "
    asknum (symnum) "Number of Crosses \1\ : "
    loop for value6 = 1 to (symnum)
      print "Cross " value6 " X,Y pixel for centre ?"
      asknum (symx) "X-Pixel \31\ : "
      asknum (symy) "Y-Pixel \29\ : "
      print "Cross Size in Arcseconds ?"
      asknum (symsiz) "Size \10.0\ : "
      obeyw plt2d cross (symx) (symy) (symsiz)
    end loop
  else if value1 = 3
    print "Number of BOXES plotted ? "
    asknum (symnum) "Number of Boxes \1\ : "
    loop for value6 = 1 to (symnum)
      print "Box " value6 " X,Y pixel for centre ?"
      asknum (symx) "X-Pixel \31\ : "
      asknum (symy) "Y-Pixel \29\ : "
      print "Box X Size in Arcseconds ?"
      asknum (symsiz) "Size \10.0\ : "
      print "Box Y Size in Arcseconds ?"
      asknum (symsizy) "Size \10.0\ : "
      obeyw plt2d box (symx) (symy) (symsiz) (symsizy) 'CENTRE'
    end loop
  else if value1 = 4
    get plt2d im_xst (imxs)
    get plt2d im_yst (imys)
    get plt2d magnification (magn)
    if magn = 0
      get plt2d image_calmag (magn)
    end if
    get plt2d platscal (platscal)
    print "Number of LABELS plotted ? "
    asknum (symnum) "Number of Labels \1\ : "
    loop for value6 = 1 to (symnum)
      print "Label number " (value6)
      print "Enter Label Text ? "
      askchar (st1) "Text \IRCAM\ : "
      print "Enter Label X,Y centre pixel ?"
      asknum (symx) "X-Pixel \31\ : "
      asknum (symy) "Y-Pixel \29\ : "
      print "Enter Label Size in arcseconds ?"
      asknum (symsiz) "Size \10\ : "
      symx = symx*magn+imxs
      symy = symy*magn+imys
      symsiz = (symsiz/platscal)*magn
      obeyw plt2d comment (st1) (symx) (symy) (symsiz)
    end loop
  end if
end proc

