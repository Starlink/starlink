{ PROCEDURE ABLOCK : plots a colour block
proc ablock value1 value2 value3
  get plt2d image_workstn (image_workstn)
  if image_workstn = 1
    yn1 = undefined(value1)
    yn2 = undefined(value2)
    yn3 = undefined(value3)
    if yn1 or yn2 or yn3
      print "Give the colour block X,Y CENTRE in screen coordinates : "
      asknum (value4) "Colour Block X Centre \384\ : "
      asknum (value5) "Colour Block Y Centre \490\ : "
      abxc = value4
      abyc = value5
      print "Give the SCALE of the colour table (1-10) ? "
      asknum (value6) "Colour Block Scale \2\ : "
    else
      value4 = value1
      value5 = value2
      value6 = value3
    end if
    print "Give the DIRECTION for the colour table ? "
    askchoice (realdir) "Direction (VERTICAL,HORIZONTAL,V,H) \VERTICAL\ : "
    if realdir = 1 or realdir = 3
      realdir = 1
      st1 = 'V'
    else
      realdir = 2
      st1 = 'H'
    end if
    print "Do you want ANNOTATION around the colour table ? "
    asklog (ctann) "Annotation (Yes or No) \Y\ : "
    if ctann = 0
      send plt2d set ct_annotation 'NO_ANNOTATION'
    else
      send plt2d set ct_annotation 'ANNOTATE'
    end if
    if ctann = 1 and realdir = 1
      print "Do you want the NUMBERS at the RIGHT or LEFT of the block ?"
      askchoice (value2) "Number Position (RIGHT,LEFT) \LEFT\ : "
      if value2 = 1
        send plt2d set blocknum_where 'R'
      else
        send plt2d set blocknum_where 'L'
      end if
    end if
    if ctann = 1 and realdir = 2
      print "Do you want the NUMBERS at the TOP or BOTTOM of the block ?"
      askchoice (value2) "Number Position (TOP,BOTTOM) \BOTTOM\ : "
      if value2 = 1
        send plt2d set blocknum_where 'T'
      else
        send plt2d set blocknum_where 'B'
      end if
    end if
    obeyw plt2d block (abxc) (abyc) (value6) (st1)
  else
    print "You CANNOT plot IMAGES/COLOURS on this workstation"
  end if
end proc

