{ PROCEDURE CABLOCK : plots a colour block at cursor position
proc cablock value1
  get plt2d image_workstn (image_workstn)
  get plt2d cursor_workstn (cursor_workstn)
  if image_workstn = 1 and cursor_workstn = 1
    yn = undefined(value1)
    if yn
      print "Give the SCALE of the colour table (1-2) ? "
      asknum (bscal) "Colour Block Scale \1\ : "
    else
      bscal = value1
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
      askchoice (value2) "Number Position (RIGHT,LEFT,R,L) \LEFT\ : "
      if value2 = 1 or value2 = 3
        send plt2d set blocknum_where 'R'
      else
        send plt2d set blocknum_where 'L'
      end if
    end if
    if ctann = 1 and realdir = 2
      print "Do you want the NUMBERS at the TOP or BOTTOM of the block ?"
      askchoice (value2) "Number Position (TOP,BOTTOM,T,B) \BOTTOM\ : "
      if value2 = 1 or value2 = 3
        send plt2d set blocknum_where 'T'
      else
        send plt2d set blocknum_where 'B'
      end if
    end if
    print "Select the CENTRE for the COLOUR TABLE with the CURSOR ..."
    obeyw plt2d curblock (bscal) (st1)
  else
    print "You either CANNOT plot IMAGES/COLOURS on this workstation"
    print "    or you CANNOT use the cursor on it ..."
  end if
end proc

