{ PROCEDURE SETCURMARK : sets the cursor marking parameter from users choice
proc setcurmark value1
  yn = undefined(value1)
  if yn
    print "Do you want to ENABLE or DISABLE the cursor cross ?"
    askchoice (curcross) "Cursor Cross (ENABLE,DISABLE,E,D) \DISABLE\ : "
  else
    curcross = value1
  end if
  if curcross = 1 or curcross = 3
    send plt2d set CURSOR_CROSS 'YES'
    print "O.K. Cursor cross has been ENABLED ..."
  else
    send plt2d set CURSOR_CROSS 'NO'
    print "O.K. Cursor cross has been DISABLED ..."
  end if
end proc

