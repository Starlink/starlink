{ PROCEDURE LINECOL : sets all line graphics pens to specified color
proc linecol st1
  yn = undefined(st1)
  if yn
    askname (st2) "Colour for line graphics : "
  else
    st2 = st1
  end if
  get plt2d lpenmin (value1)
  get plt2d lpenmax (value2)
  value2 = value2-1
  print "Line graphics pens go from " (value1) " to " (value2)
  loop for brave_man = (value1) to (value2)
    obeyw plt2d setcol (brave_man) (st2)
  end loop
end proc

