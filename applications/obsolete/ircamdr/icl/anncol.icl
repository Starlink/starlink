{ PROCEDURE ANNCOL : sets colour of annotation in surround
proc anncol st1
  yn = undefined(st1)
  if yn
    print "Give colour code for annotation : "
    askchar (st2) "Colour Code \R\ : "
  else
    st2 = "'"&st1&"'"
  end if
  st2 = upcase(st2)
  print (st2)
  send plt2d set annotate_colour (st2)
  send plt2d set border_colour (st2)
end proc
