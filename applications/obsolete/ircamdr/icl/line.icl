{ PROCEDURE LINE : plots a line between user specified positions
proc line value1 value2 value3 value4
  yn1 = undefined(value1)
  yn2 = undefined(value2)
  yn3 = undefined(value3)
  yn4 = undefined(value4)
  if yn1 or yn2 or yn3 or yn4
    print "Give the LINE X,Y start and end coordinates (in pixels) ? "
    asknum (value5) "Line X Start Pixel \1\ : "
    asknum (value6) "Line Y Start Pixel \1\ : "
    asknum (value7) "Line X End Pixel \256\ : "
    asknum (value8) "Line Y End Pixel \256\ : "
  else
    value5 = value1
    value6 = value2
    value7 = value3
    value8 = value4
  end if
  send plt2d set cursor_cross 'NO'
  obeyw plt2d line (value5) (value6) (value7) (value8)
end proc

