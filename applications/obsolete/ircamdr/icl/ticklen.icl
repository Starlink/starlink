{ PROCEDURE TICKLEN : puts either short or full ticks on box
proc ticklen io
  yn = undefined(io)
  if yn
    askchoice (wio) "SHORT or FULL ticks (S,F) \S\ ? "
    if wio = 1
      io2 = "SMALL"
    else
      io2 = "FULL"
    end if
  else
    io2 = io
    io2 = upcase(io2)
    io2 = substr(io2,1,1)
    if io2 = "S"
      io2 = "SMALL"
    else if io2 = "F"
      io2 = "FULL"
    else
      io2 = "SMALL"
    end if
  end if
  send plt2d set contour_ticklen (io2)
end proc
