{ PROCEDURE TICKIO : puts contour ticks inside box or outside box
proc tickio io
  yn = undefined(io)
  if yn
    askchoice (wio) "Ticks inside or outside box (I,O) \I\ ? "
    if wio = 1
      io2 = "I"
    else
      io2 = "O"
    end if
  else
    io2 = io
  end if
  send plt2d set tick_inout (io2)
end proc
