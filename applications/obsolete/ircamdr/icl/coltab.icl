{ PROCEDURE COLTAB : writes a colour table
proc coltab st1
  yn = undefined(st1)
  get plt2d image_workstn (image_workstn)
  if image_workstn = 1
    if yn
      print "Give the name of the COLOUR TABLE to be displayed ? "
      askname (st2) "Colour Table Image \GREY\ : "
    else
      st2 = st1
    end if
    slen = len(st2)
    st2s = st2&".sdf"
    ct_exists = file_exists(st2s)
    if ct_exists
      st3 = st2
    else
      st3 = "$LIRCAMDIR/"&st2
      st3s = "$LIRCAMDIR/"&st2&".sdf"
      ct_exists = file_exists(st3s)
      if ct_exists
      else
        print "Cannot find colour table " (st3)
        return
      end if
    end if
    get plt2d worknam (worknam)
    send plt2d set last_lut (st3)
    obeyw plt2d coltab (st3)
    print "Writing colour table " (st3) " to " (worknam)
  else
    print "You CANNOT plot COLOURS on " (worknam)
  end if
end proc
