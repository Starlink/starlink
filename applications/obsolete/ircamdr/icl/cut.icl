{ PROCEDURE CUT : plots a cut on current image
proc cut num_obsele code_image value1 value2 value3 value4
  get plt2d cut_flag (cut_flag)
  if cut_flag <> 1
    print "You MUST run SETCUT before you can plot a CUT/SLICE ..."
    return
  end if
  yn = undefined(num_obsele)
  yn2 = undefined(code_image)
  if yn
    num_obsele2 = -999
    code_image2 = -999
  else
    num_obsele2 = num_obsele
    if yn2
      code_image2 = -999
    else
      code_image2 = code_image
    end if
  end if
  get plt2d name_image (last_image)
  get_imagename (num_obsele2) (code_image2) (name_out) (last_image)
  name_image = name_out
  yn1 = undefined(value1)
  yn2 = undefined(value2)
  yn3 = undefined(value3)
  yn4 = undefined(value4)
  if yn1 or yn2 or yn3 or yn4
    print "Give the CUT X,Y Start and End (in pixels) ? "
    asknum (value5) "X Start Pixel \1\ : "
    asknum (value6) "Y Start Pixel \1\ : "
    asknum (value7) "X End Pixel \256\ : "
    asknum (value8) "Y End Pixel \256\ : "
  else
    value5 = value1
    value6 = value2
    value7 = value3
    value8 = value4
  end if
  get plt2d worknum (worknum)
  get plt2d worknam (worknam)
  if worknum = 3 or worknum = 4
    obeyw plt2d clear
  else if worknum = 3 or worknum = 4
    get plt2d tekline (tekline)
    compare (tekline) "LOCAL" (comval)
    if comval = 1
      obeyw plt2d cut (name_image) (value5) (value6) (value7) (value8)
      waitcr
    else
      print "CUT " (name_image) " on " (worknam)
      obeyw plt2d cut (name_image) (value5) (value6) (value7) (value8)
    end if
  else
    print "CUT " (name_image) " on " (worknam)
    get plt2d cut_positioning (cuttyp)
    compare (cuttyp) "CURSOR" (brave_man)
    if brave_man = 1
      obeyw plt2d cut (name_image) (value5) (value6) (value7) (value8)
    else
      obeyw plt2d cut (name_image) (value5) (value6) (value7) (value8)
    end if
  end if
  last_line = 1
  send plt2d set last_line (last_line)
end proc

