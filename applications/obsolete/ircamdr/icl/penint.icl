{ PROCEDURE PENINT : sets the intensity of the 3 guns for a pen
proc penint value1 value2 value3 value4
  get plt2d image_workstn (image_workstn)
  if image_workstn = 1
    yn1 = undefined(value1)
    yn2 = undefined(value2)
    yn3 = undefined(value3)
    yn4 = undefined(value4)
    if yn1
      print "Give PEN NUMBER (0-255) ? "
      asknum (value5) "Pen Number \0\      : "
    else
      value5 = value1
    end if
    if yn2 or yn3 or yn4
      print "Give RGB GUN INTENSITIES (0.0 - 1.0) ? "
      asknum (value6) "Red Intensity   \1\ : "
      asknum (value7) "Green Intensity \0\ : "
      asknum (value8) "Blue Intensity  \0\ : "
    else
      value6 = value2
      value7 = value3
      value8 = value4
    end if
    send plt2d set gun_spec 'INTENSITY'
    send plt2d set gun_1 (value6)
    send plt2d set gun_2 (value7)
    send plt2d set gun_3 (value8)
    obeyw plt2d setcol (value5)
  else
    print "You CANNOT set COLOURS on this workstation"
  end if
end proc

