{ PROCEDURE TESTVAL2 : tests if 2 input paramters defined
proc testval2 v1 v2
  yn1 = undefined(v1)
  yn2 = undefined(v2)
  if yn1
    v1 = -999
  end if
  if yn2
    v2 = -999
  end if
end proc

{ PROCEDURE TESTVAL4 : tests if 4 input paramters defined
proc testval4 v1 v2 v3 v4
  yn1 = undefined(v1)
  yn2 = undefined(v2)
  yn3 = undefined(v3)
  yn4 = undefined(v4)
  if yn1
    v1 = -999
  end if
  if yn2
    v2 = -999
  end if
  if yn3
    v3 = -999
  end if
  if yn4
    v4 = -999
  end if
end proc

