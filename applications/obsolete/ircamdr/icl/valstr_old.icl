{ Procedure VALSTR : creates valid filename string from input string
proc valstr st1 outst
  l1 = len(st1)
  st = 1
  st2 = st1
  more = 1
  pos = 0
  repeat = 0
  loop for j = 1 to l1
    st = substr(st2,j,1)
    if st = " "
      repeat = 1
      pos = j
    end if
    if st = "+"
      repeat = 1
      pos = j
    end if
    if st = """"
      repeat = 1
      pos = j
    end if
    if st = "''"
      repeat = 1
      pos = j
    end if
    if st = "("
      repeat = 1
      pos = j
    end if
    if st = ")"
      repeat = 1
      pos = j
    end if
    if st = "["
      repeat = 1
      pos = j
    end if
    if st = "]"
      repeat = 1
      pos = j
    end if
    if st = "!"
      repeat = 1
      pos = j
    end if
    if st = "@"
      repeat = 1
      pos = j
    end if
    if st = "%"
      repeat = 1
      pos = j
    end if
    if st = "^"
      repeat = 1
      pos = j
    end if
    if st = "&"
      repeat = 1
      pos = j
    end if
    if st = "\"
      repeat = 1
      pos = j
    end if
    if st = "/"
      repeat = 1
      pos = j
    end if
    if st = ":"
      repeat = 1
      pos = j
    end if
    if st = ";"
      repeat = 1
      pos = j
    end if
    if st = ">"
      repeat = 1
      pos = j
    end if
    if st = "<"
      repeat = 1
      pos = j
    end if
    if st = "|"
      repeat = 1
      pos = j
    end if
    if st = "~"
      repeat = 1
      pos = j
    end if
    if repeat = 1
      outst = "BAD"
      break
    else
      outst = st2
    end if
  end loop
  if outst = "BAD"
    pos2 = integer(pos-1)
    outst = substr(st2,1,pos2)
    print "Bad characters discovered in object string"
    print "Truncating to " (outst)
  end if
end proc
