{ Procedure VALSTR : creates valid filename string from input string
proc valstr st1 outst
  yn1 = undefined(st1)
  if yn1
    outst = ""
  else
    more = 1
    st2 = st1
    loop while more = 1
      l1 = len(st2)
      st = 1
      pos = 0
      repeat = 0
      loop for j = 1 to l1
        st = substr(st2,j,1)
        if st = " " or st = "+" or st = """" or st = "''" or st = "("
          repeat = 1
          pos = j
        end if
        if st = ")" or st = "[" or st = "]" or st = "!" or st = "@" or st = "%"
          repeat = 1
          pos = j
        end if
        if st = "^" or st = "&" or st = "\" or st = "/" or st = ":" or st = ";"
          repeat = 1
          pos = j
        end if
        if st = ">" or st = "<" or st = "|" or st = "~" or st = "$"
          repeat = 1
          pos = j
        end if
        if repeat = 1
          break
        end if
      end loop
      if pos <> 0
        pos2 = integer(pos-1)
        if pos2 < l1
          st3 = substr(st2,1,pos2)
        else
          st3 = ""
        end if
        pos3 = integer(pos+1)
        if pos3 < l1
          st4 = substr(st2,pos3,(l1-pos3+1))
        else
          st4 = ""
        end if
        st2 = st3&st4
      else
        more = 0
      end if
    end loop
    outst = st2
    obeyw obsrap lowcase (st2)
    getpar glob lowcase_string (outst)
    print "String processed from """(st1)""" to """(outst)""""
  end if
end proc
