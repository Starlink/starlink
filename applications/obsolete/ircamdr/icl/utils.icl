set nosave
set editor edt
set nocheckpars
defstring PR =

defstring PAU #exit
defstring PROCEDURE proc
defstring CREATESUB dcl
defstring SETPROMPT set prompt

{ PROCEDURE ASKCHAR
proc askchar var prompt
  prompt_string = (prompt)
  z0 = len(prompt)
  if z0 = 0
    print "No prompt given... programming error!"
    signal error
  end if
  z1 = index(prompt,'\')
  if z1 = 0
   default = " "
  else
    st1 = substr(prompt,z1+1,z0-z1+1)
    z2 = index(st1,'\')
    if z2 = 0
      default = " "
    else
      default = substr(st1,1,z2-1)
    end if
  end if
  input (prompt_string) (val)
  lval = len(val)
  if lval = 0
    val = char(39)&default&char(39)
  else
    val = char(39)&val&char(39)
  end if
  var = val
end proc

{ PROCEDURE ASKCHOICE
proc askchoice var prompt
  finished = false
  prompt_string = (prompt)
  z0 = len(prompt)
  if z0 = 0
    print "No prompt given... programming error!"
    signal error
  end if
  z1 = index(prompt,'\')
  if z1 = 0
   default = " "
  else
    st1 = substr(prompt,z1+1,z0-z1+1)
    z2 = index(st1,'\')
    if z2 = 0
      default = " "
    else
      default = substr(st1,1,z2-1)
    end if
  end if
  z1 = index(prompt,'(')
  if z1 = 0
    choices = "y,n"
  else
    st1 = substr(prompt,z1+1,z0-z1+1)
    z2 = index(st1,')')
    if z2 = 0
      choices = "y,n"
    else
      choices = substr(st1,1,z2-1)
    end if
  end if
  ext_choices = UPCASE(choices)&','
  loop while not finished
    input (prompt_string) (choice)
    if choice = " "
      choice = default
    end if
    choice = UPCASE(choice)
    length  = LEN (ext_choices)
    start = 1
    end_posn = INDEX (ext_choices,',')
    val = 1
    found = false
    loop while not found
      if choice = SUBSTR (ext_choices,start,end_posn-start)
        finished = true
        found = true
      else
        start = end_posn + 1
        if start > length
          found = true
        else
          end_posn = INDEX(SUBSTR(ext_choices,start,length-start + 1),',') + start - 1
          val = val + 1
        end if
      end if
    end loop
  end loop
  var = val
end proc

{ PROCEDURE ASKDEC
proc askdec var prompt
  prompt_string = (prompt)
  z0 = len(prompt)
  if z0 = 0
    print "No prompt given... programming error!"
    signal error
  end if
  z1 = index(prompt,'\')
  if z1 = 0
   default = " "
  else
    st1 = substr(prompt,z1+1,z0-z1+1)
    z2 = index(st1,'\')
    if z2 = 0
      default = " "
    else
      default = substr(st1,1,z2-1)
    end if
  end if
  input (prompt_string) (st1)
  if st1 = " "
    val = decl(default)*360.0/(2*3.1415926)
  else
    val = decl(st1)*360.0/(2*3.1415926)
  end if
  var = val
end proc

{ PROCEDURE ASKLOG
proc asklog var prompt
  prompt_string = (prompt)
  z0 = len(prompt)
  if z0 = 0
    print "No prompt given... programming error!"
    signal error
  end if
  z1 = index(prompt,'\')
  if z1 = 0
   default = " "
  else
    st1 = substr(prompt,z1+1,z0-z1+1)
    z2 = index(st1,'\')
    if z2 = 0
      default = " "
    else
      default = substr(st1,1,z2-1)
    end if
  end if
  finished = FALSE
  loop while NOT finished
    input (prompt_string) (var)
    if var = " "
      val = UPCASE(SUBSTR(default,1,1))
      if val = 'Y' or val = 'T'
        val = 1
      else if val = 'N' or val = 'F'
        val = 0
      else
        val = 0
      end if
      finished = TRUE
    else
      val = UPCASE(SUBSTR(var,1,1))
      if val = 'Y' or val = 'T'
        val = 1
        finished = TRUE
      else if val = 'N' or val = 'F'
        val = 0
        finished = TRUE
      else
        finished = FALSE
      end if
    end if
  end loop
  var = val
end proc

{ PROCEDURE ASKNAME
proc askname var prompt
  prompt_string = (prompt)
  z0 = len(prompt)
  if z0 = 0
    print "No prompt given... programming error!"
    signal error
  end if
  z1 = index(prompt,'\')
  if z1 = 0
   default = " "
  else
    st1 = substr(prompt,z1+1,z0-z1+1)
    z2 = index(st1,'\')
    if z2 = 0
      default = " "
    else
      default = substr(st1,1,z2-1)
    end if
  end if
  input (prompt_string) (val)
  lval = len(val)
  if lval = 0
    val = default
  end if
  var = val
end proc

{ PROCEDURE ASKNUM
proc asknum var prompt
  prompt_string = (prompt)
  z0 = len(prompt)
  if z0 = 0
    print "No prompt given... programming error!"
    signal error
  end if
  z1 = index(prompt,'\')
  if z1 = 0
   default = " "
  else
    st1 = substr(prompt,z1+1,z0-z1+1)
    z2 = index(st1,'\')
    if z2 = 0
      default = " "
    else
      default = substr(st1,1,z2-1)
    end if
  end if
  input (prompt_string) (var)
  if var = " "
    val = default
  else
    val = var
  end if
  var = val
end proc

{ PROCEDURE ASKRA
proc askra var prompt
  prompt_string = (prompt)
  z0 = len(prompt)
  if z0 = 0
    print "No prompt given... programming error!"
    signal error
  end if
  z1 = index(prompt,'\')
  if z1 = 0
   default = " "
  else
    st1 = substr(prompt,z1+1,z0-z1+1)
    z2 = index(st1,'\')
    if z2 = 0
      default = " "
    else
      default = substr(st1,1,z2-1)
    end if
  end if
  input (prompt_string) (st1)
  if st1 = " "
    val = ra(default)*360.0/(30*3.1415926)
  else
    val = ra(st1)*360.0/(30*3.1415926)
  end if
  var = val
end proc

{ PROCEDURE BELL
proc bell n delay
  if n = 1
    string = (char(7))
  else if n = 2
    string = (char(7))&(char(7))
  else if n = 3
    string = (char(7))&(char(7))&(char(7))
  else if n = 4
    string = (char(7))&(char(7))&(char(7))&(char(7))
  else if n = 5
    string = (char(7))&(char(7))&(char(7))&(char(7))&(char(7))
  else if n = 6
    string = (char(7))&(char(7))&(char(7))&(char(7))&(char(7))&(char(7))
  else
    string = (char(7))&(char(7))&(char(7))&(char(7))&(char(7))&(char(7))
  end if
  print "Ding!" (string)
end proc

{ PROCEDURE COMPARE
proc compare string1 string2 var
  upstring1 = UPCASE(string1)
  upstring2 = UPCASE(string2)
  if upstring1 = upstring2
    val = TRUE
  else
    val = FALSE
  end if
  var = val
end proc

{ PROCEDURE WAITCR
proc waitcr
  finished = false
  var = " "
  count = 0
  loop while not finished
    input 'Waiting...' (var)
    if var <> " "
      count = count + 1
      if count = 5
        finished = true
      end if
    else
      finished = true
    end if
  end loop
end proc

{ PROCEDURE CONCAT
proc concat string1 string2 string3
  upstring1 = UPCASE(string1)
  upstring2 = UPCASE(string2)
  val = (string1)&(string2)
  string3 = val
end proc

{ PROCEDURE RETURN
proc return
  signal RETURN_TO_ICL
end proc

{ PROCEDURE TOCHAR
proc tochar var string1
  val = var
  string1 = val
end proc

