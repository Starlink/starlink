{ PROCEDURE SETFONT : changes the font and character orientation
proc setfont value1
  yn = undefined(value1)
  if yn
    print "Enter the GKS code for the FONT required "
    asknum (value2) "Fonts \1\ : "
  else
    value2 = value1
  end if
  send plt2d set comment_font (value2)
end proc

