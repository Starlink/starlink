proc logo
  get plt2d worknum (worknum)
  if worknum <> 0
    get plt2d workxcen (workxcen)
    get plt2d workycen (workycen)
    send plt2d set magnification 0
    send plt2d set disp_mag 0
    obeyw plt2d clear
    xpos = workxcen
    ypos = workycen*1.2
    obeyw plt2d comment "Portable Ircamdr" (xpos) (ypos) 20
    xpos = workxcen
    ypos = workycen
    obeyw plt2d comment "(ircam_clred)" (xpos) (ypos) 15
    xpos = workxcen
    ypos = workycen*0.8
    obeyw plt2d comment "Version 1.0-0" (xpos) (ypos) 10
  end if
end proc
