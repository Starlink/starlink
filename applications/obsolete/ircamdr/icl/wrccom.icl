{ PROCEDURE WRCCOM : writes a comment at cursor position
proc wrccom
  get plt2d cursor_workstn (cursor_workstn)
  if cursor_workstn = 1
    morestuff = 1
    loop while morestuff = 1
      print "Give the COMMENT to be displayed, it''s SIZE in pixels, and "
      print "the NUMBER of comments written :"
      askchar (comstring) "Comment \IRCAM\ : "
      asknum (comsize) "Comment Size \15\ : "
      asknum (numcomm) "Number of Comments \1\ : "
      print "Change COMMENT ORIENTATION and/or FONT ?"
      asklog (brave_man) "Change Orientation,Font (Yes or No) \N\ : "
      if brave_man = 1
        SETCOMORI
        SETFONT
      end if
      print "Define the POSITION of your COMMENT with the CURSOR ..."
      send plt2d set cursor_cross 'NO'
      loop for brave_man = 1 to numcomm
        print "Comment number " (brave_man)
        obeyw plt2d curcom (comstring) (comsize)
      end loop
      asklog (morestuff) "Another WRCC (Yes or No) \N\ : "
    end loop
  else
    print "You CANNOT use a CURSOR on this workstation"
    print "Try routine procedure WRC instead ..."
  end if
end proc

