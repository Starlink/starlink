{ PROCEDURE SETPRE : to set correct file prefix for observation files
proc setpre st1
  get plt2d filetype (filetype)
  if filetype <> 1
    yn = undefined(st1)
    if yn
      print "Enter the FULL name of the HDS file you want to analyse "
      print "from the above list in the form IRCAM_01APR86_1"
      value1 = 1
      loop while value1 = 1
        askname (old_name) "Old HDS File : "
        compare (old_name) " " (value1)
      end loop
    else
      old_name = st1
    end if
    send plt2d set contname (old_name)
    send plt2d set container (old_name)
    concat (old_name) ".obs(" (name_prefix)
    send plt2d set name_prefix (name_prefix)
  else
    yn = undefined(st1)
    if yn
      print "Enter PREFIX for IRCAM files (e.g. RO940401_) : "
      askname (old_name) "File Prefix ? "
    else
      old_name = st1
    end if
    send plt2d set contname (old_name)
    send plt2d set container (old_name)
    name_prefix = old_name
    send plt2d set name_prefix (name_prefix)
  end if
  print "Image prefix defined as " (name_prefix)
end proc
