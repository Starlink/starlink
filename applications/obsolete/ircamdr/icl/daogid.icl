{ PROCEDURE DAOGID : gets ID of nearest star to cursor selected one
proc daogid
  print "Procedure to inspect DAOPHOT find command output file for ID of "
  print "cursor selected star"
  print "Give name of daophot find command output file : "
  askname (adao) "Daophot find output file ? "
  get plt2d name_image (name_image)
  print "Current image displayed = " (name_image)
  loop for brave_man = 1 to 1000
    print "Place cursor on the source whose DAO ID is required"
    send plt2d set cursor_cross 'YES'
    obeyw plt2d cursor
    get plt2d x_cur_pixel (value1)
    get plt2d y_cur_pixel (value2)
    fclose_b
    delfile daojunk.dat
    create bfile "daojunk.dat"
    write bfile (adao)
    write bfile (value1)
    write bfile (value2)
    fclose_b
    ! $LIRCAMDIR/daogid
    asklog (ans) "Continue selection in current image \Y\ ? "
    if ans = 0
      break
    end if
  end loop
  delfile daojunk.dat
end proc
