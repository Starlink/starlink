{ PROCEDURE DAOGID2 : gets ID of nearest star to cursor selected one
proc daogid2
  print "Procedure to inspect two DAOPHOT find output files for ID of "
  print "cursor selected stars"
  get plt2d name_image (name_image)
  print "Current image displayed = " (name_image)
  print "Give name of 1st daophot find output file ""
  askname (firstcoo) "1st find file ? "
  print "Give name of 2nd daophot find output file ""
  askname (secondcoo) "2nd find file ? "
  loop for brave_man = 1 to 1000
    print "Place cursor on the source whose DAO ID is required"
    send plt2d set cursor_cross 'YES'
    obeyw plt2d cursor
    get plt2d x_cur_pixel (value1)
    get plt2d y_cur_pixel (value2)
    fclose_b
    delfile daojunk2.dat
    create bfile "daojunk2.dat"
    write bfile (firstcoo)
    write bfile (secondcoo)
    write bfile (value1)
    write bfile (value2)
    fclose_b
    ! $LIRCAMDIR/daogid2
    asklog (ans) "Continue selection in current image \Y\ ? "
    if ans = 0
      break
    end if
  end loop
  delfile daojunk2.dat
end proc
