{ PROCEDURE DAOCEN : takes cursor input and centroids at that position
proc daocen
  print "Cursors on current image and does CENTROID at position chosen"
  loop for brave_man = 1 to 1000
    print "Place the cursor on the source"
    obeyw plt2d cursor
    get plt2d x_cur_pixel (value1)
    get plt2d y_cur_pixel (value2)
    get plt2d name_image (name_image)
    obeyw rapi2d CENTROID (name_image) (value1) (value2) n ~
      11 9 y 7 0.05
    asklog (ans) "Continue selection in current image \Y\ ? "
    if ans=0
      break
    end if
  end loop
end proc

