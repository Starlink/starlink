proc satsub
  loop for jj = 176 to 1259
    im1 = "satG"&jj
    im2 = "satG175"
    im3 = "diff"&jj
    print "Subtracting images " (im1) " and " (im2) " - result to " (im3)
    obeyw rapi2d sub (im1) (im2) (im3)
  end loop
end proc
