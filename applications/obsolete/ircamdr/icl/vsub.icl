proc vsub
  asknum (j1) "Start obs no. ? "
  asknum (j2) "End   obs no. ? "
  loop for jj = j1 to j2
    im1 = "vG"&jj
    im2 = "sky"
    im3 = "d"&jj
    print "Subtracting images " (im1) " and " (im2) " - result to " (im3)
    obeyw rapi2d sub (im1) (im2) (im3)
  end loop
end proc
