proc skysub
  asknum (j1) "Start obs no. ? "
  asknum (j2) "End   obs no. ? "
  loop for jj = j1 to j2
    im1 = "ro951203_9"
    im2 = "ro951203_"&jj
    im3 = "skysub"&jj
    print "Subtracting images " (im1) " and " (im2) " - result to " (im3)
    obeyw rapi2d sub (im1) (im2) (im3)
  end loop
end proc
