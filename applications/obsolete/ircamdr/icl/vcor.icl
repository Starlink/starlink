proc vcor
  asknum (j1) "Start obs no. ? "
  asknum (j2) "Enf   obs no. ? "
  loop for jj = j1 to j2
    im1 = "d"&jj
    im2 = "c"&jj
    print "Colmed-ing images " (im1)
    obeyw obsrap colmed (im1) cmed n
    print "Ygrow-ing result..."
    obeyw obsrap ygrow cmed 64 ygro
    print "Subtracting correction image from " (im1) " result to " (im2)
    obeyw rapi2d sub (im1) ygro (im2)
    delfile cmed.sdf
    delfile ygro.sdf
  end loop
end proc
