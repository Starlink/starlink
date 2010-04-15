proc skyext
  asknum (j1) "Start obs no. ? "
  asknum (j2) "End   obs no. ? "
  delfile skysub.txt
  create zz skysub.txt
  loop for jj = j1 to j2
    im1 = "skysub"&jj
    print "Extracting median from image  " (im1)
    obeyw rapi2d histo (im1) \
    getpar glob histo_median (medval)
    print "  Median = " (medval)
    c1 = jj:10:1
    c2 = medval:10:2
    dline = c1&"    "&c2
    write zz (dline)
  end loop
  close zz
end proc
