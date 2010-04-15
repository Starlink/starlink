proc vdel
  asknum (j1) "Start obs no. ? "
  asknum (j2) "Enf   obs no. ? "
  loop for jj = j1 to j2
    im1 = "vG"&jj&".sdf"
    print "Deleting images " (im1)
    delfile (im1)
  end loop
end proc
